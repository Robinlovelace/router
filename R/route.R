#' Plan routes on the transport network
#'
#' Takes origins and destinations, finds the optimal routes between them
#' and returns the result as a spatial (sf or sp) object.
#' The definition of optimal depends on the routing function used
#'
#' @param desire_lines A spatial (linestring) object
#' @param route_fun A routing function to be used for converting the lines to routes
#' @param n_print A number specifying how frequently progress updates
#' should be shown
#' @param list_output If FALSE (default) assumes spatial (linestring) object output.
#' Set to TRUE to save output as a list.
#' @param ... Arguments passed to the routing function
#' @param cl Cluster
#' @param wait How long to wait between routes?
#'   0 seconds by default, can be useful when sending requests to rate limited APIs.
#' @param batch Route in batch mode? `FALSE` by default.
#' @family routes
#' @export
#' @examples
#' library(od)
#' odsf = od_to_sf(od_data_df[1:2, ], od_data_zones)
#' od::od_coordinates(odsf)
#' odroutes = route(odsf)
#' plot(odroutes)
route = function(desire_lines = NULL, route_fun = cyclestreets::journey, wait = 0,
                  n_print = 10, list_output = FALSE, cl = NULL, batch = FALSE, ...) {
  FUN = match.fun(route_fun)
  
  browser()
  # generate od coordinates
  ldf = od::od_coordinates(desire_lines)
  
  # Check for batch mode
  if(requireNamespace("opentripplanner", quietly = TRUE) && !batch) {
    batch = identical(FUN, opentripplanner::otp_plan)
  }
  if(requireNamespace("r5r", quietly = TRUE) && !batch) {
    batch = identical(FUN, r5r::detailed_itineraries)
  }
  
  if (batch) {
      message("Routing in batch mode")
      f_mat = ldf[, 1:2]
      t_mat = ldf[, 3:4]
      if(identical(FUN, opentripplanner::otp_plan)) {
        routes_out = FUN(
          fromPlace = f_mat,
          toPlace = t_mat,
          ...
        )
      }
      if(identical(FUN, r5r::detailed_itineraries)) {
        f_df = data.frame(id = desire_lines[[1]], lon = f_mat[, 1], lat = f_mat[, 2])
        t_df = data.frame(id = desire_lines[[2]], lon = t_mat[, 1], lat = t_mat[, 2])
        routes_out = FUN(
          origins = f_df,
          destinations = t_df,
          ...
        )
      }
      
      nrow_diff = 1 - (nrow(routes_out) / nrow(desire_lines))
      if(nrow_diff == 0) {
        message("Routes calculated for every desire line")
        message("Binding new columns to the data")
        routes_out = sf::st_sf(
          cbind(
            sf::st_drop_geometry(desire_lines),
            sf::st_drop_geometry(routes_out)
          ),
          geometry = routes_out$geometry
        )
      }
      if(nrow_diff > 0) {
        message(round(nrow_diff * 100, digits = 2), "% routes missing")
        d_df = sf::st_drop_geometry(desire_lines)
        d_df$id = paste(d_df[[1]], d_df[[2]])
        routes_out$id = paste(routes_out[["fromId"]], routes_out[["toId"]])
        routes_out = dplyr::right_join(d_df, routes_out, by = "id")
      }

      return(routes_out)
  }

  # Check the CRS before trying to do routing:
  # https://github.com/ropensci/stplanr/issues/474
  if(!sf::st_is_longlat(desire_lines)) {
    warning("CRS of line object is not geographic (in degrees lon/lat)")
    message("It has the following CRS: ", sf::st_crs(desire_lines))
    message("See ?st_transform() to transform its CRS, e.g. to EPSG 4326")
  }
  if (list_output) {
    if (is.null(cl)) {
      list_out = pbapply::pblapply(1:nrow(desire_lines), function(i) route_l(FUN, ldf, i, desire_lines, ...))
    } else {
      list_out = pbapply::pblapply(1:nrow(desire_lines), function(i) route_l(FUN, ldf, i, desire_lines, ...), cl = cl)
    }
  } else {
    if (is.null(cl)) {
      list_out = pbapply::pblapply(1:nrow(desire_lines), function(i) route_i(FUN, ldf, wait, i, desire_lines, ...))
    } else {
      list_out = pbapply::pblapply(1:nrow(desire_lines), function(i) route_i(FUN, ldf, wait, i, desire_lines, ...), cl = cl)
    }
  }

  list_elements_sf = most_common_class_of_list(list_out, "sf")
  if (sum(list_elements_sf) < length(list_out)) {
    failing_routes = which(!list_elements_sf)
    message("These routes failed: ", paste0(failing_routes, collapse = ", "))
    message("The first of which was:")
    print(list_out[[failing_routes[1]]])
  }
  if (list_output | !any(list_elements_sf)) {
    message("Returning list")
    return(list_out)
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    # warning("data.table used to create the sf object, bounding box may be incorrect.")
    out_dt = data.table::rbindlist(list_out[list_elements_sf])
    attribute_names = !names(out_dt) %in% "geometry"

    out_dtsf = sf::st_sf(out_dt[attribute_names], geometry = out_dt$geometry)
    # attributes(out_dtsf$geometry)
    # identical(sf::st_bbox(out_dtsf), sf::st_bbox(out_sf)) # FALSE
    attr(out_dtsf$geometry, "bbox") = sfheaders::sf_bbox(out_dtsf)
    # identical(sf::st_bbox(out_dtsf), sf::st_bbox(out_sf)) # TRUE
    return(out_dtsf)
  } else {
    out_sf = do.call(rbind, list_out[list_elements_sf])
    out_sf
  }
}

# output sf objects
route_i = function(FUN, ldf, wait, i, desire_lines, ...) {
  Sys.sleep(wait)
  error_fun = function(e) {
    e
  }
  tryCatch(
    {
      single_route = FUN(ldf[i, 1:2], ldf[i, 3:4], ...)
      sf::st_sf(cbind(
        sf::st_drop_geometry(desire_lines[rep(i, nrow(single_route)), ]),
        route_number = i,
        sf::st_drop_geometry(single_route)
      ),
      geometry = single_route$geometry
      )
    },
    error = error_fun
  )
}

# output whatever the routing function returns
route_l = function(FUN, ldf, i, desire_lines, ...) {
  error_fun = function(e) {
    e
  }
  tryCatch(
    {
      single_route = FUN(ldf[i, 1:2], ldf[i, 3:4], ...)
    },
    error = error_fun
  )
}

most_common_class_of_list = function(desire_lines, class_to_find = "sf") {
  class_out = sapply(desire_lines, function(x) class(x)[1])
  most_common_class = names(sort(table(class_out), decreasing = TRUE)[1])
  message("Most common output is ", most_common_class)
  is_class = class_out == class_to_find
  is_class
}
