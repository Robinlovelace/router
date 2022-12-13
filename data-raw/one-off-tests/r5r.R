
library(tidyverse)
library(r5r)
r5r_core = r5r::setup_r5(data_path = "~/github/modeshift/data_r5/")
library(od)
odsf = od_to_sf(od_data_df[1:2, ], od_data_zones)
od::od_coordinates(odsf)
odroutes = route(odsf)

origins <- lwgeom::st_startpoint(odsf) %>% sf::st_sf()
origins$id = odsf[[1]]
destinations <- lwgeom::st_endpoint(odsf) %>% sf::st_sf()
destinations$id = odsf[[2]]
mode <- c("BICYCLE")

# calculate detailed itineraries
s = system.time({
  det <- detailed_itineraries(
    max_bike_time = 10000,
    max_lts = 2,
    r5r_core = r5r_core,
    origins = origins,
    destinations = destinations,
    mode = mode,
    shortest_path = FALSE,
    verbose = FALSE, progress = TRUE
  )
})

mapview::mapview(odsf) +
  mapview::mapview(det)
od_r5 = route(odsf, detailed_itineraries, r5r_core = r5r_core, mode = mode,
              shortest_path = FALSE, verbose = FALSE, progress = TRUE)
  


od_leeds_jittered = readRDS("~/github/modeshift/od_leeds_jittered_disag_30.Rds")
od_leeds_jittered = od_leeds_jittered %>% 
  mutate(pad = stringr::str_pad(seq(n()), width = 10, side = "left", pad = 0)) %>% 
  mutate(LSOA11 = paste(LSOA11, pad)) %>% 
  mutate(laestab = paste(laestab, pad)) %>% 
  select(-pad)

mode = "WALK"
routes_walking = route(od_leeds_jittered[1:15, ], route_fun = detailed_itineraries,
                               max_trip_duration = 10, r5r_core = r5r_core,
                               mode = mode, shortest_path = TRUE, 
                               verbose = FALSE, progress = TRUE)
summary(duplicated(routes_walking$id))
