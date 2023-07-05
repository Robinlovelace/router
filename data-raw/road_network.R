## code to prepare `road_network` dataset goes here

library(od)
library(osmdata)
library(tidyverse)
library(tidygraph)
library(sfnetworks)
library(sf)

bb = sf::st_bbox(od::od_data_zones_small)
bb
osm_data = osmdata::opq(bb) |>
  osmdata::add_osm_feature(key = "highway") |>
  osmdata::osmdata_sf()
road_network = osm_data$osm_lines
nrow(road_network) # 18k rows, we want only ~1k
# Get network centrality
road_network_sfn = as_sfnetwork(road_network)
net_centrality = road_network_sfn %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(bc = centrality_edge_betweenness(weights = weight, directed = FALSE))
net_centrality
# Convert back to sf object: 
road_network_centrality = net_centrality %>%
  activate("edges") %>%
  st_as_sf()
names(road_network_centrality)

road_network = road_network_centrality %>%
  select(osm_id, highway, cycleway, oneway, maxspeed, bc) %>%
  slice_max(bc, n = 1000) 

# Keep only road network elements in the largets connected component:
road_network_sfn = as_sfnetwork(road_network)
road_network = road_network_sfn %>%
  mutate(connected_component = igraph::components(road_network_sfn)$membership) %>%
  filter(connected_component == 1) %>%
  select(-connected_component) |>
  activate("edges") %>%
  st_as_sf()

plot(road_network)
nrow(road_network)
usethis::use_data(road_network, overwrite = TRUE)
