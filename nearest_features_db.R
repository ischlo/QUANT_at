# THIS SCRIPT IS OUTDATED BECAUSE I WAS USING SQLITE AND THEN SWITHCHED AND THEM DELETED THIS DATA 
# BASE TO MAKE A NEW BETTER ONE, 
# UPDATE WHEN EVERYTHING IS SET UP, THE POSTGRESQL DBS

library(sf)
library(data.table)
library(rlist)
library(dplyr)
library(readr)
library(tmap)
library(foreach)
library(DBI)
library(RPostgreSQL)

# road_nodes_london <- read_csv("data/nodes_database/road_nodes_london_wkt.csv"
#                               ,col_types = cols(osmid = col_character()))
# 
# road_nodes_london <- road_nodes_london |> st_as_sf(coords = c("x","y"),crs = 4326)
#

# uncomment the next steps if running the script indepentdently from test_city
# london_centroids <- readr::read_csv("/Users/ischlo/Documents/CASA/data/london_msoa.csv") #|> as.data.table()
# 
# colnames(london_centroids)

# the postgis way with postgresql

get_knn <- function(geom_wkt, k = 3){
  paste0("SELECT osmid, geom <-> ST_Transform(ST_GeomFromText('",geom_wkt,"',4326),27700) AS dist FROM nodes_proj ORDER BY dist LIMIT ",k,";")
}

conn <- dbConnect(PostgreSQL()
                  ,host = "localhost"
                  ,dbname = "nodes_pgsql")

# test query to the db
# spatial_query <- get_knn("POINT ( -0.17 51.0)", 1)
#   
# res <- DBI::dbSendQuery(conn, spatial_query) |> DBI::dbFetch()
# benchmarks showed a 1e5 performance gain from using indexed postgresql tables

#### Serializing ####
#  ultimately, run this once for all the GB centroids and save to use for graph routing. 
#  try in parralel ? Can the server side handle that without modifications ? apparently yes for postgres

nn_ <- foreach(geom = area_selection$centroid |> st_as_text() # if not with test_city: london_centroids$centroid
              ,.combine = rbind) %do% {
  DBI::dbGetQuery(conn, get_knn(geom,k = 1))
}

nn <- nn_[,1]

