library(sf)
library(dbplyr)
library(DBI)
library(data.table)
library(readr)
library(dplyr)
library(tidyr)
library(RPostgreSQL)
library(cppRouting)
library(Btoolkit)
library(cppSim)

#####

gb_graph <- rlist::list.load("/Users/ivann/Documents/CASA/Benchmarks/modified_cppr_ networks/osm_cppr_modified.rds")

london_edges <- st_read("/Users/ivann/Documents/CASA/RC_outputs/london_all/london_all.gpkg"
                        ,layer = "edges")

####

db_name <- "osm_gb"

conn_db <-
  DBI::dbConnect(RPostgreSQL::PostgreSQL()
                 ,host = "localhost"
                 ,dbname = db_name)

#####

study_area <- function(limits
                       ,conn = conn_db) {
  require(leaflet)
  require(leafgl)
  # uses the leafGL library to render a high number of points allowing to load
  # a greater observation windiw and debugging faster

  poly_1 <- Btoolkit::make_poly(limits = limits) |> sf::st_as_text()

  nodes_query_1 <- paste0("SELECT id, ST_X(geom) AS x,"
                          ,"ST_Y(geom) AS Y "
                          ,"FROM nodes WHERE "
                          ,"ST_Contains(ST_GeomFromText('"
                          ,poly_1
                          ,"',4326), geom);")

  area_1 <- DBI::dbGetQuery(conn = conn_db,nodes_query_1) |>
    sf::st_as_sf(coords = c(2,3),crs = 4326)

  leaflet() %>%
    addProviderTiles(provider = "OpenStreetMap")  |>
    addGlPoints(data = area_1
                ,fillOpacity = 1
                ,fillColor = "black"
                  ,radius = 9
                ,popup = area_1$id)


}


#####

limits_1 <- c(-0.1384,51.5213,-0.1343,51.5233)
study_area(limits_1)

path_commute <- get_path_pair(gb_graph,"25497595","185650679")

length_commute <- get_distance_pair(gb_graph,"25497595","185650679")

nodes_path_query <- paste0("SELECT id, ST_X(geom) AS x,"
                           ,"ST_Y(geom) AS Y "
                           ,"FROM nodes_simp WHERE "
                           ,"id IN ('"
                           ,paste0(path_commute[[1]],collapse="','")
                           ,"');")

nodes_commute_coord <- DBI::dbGetQuery(conn_db, nodes_path_query) |>
  as.data.table()

path_dt <- data.table("nodes" = path_commute[[1]])

path_dt <- merge.data.table(path_dt
                            ,nodes_commute_coord
                            ,by.x = "nodes"
                            ,by.y = "id"
                            ,sort = FALSE
                            ,all.y = TRUE)

path_lines <- get_lines(from = path_dt[,.(x,y)],crs = 27700) |> st_as_sf()

path_crow <- get_lines(path_dt[1,.(x,y)]
                       ,path_dt[.N,.(x,y)]
                       ,crs = 27700) |> st_as_sf()

length_crow <- st_length(path_crow)

detour <- (length_commute/length_crow)

#### limits bbox

limit_2 <- c(-0.1384,51.5125,-0.0957,51.5233)

make_poly(limit_2) |> st_bbox()

lines <- london_edges[st_intersects(london_edges,make_poly(limit_2),sparse = FALSE),]

tmap::tm_shape(lines,bbox = limit_2) + tmap::tm_lines(col = "black",lwd = 1) +
  tmap::tm_shape(path_lines) + tmap::tm_lines(col = "darkred"
                                                ,lwd = 3) +
  tmap::tm_shape(path_crow) + tmap::tm_lines(col = "darkblue"
                                               ,lwd = 3) +
  tmap::tm_layout(main.title = paste0("Detour index, 1.18"))



