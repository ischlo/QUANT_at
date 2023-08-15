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
library(reshape2)

# library(connections) # https://rstudio.github.io/connections/ check here for more

###########

db_name <- "osm_gb"

conn_db <-
  DBI::dbConnect(RPostgreSQL::PostgreSQL()
                               ,host = "localhost"
                               ,dbname = db_name)

# way_nodes_db <- tbl(conn_db,"way_nodes")
#
# nodes_db <-  tbl(conn_db, "nodes")
#
# ways_db <- tbl(conn_db,"ways")
#
# tables <- dbListTables(conn_db)
#
# col_names <- lapply(tables, function(x) colnames(tbl(conn_db, x))) |> `names<-`(tables)
#
# col_names

######

# bb <- osmdata::getbb("Liverpool, UK")

# tmap::qtm(orig_poly
#     ,fill.alpha = .5) + tmap::qtm(lims)

### Set the limits of the observation window, local analysis doesn't allow to full country scale models
# so the next steps extract only the part of the network for a selected observation window:
#  two ways to proceed are possible here:
# 1) extract the way_nodes corresponding to the ways that are inside the observation window
#   then extract all the nodes inside this observation window.
    # Disadvantage: Need to extend the window for getting nodes beacuase some might be part of ways that
    # are only partially inside the o_w. So need to select a wider o_w, but how much wider ??
    # MAYBE: take as buffer the length of the longest way in the data set.
    # ADVANTAGES: with the right spatial index in place should be very fast, faster than (2)
# 2) extract the way_nodes corresponding to the ways that are inside the observation window
#   then extract all the nodes that are in the way_nodes node_ids extracted
#   DISADVANTAGE: intuitively, the IN operation of the set of desired nodes on the whole set of nodes
    # seems to be a slow thing to do
    # ADVANTAGE: is easy to implement, no need to consider wider windows of observation and risk
    # selecting a bad value.


# IF LINESTRING IS ENABLIED IN THE PSQL DB, YOU CAN QUERY WITH A BBOX BY SELECTING
# ONLY THE INTERSECTING WAYS. THE DOWNSIDE TO THAT IS THAT THE WAYS
# TABLE USES A LOT MORE MEMORY WHEN STORING THE GEOMETRY AS WELL.
# # this only queries the ways data table which is not really of interest here
# ways_query <- paste0("SELECT id, ST_AsText(linestring) AS geom FROM ways "
#                      ,"WHERE ST_Intersects(linestring,ST_GeomFromText('"
#                      ,poly_bound
#                      ,"',4326));")
# ways_query <- paste0("SELECT * FROM way_nodes WHERE way_id IN"
#                      ,"(SELECT id FROM ways WHERE ST_Intersects(linestring, ST_GeomFromText('"
#                      ,orig_poly
#                      ,"',4326)));")
#
# ways_oi <- dbGetQuery(conn = conn_db
#                       ,statement = ways_query) |>
#   as.data.table()

# nodes_query <- paste0("SELECT id, ST_AsText(geom) AS geom FROM nodes "
#                       ,"WHERE id IN ('"
#                       ,paste0(unique(ways_oi$node_id), collapse = "', '")
#                       ,"');")
#
# nodes_oi <- dbGetQuery(conn_db, nodes_query) |> as.data.table()
#
# ways_dt <-
#   ways_oi |>
#   group_by(way_id) |>
#   reframe(from = node_id[1:(dplyr::n()-1)]
#           ,to = node_id[2:dplyr::n()]) |>
#   as.data.table()
#
# ways_dt <- dplyr::left_join(ways_dt
#                             ,nodes_oi
#                             ,by = c("from" = "id")
#                             ,suffix = c("", "_from")
#                             ,keep = FALSE)
#
# ways_dt <- dplyr::left_join(ways_dt
#                             ,nodes_oi
#                             ,by = c("to"= "id")
#                             ,suffix = c("", "_to")
#                             ,keep = FALSE)
#
# if(ways_dt$geom |> is.na() |> any()) {
#   print("Some ways have an empty 1st node")
#   } else print("from nodes OK")
#
# if(ways_dt$geom_to |> is.na() |> any()) {
#   print("Some ways have an empty 2nd node")
#   } else print("to nodes OK")
#
# # ways_dt <- ways_dt |> drop_na(geom_wkt,geom_wkt_to)
#
# ways_dt <- get_lcc(ways = ways_dt)
#
# ways_dt$len <- st_distance(st_as_sf(ways_dt[,"geom"],wkt = 1, crs = 4326)
#                            ,st_as_sf(ways_dt[,"geom_to"], wkt = 1, crs = 4326)
#                            ,by_element = TRUE) |> units::set_units(NULL) |> round(1)
#
# ways_dt$len |> summary()
#
# # Making the nodes
#
# nodes_dt <- nodes_oi |>
#   st_as_sf(wkt = 2, crs = 4326) |>
#   mutate(X = st_coordinates(geom, crs = 4326)[,1]
#          ,Y = st_coordinates(geom, crs = 4326)[,2]) |>
#   st_drop_geometry() |>
#   as.data.table()
#
# england_graph <- cppRouting::makegraph(df = ways_dt[,.(from,to,len)]
#                                        ,coords = nodes_dt
#                                        ,directed = FALSE) # |>
#   # cpp_simplify(iterate = TRUE
#   #              ,rm_loop = FALSE)
#
# # All this to build the network


##### READ in. the graph

gb_graph <- rlist::list.load("gb_graph_ch/v_3/gb_graph_ch.rds")

gb_graph$nbnode

format(object.size(gb_graph),units = "Mb")

nodes_codes <- fread("gb_graph_ch/v_3/nodes_codes_ordered.csv"
                     ,header = TRUE)

nodes_codes[,id:=as.character(id)]

#### Now connecting to the other db to get the flows matrice and the centroids.

test_area_name <- "Scotland, UK"

orig_poly <- osmdata::getbb(test_area_name
                            # ,format_out = "sf_polygon"
                            ) |> make_poly()

# orig_poly

# tmap::tmap_mode("view")
# orig_poly |> tmap::qtm(fill.alpha = .5)

orig_poly_wkt <- Btoolkit::get_bb(test_area_name)

area_selection_query <- paste0("SELECT code, name, ST_AsText(geom) AS geometry "
                               ,"FROM gb_areas WHERE ST_Intersects(geom, ST_GeomFromText('"
                               ,orig_poly_wkt
                               ,"', 4326));")

area_selection <- dbGetQuery(conn = conn_db, area_selection_query)

area_selection$code |> unique()
area_selection$code |> unique() |> length()

areas <- area_selection$code

####

uk_flows <- tbl(conn_db, "uk_flows")

flows_db <- uk_flows |>
  select(residence,workplace, foot,bicycle) |>
  filter((residence %in% areas) & (workplace %in% areas))

# flows_db |> show_query()

flows <- flows_db |> collect()

flows |> nrow()

flows <- flows |> mutate(at = foot)#bicycle +

flows_mat <- flows |>
  as.data.table() |>
  dcast.data.table(residence ~ workplace
                   ,value.var = "at"
                   ,fill = 0
                   ,drop = FALSE)

flows_mat |> dim()

areas <- flows_mat[,2:ncol(flows_mat)] |> names()

areas |> length()

flows_mat  <- flows_mat[,2:ncol(flows_mat)] |> as.matrix()

from <- to <- nodes_codes[match(areas,nodes_codes$areakey),id]

RcppParallel::setThreadOptions(numThreads = 3)

system.time({
distance_mat <<- cppRouting::get_distance_matrix(gb_graph
                                                ,from = from
                                                ,to = to
                                                ,algorithm = "mch")
})


### import gb_areas to do this.

gb_areas[errors,'centroid'] |> st_as_sf(wkt=1,crs = 4326)

leaflet(data = gb_areas[errors,geometry]) |>
  leaflet::addTiles() |>
  addCircles(data = gb_areas[errors,'centroid'] |> st_as_sf(wkt=1,crs = 4326)
              ,fillColor = 'black') |>
  leafgl::addGlPoints(wc_centroid[errors_s[,1],]
                      ,fillColor = 'red'
                      ,fillOpacity = 1)

diag_dist <- sf::st_distance(gb_areas[errors,'centroid'] |> st_as_sf(wkt=1,crs = 4326)
                             ,gb_areas[errors,'w_centroid'] |> st_as_sf(wkt=1,crs = 4326)
                             ,by_element = TRUE) |>
  round() |>
  units::set_units(NULL)

errors <- which(is.na(diag_dist))

diag_dist <- diag_dist/1000

distance_mat <- `diag<-`(distance_mat,diag_dist[match(areas,gb_areas$area_code)])

errors_s <- which(is.na(distance_mat),arr.ind = TRUE)

# microbenchmark::microbenchmark(
#   "distance_mat_mch" = cppRouting::get_distance_matrix(gb_graph
#                                                        ,from = from
#                                                        ,to = to
#                                                        ,algorithm = "mch")
#   ,"distance_mat_phast" = cppRouting::get_distance_matrix(gb_graph
#                                                           ,from = from
#                                                           ,to = to
#                                                           ,algorithm = "phast")
#   ,times = 1
# )

distance_mat <- distance_mat/1000

distance_mat |> dim()

distance_mat |> c() |> hist(breaks = 100)

# distance_mat

which(is.na(distance_mat),arr.ind = TRUE)

#### SIM

typeof(flows_mat)
class(flows_mat)
typeof(distance_mat)
class(distance_mat)

profvis::profvis({
  sim <- cppSim::simulation(flows_matrix = flows_mat
                            ,dist_matrix = distance_mat
                            ,beta_offset = 0.6)

})

sim$best_fit_beta

sim$best_fit_values |> dim()
flows_mat |> dim()

cor(sim$best_fit_values[flows_mat!=0] |> as.numeric()
    ,flows_mat[flows_mat!=0] |> as.numeric() |> as.numeric())^2 |> round(2)


flows_dt <- data.table('flow'=c(flows_mat)
                       ,'distance'=c(distance_mat)
                       ,'flow_model'=c(sim$best_fit_values)
                       )

ped_speed <- 5

flows_dt[distance!=0,paste0(round(sum(flow*distance)/sum(flow)/5*60,0),' minutes.')]

flows_dt[flow!=0,cor(flow,flow_model)]

flows_dt[flow!=0 & flow_model!=0][
  ,{plot(flow,flow_model,log='xy')
    lines(1:1000,1:1000,col = 'darkred',lwd = 2)}]



