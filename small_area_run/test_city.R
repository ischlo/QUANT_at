# THIS SCRIPT IS OUTDATED, IT IS SUPPOSED TO BE USED TOGETHER WITH graph.R AND nearest_feature.R
# So update when postgres is set up

library(sf)
library(data.table)
library(rlist)
library(dplyr)
library(readr)
# library(tmap) # to slow to load
library(foreach)
library(DBI)
library(RPostgreSQL)
library(RSQLite)
library(dbplyr)
library(cppSim)
library(Btoolkit)


#### connecting to the postgresql database that stores areas for ews, nodes for london, and uk_flows

conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL()
                       ,host = "localhost"
                       ,dbname = "nodes_pgsql") # this db doe not exist anymore. 

#### Load roadnetwork and make graph, make spatial data base of nodes

gpkg_filepath <- "/Users/ischlo/Documents/CASA/RC_outputs/london_all/london_all.gpkg"

road_nodes <- st_read(gpkg_filepath
                      ,layer = "nodes")

road_nodes$osmid <- as.character(road_nodes$osmid)

road_edges <- st_read(gpkg_filepath
                      ,layer = "edges")

#### make_graph

graph <- cppRouting::makegraph(df = road_edges[,c("from","to","length")] |> st_drop_geometry()
                               ,directed = FALSE
                               ,coords = road_nodes[,c("osmid","x","y")] |> st_drop_geometry())

#### load msoa data, find centroids
# 
# ews_areas <- st_read("/Users/ischlo/Documents/CASA/data/ews_areas.geojson")
# 
# ews_areas <- ews_areas |> mutate(centroid = st_centroid(geometry))

test_area <- "Greater London, UK"

observation_window <- get_bb(test_area)

area_selection_query <- paste0("SELECT code, name, ST_AsText(geom) AS geometry "
                               ,"FROM ews_areas WHERE ST_Intersects(geom, ST_GeomFromText('"
                               ,observation_window
                               ,"', 4326));")

area_selection <- dbGetQuery(conn, area_selection_query)

area_selection$code |> unique()

# area_selection <- area_selection |> st_as_sf(wkt = "geometry", crs = 4326)

# the next operation was replaced by the call to the postgres db
# area_selection <- ews_areas[st_intersects(observation_window,ews_areas$centroid,sparse = FALSE),]

areas <- area_selection$code

####

uk_flows <- tbl(conn, "uk_flows")

flows_db <- uk_flows |> 
  select(residence,workplace, foot,bicycle) |> 
  filter((residence %in% areas) & (workplace %in% areas))

# flows_db |> show_query()

flows <- flows_db |> collect()

flows |> nrow()

flows <- flows |> mutate(at = bicycle + foot)

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

#### 

# replace with database connection, uploading nodes, indexing, and running
# instead of running the following line for London, can run the nearest_feature_db script.
# this ultimately should transform into a database table where for the nodes of the graph of great britain, 
# the selected centroids of areas, there is a correspondence. 
# it can be an extra column of the ews_areas data for a given network. 
# the match in the following operation is a safety measure in case 
# the order of elements is changed when reassigning the areas from the cast matrix earlier

# nn <- st_nearest_feature(area_selection[match(areas,area_selection$code),"centroid"], road_nodes)

# from <- to <- nn

# the query is currently only valid for london as it's the data set imported on the pgsql db
nn_query <- paste0("SELECT nodes.osmid AS osmid , areas.code AS code "
                   ,"FROM area_selected areas "
                   ,"CROSS JOIN LATERAL (SELECT nodes.osmid, nodes.geom <-> areas.geom AS dist "
                   ,"FROM nodes ORDER BY dist LIMIT 1 ) nodes;")

nn <- dbGetQuery(conn = conn
                 ,statement = nn_query)

nn$osmid <- as.character(nn$osmid)

from <- to <- nn$osmid # graph$coords[match(road_nodes$osmid[nn],graph$coords$osmid),"osmid"] # road_nodes$osmid[nn]

length(from)

graph <- cppRouting::cpp_simplify(graph, keep = from)

graph_ch <- cppRouting::cpp_contract(graph)

RcppParallel::setThreadOptions(numThreads = 2)

# routing_algorithms_comparison <- 
#   microbenchmark::microbenchmark(
#     "distance_mat_ch" = cppRouting::get_distance_matrix(graph_ch
#                                                         ,from = from
#                                                         ,to = to
#                                                         ,algorithm = "mch"
#     )
#     ,"distance_mat_phast" = cppRouting::get_distance_matrix(graph_ch
#                                                             ,from = from
#                                                             ,to = to
#                                                             ,algorithm = "phast"
#     )
#     ,"distance_mat" = cppRouting::get_distance_matrix(graph
#                                                       ,from = from
#                                                       ,to = to
#                                                       # ,algorithm = "mch"
#     )
#     ,times = 5
# )
# routing_algorithms_comparison

distance_mat <- cppRouting::get_distance_matrix(graph_ch
                                                ,from = from
                                                ,to = to
                                                ,algorithm = "mch")

distance_mat <- distance_mat/1000

distance_mat |> dim()

#### SIM

typeof(flows_mat)
class(flows_mat)
typeof(distance_mat)
class(distance_mat)

sim <- cppSim::run_model(flows = flows_mat
                         ,distance = distance_mat
                         ,beta = 0.6)

# plot(flows_mat |> as.numeric()
#      ,sim$values |> as.numeric()
#      # ,log = "xy"
#      )

cor(sim$values |> as.numeric()
    ,flows_mat |> as.numeric())^2


