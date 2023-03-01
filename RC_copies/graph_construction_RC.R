library(data.table)
# library(igraph)
# library(cppRouting)

####
#
# get_lcc <- function(ways, mode = "weak") {
#
#   # require("igraph")
#
#   stopifnot("data.table" %in% class(ways), "from" %in% colnames(ways), "to" %in% colnames(ways))
#
#   igraph_ways <- igraph::graph_from_data_frame(ways[,.(from,to)],directed = FALSE)
#
#   if(igraph_ways |> igraph::is_connected(mode = "weak")) {stop("Already a connected graph")}
#
#   nodes_comp <- igraph::components(igraph_ways,mode = "weak")
#
#   vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)]$name
#
#   return(ways[from %in% vert_ids & to %in% vert_ids,])
# }


####

way_nodes <- fread(here::here("data/way_nodes.csv")
                   # ,nrows = 25000
                   ,header =TRUE)

nodes <- fread(here::here("data/nodes_text.csv")
                # ,nrows = 10000
               ,header = TRUE)

ways <- merge(way_nodes,nodes, by.x = "node_id", by.y = "id",all.x = TRUE)

if(any(is.na(ways$geom))){print("Some nodes are missing;")}

# ways_edges <- ways[,cbind(.SD[1:(.N-1)
#                               ,.(orig = node_id
#                                  ,orig_geom = geom)],.SD[2:.N,.(dest = node_id
#                                                                 ,dest_geom = geom)])
#                    ,by = way_id]
#
# ways_edges[,length := round(units::set_units(st_distance(st_as_sf(.SD[,"orig_geom"],wkt = 1,crs = 4326)
#                                                          ,st_as_sf(.SD[,"dest_geom"],wkt = 1,crs = 4326)
#                                                          ,by_element = TRUE),NULL))]
#
