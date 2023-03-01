library(readr)
library(data.table)
library(igraph)
library(cppRouting)
# library(tidytable)

####

get_lcc <- function(ways, mode = "weak") {

  # require("igraph")

  stopifnot("data.table" %in% class(ways), "from" %in% colnames(ways), "to" %in% colnames(ways))

  igraph_ways <- igraph::graph_from_data_frame(ways[,.(from,to)],directed = FALSE)

  if(igraph_ways |> igraph::is_connected(mode = "weak")) {stop("Already a connected graph")}

  nodes_comp <- igraph::components(igraph_ways,mode = "weak")

  vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)]$name

  return(ways[from %in% vert_ids & to %in% vert_ids,])
}


####

way_nodes <- fread("/Users/ivann/Documents/CASA_quant/data/way_nodes.csv"
                  ,nrows = 25000
                  ,header =TRUE)


#### TWO ways of getting the nodes, either by reading from a database, or a csv, which is very big.
# if ram is limited, use db to get some nodes of interest.
# nodes <- fread("/Users/ivann/Documents/CASA_quant/data/nodes_text.csv"
#                 ,nrows = 10000
#                ,header = TRUE)

nodes_query <- paste0("SELECT id, ST_AsText(geom) AS geom FROM nodes "
                      ,"WHERE id IN ('"
                      ,paste0(unique(way_nodes$node_id), collapse = "', '")
                      ,"');")

nodes <- dbGetQuery(conn = conn_db
                    ,statement = nodes_query) %>% as.data.table()

way_nodes$node_id <- as.double(way_nodes$node_id)


# get_length <- function(x,y) {  }

ways <- merge(way_nodes,nodes, by.x = "node_id", by.y = "id",all.x = TRUE)

ways_edges <- ways[,cbind(.SD[1:(.N-1)
                              ,.(orig = node_id
                                 ,orig_geom = geom)],.SD[2:.N,.(dest = node_id
                                                                ,dest_geom = geom)])
                   ,by = way_id]

ways_edges[,length := round(units::set_units(st_distance(st_as_sf(.SD[,"orig_geom"],wkt = 1,crs = 4326)
                                                         ,st_as_sf(.SD[,"dest_geom"],wkt = 1,crs = 4326)
                                                         ,by_element = TRUE),NULL))]

# ways <- way_nodes[,.SD[.N != 1],by = node_id]

# way_nodes[,.SD[1:(.N-1),node_id],.SD[2:.N,node_id],keyby = way_id]




ways_dt <- way_nodes |>
  group_by(way_id) |>
  reframe(from = node_id[1:(dplyr::n()-1)]
          ,to = node_id[2:dplyr::n()]) |>
  as.data.table()

ways_dt <- get_lcc(ways = ways_dt)

ways_dt <- dplyr::left_join(ways_dt
                            ,nodes
                            ,by = c("from" = "id")
                            ,suffix = c("", "_from")
                            ,keep = FALSE)

ways_dt <- dplyr::left_join(ways_dt
                            ,nodes
                            ,by = c("to"= "id")
                            ,suffix = c("", "_to")
                            ,keep = FALSE)


ways_dt$len <- st_distance(st_as_sf(ways_dt[,"geom"],wkt = 1, crs = 4326)
                           ,st_as_sf(ways_dt[,"geom_to"], wkt = 1, crs = 4326)
                           ,by_element = TRUE) |> units::set_units(NULL) |> round(1)

nodes_coord <- nodes |>
  st_as_sf(wkt = 2, crs = 4326) %>%
  st_coordinates()

nodes_dt <- cbind(nodes[,"id"],nodes_coord) %>%
  as.data.table()

gb_graph <- cppRouting::makegraph(df = ways_dt[,.("from","to","length")]
                                  ,directed = FALSE)

gb_graph %>% rlist::list.save("gb_graph.rds")







