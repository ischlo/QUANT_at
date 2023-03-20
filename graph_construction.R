# library(readr)
library(data.table)
library(igraph)
library(cppRouting)
library(DBI)
# library(tidytable)

####
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
#   setkey(ways,from,to)
#
#   return(ways[from %in% vert_ids & to %in% vert_ids,])
# }


####

way_nodes <- fread("/Users/ivann/Documents/CASA_quant/data/way_nodes.csv"
                  ,nrows = 5000
                  ,header =TRUE)

way_nodes$node_id <- as.character(way_nodes$node_id)


#### TWO ways of getting the nodes, either by reading from a database, or a csv, which is very big.
# if ram is limited, use db to get some nodes of interest.
# nodes <- fread("/Users/ivann/Documents/CASA_quant/data/db_files/nodes_proj.csv"
#                 ,nrows = 100
#                ,header = TRUE)

db_name <- "osm_gb"
conn_db <-
  DBI::dbConnect(RPostgreSQL::PostgreSQL()
                 ,host = "localhost"
                 ,dbname = db_name)

nodes_query <- paste0("SELECT id, ST_X(ST_Transform(geom,27700)) as x, ST_Y(ST_Transform(geom,27700)) as y FROM nodes "
                      ,"WHERE id IN ('"
                      ,paste0(unique(way_nodes$node_id), collapse = "', '")
                      ,"');")

nodes <- DBI::dbGetQuery(conn = conn_db
                    ,statement = nodes_query) %>% as.data.table()

nodes$id <- as.character(nodes$id)

# get_length <- function(x,y) {  }

# profvis::profvis({
  setorder(way_nodes, way_id, sequence_id)

  ways <- merge.data.table(way_nodes
                           ,nodes
                           ,sort = FALSE
                           ,by.x = "node_id"
                           ,by.y = "id"
                           ,all.x = TRUE)

  print(any(is.na(ways$x),is.na(ways$y)))

  #####

  # x <- 1:15
  #
  # shift(x,n = 1L,type = "lag")

  ways[,`:=`(from = shift(node_id, 1,type = "lag")
             ,from_x = shift(x,1,type = "lag")
             ,from_y = shift(y,1,type = "lag"))]

  ways %>% head(50)

  setnames(ways, c("node_id","x","y"),c("to","to_x","to_y"))

  ways %>% head()

  setkey(ways, sequence_id,physical = FALSE)

  ways <- ways[sequence_id != 0,]

# })

#####


# profvis::profvis({
  # edges <- fread("/Users/ivann/Documents/CASA_quant/data/db_files/edges_graph.csv"
  #                ,header = TRUE
  #                ,nrows = 100)
  #
  # setnames(edges, c("from_id","to_id"),c("from","to"))

  edges[,`:=`(from = as.character(from)
              ,to = as.character(to))]

  stopifnot("data.table" %in% class(edges), "from" %in% colnames(edges), "to" %in% colnames(edges))

  igraph_edges <- igraph::graph_from_data_frame(edges[,.(from,to)],directed = FALSE)

  if(igraph_edges |> igraph::is_connected(mode = "weak")) {stop("Already a connected graph")}

  nodes_comp <- igraph::components(igraph_edges,mode = "weak")

  vert_ids <- igraph::V(igraph_edges)[nodes_comp$membership == which.max(nodes_comp$csize)]$name

  # setkey(edges,from,to)

  edges <- edges[from %in% vert_ids & to %in% vert_ids,]
# })

fwrite(edges, "edges_lcc.csv")

# nodes <- fread("/Users/ivann/Documents/CASA_quant/data/db_files/nodes_proj.csv"
#                ,header  =TRUE
#                ,nrows = 10000)

# head(nodes$id)
# head(edges$to)

gb_graph <- cppRouting::makegraph(df = edges[,.("from","to","length")]
                                  ,directed = FALSE)

gb_graph %>% rlist::list.save("gb_graph.rds")
