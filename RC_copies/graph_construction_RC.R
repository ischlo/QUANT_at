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

ways <- fread("/home/ucfnisc/Scratch/osm_gb_graph/data/way_nodes.csv"
                   ,header =TRUE)

ways$node_id <- as.double(way_nodes$node_id)

nodes <- fread("/home/ucfnisc/Scratch/osm_gb_graph/data/nodes_text.csv"
               ,header = TRUE)

nodes$id <- as.double(nodes$id)

setorder(ways, way_id, sequence_id)

ways <- merge.data.table(ways
                         ,nodes
                         ,sort = FALSE
                         ,by.x = "node_id"
                         ,by.y = "id"
                         ,all.x = TRUE)

print(any(is.na(ways$geom)))

#####

# x <- 1:15
#
# shift(x,n = 1L,type = "lag")

ways[,`:=`(from = shift(node_id, 1,type = "lag"),from_geom = shift(geom,1,type = "lag"))]

ways %>% head(50)

setnames(ways, c("node_id","geom"),c("to","to_geom"))

ways %>% head()

setkey(ways, sequence_id,physical = FALSE)

ways <- ways[sequence_id != 0,]

fwrite(ways,file = "edges.csv")


