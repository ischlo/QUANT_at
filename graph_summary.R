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

####


db_name <- "osm_gb"

conn_db <-
  DBI::dbConnect(RPostgreSQL::PostgreSQL()
                 ,host = "localhost"
                 ,dbname = db_name)

####

gb_graph <- rlist::list.load("/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_1/gb_graph_ch.rds")

gb_edge_dens <- density(gb_graph$original$data$dist[gb_graph$original$data$dist > 0]
                        ,bw = 3
                        ,adjust = 1)

plot(gb_edge_dens
     ,main = "Distribution of edge length"
     # ,xlab = "Edge length"
     ,ylab = "Density"
     ,log = "xy"
     ,ylim = c(1e-7,1e-1)
     ,xlim = c(1,25000))

bw_1 <- 6.65
bw_2 <- 6.49

gb_graph$original$data$dist |> summary()

#### NODE DEGREE


nodes_degree <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_1/nodes_degree.csv")


png(filename = "presentation/deg_distr.png"
    ,bg = "white"
    )
par(bty = "n")
nodes_degree[,.N,keyby = "degree"] |> plot(pch = 19
                                        ,log = "y"
                                        ,type = "b"
                                        ,main = "Degree distribution of nodes")
dev.off()

####

colnames_db <- lapply(dbListTables(conn = conn_db), FUN = function(x) tbl(conn_db, x) |> colnames()) |>
  `names<-`(dbListTables(conn = conn_db))

box <- Btoolkit::get_bb("Fitzrovia, London, UK")

nodes_query <- paste0("SELECT id, ST_X(geom) as x, ST_Y(geom) as y FROM nodes"
                      ," WHERE ST_Intersects(nodes.geom,ST_GeomFromText('"
                      ,box
                      ,"',4326));")

nodes <- dbGetQuery(conn = conn_db, statement = nodes_query)
nodes <- as.data.table(nodes)

ways_query <- paste0("SELECT * FROM way_nodes WHERE node_id IN ('"
                     ,paste(nodes$id,collapse = "','")
                     ,"');")

ways <- dbGetQuery(conn = conn_db, statement = ways_query)

ways <- as.data.table(ways)

setorderv(ways,cols =  c("way_id","sequence_id"))

ways[,`:=`(way_id = as.character(way_id)
           ,node_id = as.character(node_id))]

nodes[,`:=`(id = as.character(id))]

ways <- merge.data.table(ways, nodes, by.x = "node_id",by.y = "id", all = TRUE,sort = FALSE)

ways_lines <- ways[,list(Btoolkit::get_lines(from = .SD[,.(x,y)])), by = way_id]

ways_lines <- ways_lines[way_id %in% ways[,.N,by = "way_id"][N!=1,way_id],]

tmap::tmap_mode("plot")

ways_lines

par(bty = "n"
    ,bg = "transparent")
png(filename = "fitzrovia_network.png"
    ,height = 960
    ,bg = "transparent")
ways_lines |> st_as_sf(sf_column_name = "V1") |> tmap::qtm(bbox = osmdata::getbb("Fitzrovia, London, UK"))
dev.off()




