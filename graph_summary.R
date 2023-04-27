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

gb_graph$original$data$dist |> summary()

# density only of links that are less than 25km long, removes the ferrys
gb_edge_dens <- density(round(gb_graph$original$data$dist[gb_graph$original$data$dist > 0 &
                                                            gb_graph$original$data$dist < 25000])
                        ,bw = .8
                        # ,adjust = 1
                        )

plot(gb_edge_dens
     ,main = "Distribution of edge length"
     ,xlab = "Edge length"
     ,ylab = "Density"
     ,log = "xy"
     ,ylim = c(1e-7,1)
     ,xlim = c(1,25000))

#### NODE DEGREE

nodes_degree <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_3/nodes_degree.csv")

png(filename = "presentation/deg_distr.png"
    ,bg = "white"
    )
par(bty = "n")
nodes_degree[,.N,keyby = "degree"] |> plot(pch = 19
                                        ,log = "y"
                                        ,type = "b"
                                        ,main = "Degree distribution of nodes")
dev.off()


#### Looking at the high degree nodes:

high_deg_nodes <- nodes_degree[degree >=10,]

hdn_query <- paste0("select * from way_nodes where way_id IN "
                    ,"(select way_id from way_nodes where node_id IN ('"
                    ,paste0(high_deg_nodes$node_id,collapse = "','")
                    ,"'));")

high_deg_ways <- DBI::dbGetQuery(conn_db,hdn_query) |>
  as.data.table()

hdw_nodes_query <- paste0("select id, ST_X(geom) as x, ST_Y(geom) as y from nodes "
                          ,"where id IN ('"
                          ,paste0(high_deg_ways$node_id,collapse = "','")
                          ,"');")

hdw_nodes <- DBI::dbGetQuery(conn = conn_db,hdw_nodes_query) |>
  as.data.table()

high_deg_ways <- merge.data.table(high_deg_ways
                                  ,hdw_nodes
                                  ,by.x = "node_id"
                                  ,by.y = "id"
                                  ,all = TRUE
                                  ,sort = FALSE
                                  )

hig_deg_lines <- high_deg_ways[,list(get_lines(from = .SD[,.(x,y)])),by = "way_id"] |>
  st_as_sf()

tmap::tmap_mode("view")
tmap::tm_shape(hig_deg_lines) + tmap::tm_lines(lwd = 3)

#### Getting the network out of a bbox in fitzrovia just for the illustration putposes
#
# colnames_db <- lapply(dbListTables(conn = conn_db), FUN = function(x) tbl(conn_db, x) |> colnames()) |>
#   `names<-`(dbListTables(conn = conn_db))
#
# box <- Btoolkit::get_bb("Marylebone,London, UK")
#
# nodes_query <- paste0("SELECT id, ST_X(geom) as x, ST_Y(geom) as y FROM nodes"
#                       ," WHERE ST_Intersects(nodes.geom,ST_GeomFromText('"
#                       ,box
#                       ,"',4326));")
#
# nodes <- dbGetQuery(conn = conn_db, statement = nodes_query) |> as.data.table()
#
# nodes |> nrow()
#
# ways_query <- paste0("SELECT * FROM way_nodes WHERE node_id IN ('"
#                      ,paste(nodes$id,collapse = "','")
#                      ,"');")
#
# ways <- dbGetQuery(conn = conn_db, statement = ways_query) |> as.data.table()
#
# nrow(ways)
#
# setorderv(ways,cols =  c("way_id","sequence_id"))
#
# ways[,`:=`(way_id = as.character(way_id)
#            ,node_id = as.character(node_id))]
#
# nodes[,`:=`(id = as.character(id))]
#
# ways <- merge.data.table(ways, nodes, by.x = "node_id",by.y = "id", all = TRUE,sort = FALSE)
#
# ways_lines <- ways[,list(Btoolkit::get_lines(from = .SD[,.(x,y)])), by = way_id]
#
# ways_lines <- ways_lines[way_id %in% ways[,.N,by = "way_id"][N!=1,way_id],]
#
# tmap::tmap_mode("plot")
#
# ways_lines$V1 |> st_is_valid() |> sum()
#
# par(bty = "n"
#     ,bg = "transparent")
# png(filename = "fitzrovia_network.png"
#     ,height = 960
#     ,bg = "transparent")
# ways_lines |> st_as_sf(sf_column_name = "V1") |> tmap::qtm(bbox = osmdata::getbb("Fitzrovia, London, UK"))
# dev.off()

