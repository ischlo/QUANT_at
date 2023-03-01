 # THIS SCRIPT IS SUPPOSED TO RUN AFTER nearest_feature_db.R 
# WHICH IS OUTDATED, SO CHECK NEXT TIME BEFORE USE.

library(cppRouting)
library(sf)
library(data.table)
library(rlist)
library(dplyr)
library(readr)
library(foreach)
library(DBI)
library(RPostgreSQL)

####

st_layers("/Users/ischlo/Documents/CASA/RC_outputs/london_all/london_all.gpkg")

road_links <- st_read("/Users/ischlo/Documents/CASA/RC_outputs/london_all/london_all.gpkg"
                      ,layer = "edges")
road_links <- road_links |> st_drop_geometry()

colnames(road_links)

road_links[,c("from","to","length")]

london_graph <- cppRouting::makegraph(df = road_links[,c("from","to","length")]
                                      ,directed = FALSE)

london_graph <- london_graph |> cpp_simplify(keep = nn[,1])

london_graph_ch <- london_graph |> cpp_contract()

london_distance_matrix <- cppRouting::get_distance_matrix(london_graph
                                                          ,from = nn[,1]
                                                          ,to = nn[,1])
### benchmarking the contraction hierarchy
# 
# hraph_ch_benchmark <- microbenchmark::microbenchmark(
#   "grpah_ch" = cppRouting::get_distance_matrix(london_graph_ch
#                                                ,from = nn[,1]
#                                                ,to = nn[,1])
#   ,"graph" = cppRouting::get_distance_matrix(london_graph
#                                              ,from = nn[,1]
#                                              ,to = nn[,1])
#   ,times = 10
# )
# hraph_ch_benchmark
# 
# list("benchmark" = hraph_ch_benchmark
#      ,"description" = "normal vs contraction hierarchy computation of distance matrix for london") |> rlist::list.save("graph_ch_benchmark.rds")
