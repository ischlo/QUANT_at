library(data.table)

####

gb_graph <- rlist::list.load("/Users/ivannschlosser/Documents/CASA_quant/gb_graph_ch/v_3/gb_graph_ch.rds")

nodes <- gb_graph$dict |> as.data.table()
nodes[,ref:=as.character(ref)]

edges <- gb_graph$original$data |> as.data.table()

#####
nodes |> head()
nodes[(edges$from[1:10]+1),ref]

edges_simp <- list("from" = nodes[(edges$from+1),ref]
                   ,"to" = nodes[(edges$to+1),ref]
                   ,"length" = edges$dist) |>
  as.data.table()

fwrite(edges_simp,"/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_3/edges_simple.csv")
