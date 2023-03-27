library(data.table)
library(Btoolkit)

code_lookup <- fread("/Users/ivann/Documents/CASA_quant/data/EWS_ZoneCodes.csv"
                     ,header = TRUE)

code_lookup$areakey |> is.character()
code_lookup |> nrow()

gb_areas <- fread("/Users/ivann/Documents/CASA_quant/data/gb_areas_wkt.csv")

# important to specify the right path to the latest
# version of the nodes_codes file for the right graph
nodes_codes <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_1/nodes_codes.csv"
                     ,header = TRUE)

nodes_codes[,id:=as.character(id)]

nodes_codes$id |> unique() |> length() # every node should be unique here !!

# nodes_codes[duplicated(id),]

Btoolkit::overlap(code_lookup$areakey,gb_areas$area_code) # should be equal to 1

nodes_codes_ordered <- merge.data.table(code_lookup[,.(zonei,areakey)]
                 ,nodes_codes
                 ,by.x = "areakey"
                 ,by.y = "code"
                 ,all = TRUE
                 # ,sort = FALSE
                 )

setorder(nodes_codes_ordered,zonei)

fwrite(nodes_codes_ordered, "/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_1/nodes_codes_ordered.csv")

