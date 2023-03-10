library(data.table)
library(Btoolkit)

code_lookup <- fread("/Users/ivann/Documents/CASA_quant/data/EWS_ZoneCodes.csv"
                     ,header = TRUE)

code_lookup$areakey |> is.character()

gb_areas <- fread("/Users/ivann/Documents/CASA_quant/data/gb_areas_wkt.csv")

nodes_codes <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/nodes_codes.csv"
                     ,header = TRUE)

code_lookup |> nrow()


Btoolkit::overlap(code_lookup$areakey,gb_areas$area_code) # should be equal to 1

nodes_codes_ordered <- merge.data.table(code_lookup[,.(zonei,areakey)]
                 ,nodes_codes
                 ,by.x = "areakey"
                 ,by.y = "code"
                 ,all = TRUE
                 # ,sort = FALSE
                 )

setorder(nodes_codes_ordered,zonei)

fwrite(nodes_codes_ordered, "/Users/ivann/Documents/CASA_quant/gb_graph_ch/nodes_codes_ordered.csv")

