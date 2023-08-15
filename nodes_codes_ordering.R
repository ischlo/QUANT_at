library(data.table)
library(Btoolkit)

code_lookup <- fread("data/EWS_ZoneCodes.csv"
                     ,header = TRUE)

code_lookup$areakey |> is.character()
code_lookup |> nrow()

gb_areas <- fread("data/gb_areas_wkt.csv")

# important to specify the right path to the latest
# version of the nodes_codes file for the right graph
# nodes_codes <- fread("gb_graph_ch/v_3/nodes_codes.csv"
#                      ,header = TRUE)
#
# nodes_codes[,id:=as.character(id)]
#
# nodes_codes$id |> unique() |> length() # every node should be unique here !!

##### the population and workpplace weighted centroids.

nodes_codes_pop <- fread("gb_graph_ch/v_3/nodes_codes_pop.csv"
                         ,header = TRUE)
nodes_codes_pop[,id_pop:=as.character(id_pop)]

##
nodes_codes_work <- fread("gb_graph_ch/v_3/nodes_codes_work.csv"
                          ,header = TRUE)
nodes_codes_work[,id_work:=as.character(id_work)]

nodes_codes_join <- merge.data.table(nodes_codes_pop
                                     ,nodes_codes_work
                                     ,by = 'code'
                                     ,all = TRUE
                                     ,sort = FALSE)
nodes_codes_join[,length(unique(code))]
nrow(nodes_codes_join) # 8436

# which_equal <- nodes_codes_join[id_pop==id_work,code]

# gb_areas[match(which_equal,area_code)]
#
# tmap::tmap_mode('view')
# gb_areas[match(which_equal,area_code)] |>
#   sf::st_as_sf(wkt = 3,crs=27700) |>
#   tmap::qtm() + (
#     gb_areas[match(which_equal,area_code)] |>
#       sf::st_as_sf(wkt = 5,crs=27700) |>
#       tmap::qtm(dots.col = 'red')
#   )

# nodes_codes[duplicated(id),]

Btoolkit::overlap(code_lookup$areakey,gb_areas$area_code) # should be equal to 1

nodes_codes_ordered <- merge.data.table(code_lookup[,.(zonei,areakey)]
                 ,nodes_codes_join
                 ,by.x = "areakey"
                 ,by.y = "code"
                 ,all = TRUE
                 # ,sort = FALSE
                 )

setorder(nodes_codes_ordered,zonei)

nrow(nodes_codes_ordered)
nodes_codes_ordered[,length(unique(areakey))]

### validate ###

nodes_codes_ordered |> lapply(FUN = typeof)
nodes_codes_ordered |> summary()
nodes_codes_ordered |> lapply(FUN = function(x){any(is.na(x),is.null(x))})

# not sure why i was doind that
nodes_codes_ordered[,`:=`(areakey=as.factor(areakey)
                          ,id_pop=as.factor(id_pop)
                          ,id_work=as.factor(id_work))]

####

fwrite(nodes_codes_ordered, "gb_graph_ch/v_3/nodes_codes_ordered.csv"
       ,row.names = FALSE
       ,col.names = TRUE)

# nodes_codes_ordered |> readr::write_csv("gb_graph_ch/v_3/nodes_codes_ordered_2.csv")

