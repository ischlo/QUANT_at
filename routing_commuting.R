library(data.table)
library(sf)
library(Btoolkit)
library(cppRouting)
library(rlist)

####

gb_graph <- rlist::list.load('gb_graph_ch/v_3/gb_graph_ch.rds')

nodes_codes <- fread('gb_graph_ch/v_3/nodes_codes_ordered.csv')

gb_areas <- sf::st_read('data/gb_areas.geojson')

# flows <-

####

from_samp <- Btoolkit::samp_dt(nodes_codes,.2)

from_samp |> nrow()

from_samp[,`:=`(id_pop=as.character(id_pop)
               ,id_work=as.character(id_work))]

#### Routing

RcppParallel::setThreadOptions(numThreads = 7)

profvis::profvis(
  {test_mat_comm <- cppRouting::get_distance_matrix(gb_graph
                                                   ,from = nodes_codes$id_pop
                                                   ,to = nodes_codes$id_work
                                                   ,algorithm = 'mch'
  )}
)


diag(test_mat_comm) |> hist(breaks = 100)

# test_mat_comm |> dim()

# test_mat_comm |> c() |> is.na() |> any()

zeros <- which(test_mat_comm==0,arr.ind = TRUE)[,1]

dists <- sf::st_distance(
  gb_areas[zeros,c('centroid','w_centroid')] |> st_drop_geometry() |> st_as_sf(wkt = 1,crs=27700)
  ,gb_areas[zeros,c('centroid','w_centroid')] |> st_drop_geometry() |> st_as_sf(wkt = 2,crs=27700)
  ,by_element = TRUE
) |> units::set_units(NULL)

hist(dists,breaks=100)

test_mat_comm[which(test_mat_comm==0,arr.ind = TRUE)] <- dists

# gb_areas[zeros,] |> st_area() |> units::set_units(NULL) |> sqrt() |> hist(breaks = 100)

# gb_areas[zeros[227],] |> tmap::qtm()

# test_mat <- cppRouting::get_distance_matrix(gb_graph
#                                             ,from = from_samp$id_pop
#                                             ,to = from_samp$id_pop
#                                             ,algorithm = 'mch'
# )

###
#
# test_mat <- `diag<-`(test_mat,gb_areas[from_samp$zonei+1,'geometry'] |> st_area() |> units::set_units(NULL) |> sqrt())
#
# (test_mat - test_mat_comm) |> hist(breaks=100)

# out <- which((test_mat - test_mat_comm)>5000,arr.ind = TRUE)[,2] |> unique()
# tmap::tmap_mode('view')
# gb_areas[from_samp[out,zonei]+1,] |> tmap::qtm()
