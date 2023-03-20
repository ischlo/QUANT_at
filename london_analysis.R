
# double checking how apply() works, the margin value indicates which
# matrix index is fixed (rows = 1, cols = 2) when applying the function
#
# test <- matrix(data = 1:9,nrow = 3,byrow = TRUE)
# apply(test, MARGIN = 1, sum)


## run osm_gb.R file on a given network. before this one

area_selection$observed_out <- apply(flows_mat, FUN = sum, MARGIN = 1)

area_selection$observed_in <- apply(flows_mat, FUN = sum, MARGIN = 2)

### model data

area_selection$model_out <- apply(sim$best_fit_values, FUN = sum, MARGIN = 1)

area_selection$model_in <- apply(sim$best_fit_values, FUN = sum, MARGIN = 2)

####

cor(area_selection$observed_out,area_selection$model_out)

cor(area_selection$observed_in,area_selection$model_in)


####

area_selection <- area_selection |> mutate(resid_out = observed_out-model_out
                                           ,resid_in = observed_in-model_in)

area_selection |> st_as_sf(wkt = 3,crs = 4326) |> tmap::tm_shape() + tmap::tm_symbols(col = "resid_out"
                                                                                      ,palette = "viridis"
                                                                                      ,size = .1
                                                                                      ,size.max = 5
                                                                                      ,n = 7)


####

geom <- area_selection[match(areas,area_selection$code),] |> st_as_sf(wkt = 3,crs = 4326) |> st_coordinates()

euclid_dist <- fdistance(geom
                         ,geom
                         ,one_to_one = FALSE
                         ,coords = "unprojected")

euclid_dist <- euclid_dist/1000

any(is.na(euclid_dist |> c()))

####

distance_mat <- `diag<-`(distance_mat,.1)
euclid_dist <- `diag<-`(euclid_dist,.1)

distance_mat |> c() |> hist(freq = FALSE,breaks = 100)
euclid_dist |> c() |> hist(freq = FALSE,breaks = 100)

(distance_mat/euclid_dist) |> c() |> hist(breaks = 100)

delta_m <- (distance_mat/euclid_dist)

delta_m |> c() |> hist(breaks = 100)

area_selection$delta <- apply(delta_m,FUN = median,MARGIN = 1)

area_selection |> st_as_sf(wkt = 3,crs = 4326) |> tmap::tm_shape() + tmap::tm_symbols(col = "delta"
                                                                                      ,palette = "Reds"
                                                                                      ,size = .1
                                                                                      ,size.max = 5
                                                                                      ,n = 7)


plot(area_selection$delta, area_selection$observed_in, log = "y")

plot(area_selection$delta, area_selection$observed_out, log = "y")

# hist(delta_m |> c()
#      ,breaks = 100)

i <- sample(length(euclid_dist), 300)

plot(euclid_dist[i],distance_mat[i], log = "xy")

glm((distance_mat |> c()) ~ (euclid_dist |> c()), family = poisson())







