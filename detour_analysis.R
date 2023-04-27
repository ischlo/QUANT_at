library(data.table)
library(sf)
library(Btoolkit)
library(DBI)
library(RPostgreSQL)

####
conn_db <- DBI::dbConnect(RPostgreSQL::PostgreSQL()
                          ,dbname = "osm_gb")

dbListTables(conn = conn_db)

####

gb_mat <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_3/distance_mat_gb/distance_mat_gb.csv"
                ,header = TRUE) |>
  as.matrix()

gb_mat |> dim()

diag(gb_mat) |> summary()

any(is.na(gb_mat))

which(gb_mat == 0, arr.ind = TRUE) # should equal the number of origins.

####

nodes_codes <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_3/nodes_codes_ordered.csv")

nodes_codes[,id:=as.character(id)]

unique(nodes_codes$id) |> length()

####

gb_areas <- st_read("/Users/ivann/Documents/CASA_quant/data/gb_areas.geojson") |> as.data.table()

gb_areas <- merge(gb_areas,nodes_codes
                  ,sort = FALSE
                  ,by.x = "area_code"
                  ,by.y = "areakey"
                  ,all.x = TRUE
                  ,suffixes = c("",""))

setorder(gb_areas,"zonei")

gb_areas_coord <- gb_areas[,"centroid"] |>
  st_as_sf(crs = 4326,wkt = 1) |>
  st_transform(27700) |>
  st_coordinates()

#### Using my own function that is very fast, and projecting the coordinates before that.
gb_areas_distance <- fdistance(gb_areas_coord,gb_areas_coord,one_to_one = FALSE,coords = "projected")

diag(gb_areas_distance) |> summary()

gb_areas_distance <- `diag<-`(gb_areas_distance,1)

gb_detour <- (gb_mat/gb_areas_distance)

gb_detour |> as.numeric() |> summary()

####

gb_detour <- round(gb_detour,2)

gb_areas_distance <- round(gb_areas_distance,-2)

#### Assembling the flows.

uk_flows <- dplyr::tbl(conn_db, "uk_flows")

flows_db <- uk_flows |>
  dplyr::select(residence,workplace, foot,bicycle)

# flows_db |> show_query()

flows <- flows_db |> dplyr::collect() |>

flows |> dim()

flows <- flows |>
  dplyr::mutate(at = bicycle + foot) |>
  as.data.table()

flows$residence |> unique() |> length()

####

flows$from <- nodes_codes[match(flows$residence,areakey), (zonei+1)]
flows$to <- nodes_codes[match(flows$workplace,areakey), (zonei+1)]

####

flows <- flows[!is.na(from),]
flows <- flows[!is.na(to),]

####

flows |> dim()

####

flows$distance <- gb_areas_distance[flows[,as.matrix(.SD[,.(from,to)])]]
flows$detour <- gb_detour[flows[,as.matrix(.SD[,.(from,to)])]]

flows[at!=0,summary(distance)]

####

detour_dist_summary <- flows[,list(avg=mean(detour)
                                   ,med = median(detour)),keyby = "distance"]

detour_dist_summary |> dim()

plot(detour_dist_summary$distance
     ,detour_dist_summary$avg
     ,main = "Average detour per distance"
     ,log = "x"
     ,xlab = "distance"
     ,ylab = "detour"
     ,pch = 19
     ,cex = .3)

####

# to simplify the work of the density function, select only inside an interval
# of interest.

detour_density <- flows[detour < 1.6 & detour > .8 & at != 0,detour] |>
  stats::density()

detour_density_w <- flows[detour < 1.6 & detour > .8 & at != 0,detour] |>
  stats::density(weights = flows[detour < 1.6 & detour > .8 & at != 0,at/sum(at)])

####

hist(flows[detour < 1.6 & detour > .8 & at != 0,detour],breaks = 100, freq = FALSE)

plot(detour_density
     ,main = "Detour distribution"
     ,xlab = "Detour index")
lines(detour_density_w
      ,col = "darkred")
legend(x = 1.4
       ,y = 6
       ,fill = c("black","darkred")
       ,legend = c("unweighted","weighted"))

#### Comparing observed detours with randomly sampled ones.

rand_orig <- round(runif(n = 1000000,min = (1/nrow(gb_areas)))*nrow(gb_areas),0)
rand_dest <- round(runif(n = 1000000,min = (1/nrow(gb_areas)))*nrow(gb_areas),0)

####

mean(gb_detour[cbind(rand_orig,rand_dest)][gb_areas_distance[cbind(rand_orig,rand_dest)] < 15000])

rand_dens <- density(gb_detour[cbind(rand_orig,rand_dest)][gb_areas_distance[cbind(rand_orig,rand_dest)] < 15000])

plot(rand_dens)


