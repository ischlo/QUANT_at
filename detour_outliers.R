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


#### Setting the diagonal value of the distance matrice to be the squareroot of the area.

d <- gb_areas[["geometry"]] |>
  st_area() |>
  sqrt() |>
  # units::set_units("km") |>
  units::set_units(NULL)

hist(d,breaks = 100)

gb_mat <- `diag<-`(gb_mat,d)

gb_mat[,1]

gb_mat <- `dimnames<-`(gb_mat,list(NULL,NULL))

fwrite(gb_mat,"gb_graph_ch/v_3/distance_mat_gb/gb_dist_diag.csv",row.names = FALSE,col.names = FALSE)
# write.table()
####
# validating the output
fread("gb_graph_ch/v_3/distance_mat_gb/gb_dist_diag.csv",nrows = 10)


#### Using my own function that is very fast, and projecting the coordinates before that.
gb_areas_distance <- fdistance(gb_areas_coord,gb_areas_coord,one_to_one = FALSE,coords = "projected")

diag(gb_areas_distance) |> summary()

gb_areas_distance <- `diag<-`(gb_areas_distance,1)

gb_detour <- (gb_mat/gb_areas_distance)

gb_detour |> as.numeric() |> summary()

outliers <- which(gb_detour > 5,arr.ind = TRUE)

outliers

gb_detour[outliers] |> hist(breaks=100)

outliers_coords <-
  list("from"= gb_areas[outliers[,1],area_code]
       ,"from_x"=gb_areas_coord[outliers[,1],1]
       ,"from_y"=gb_areas_coord[outliers[,1],2]
       ,"to"=gb_areas[outliers[,2],area_code]
       ,"to_x"=gb_areas_coord[outliers[,2],1]
       ,"to_y"=gb_areas_coord[outliers[,2],2]
       ,"detour" = gb_detour[outliers]) |>
  as.data.table()

outliers_coords$geom <- get_lines(from = outliers_coords[,.(from_x,from_y)]
                           ,to = outliers_coords[,.(to_x,to_y)]
                           ,crs = 27700)

tmap::tmap_mode("view")

outliers_coords |>
  st_as_sf() |>
  tmap::qtm(lines.col = "detour"
            ,lines.palette = "viridis"
            ,lines.lwd = 1)





