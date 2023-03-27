library(data.table)
library(sf)
library(Btoolkit)
library(DBI)
library(RPostgreSQL)

gb_mat <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/v_1/distance_mat_gb/distance_mat_gb.csv"
                ,header = TRUE
                # ,nrows = 1000
                ) |> as.matrix()

gb_mat |> dim()

gb_mat <- `diag<-`(gb_mat,1)

any(is.na(gb_mat))

diag(gb_mat) |> summary()

which(gb_mat == 0, arr.ind = TRUE)

####

nodes_codes <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/nodes_codes_ordered.csv")

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

####

gb_nodes <- fread("/Users/ivann/Documents/CASA_quant/gb_graph_ch/nodes_simplified.csv"
                  ,header = TRUE)

gb_nodes[,id:=as.character(id)]

####

ids <- nodes_codes[duplicated(id),id]

gb_nodes[id %in% ids,] |> st_as_sf(coords = c(2,3),crs = 27700) |> tmap::qtm()

####

gb_areas_coord <- gb_areas[,"centroid"] |> st_as_sf(crs = 4326,wkt = 1) |> st_coordinates()

# gb_areas_distance <- st_distance(gb_areas_coord[samp_orig,],gb_areas_coord[samp_dest,],by_element = FALSE) |>
#   units::set_units(NULL)

gb_areas_distance <- fdistance(gb_areas_coord,gb_areas_coord,one_to_one = FALSE,coords = "unprojected")

gb_areas_distance <- `diag<-`(gb_areas_distance,1)

####

samp_orig <- unique(which(gb_mat == 0, arr.ind = TRUE)[,1]) #sample(1:8436,1000)
samp_dest <- unique(which(gb_mat == 0, arr.ind = TRUE)[,2]) #sample(1:8436,1000)

samp <- which(gb_mat == 0, arr.ind = TRUE)

cor(gb_areas_distance[samp] |> c()
    ,gb_mat[samp] |> c())

(gb_mat[samp]/gb_areas_distance[samp]) |> hist(breaks = 100)

(gb_mat[samp]/gb_areas_distance[samp]) |> min()

gb_mat[samp] |> dim()

######

tmap::tmap_mode("view")

tmap::qtm(gb_areas[samp_orig,.(geometry,area_code,id)] |> st_as_sf()
          ,fill = "black"
            ,fill.alpha = .5
          ,border.alpha = 1) +
  (gb_nodes[id %in% ids,] |> st_as_sf(coords = c(2,3),crs = 27700) |> tmap::qtm())
  # tmap::qtm(gb_areas[samp_dest,geometry]
  #           ,fill = "red"
  #             ,fill.alpha = .5)


#### makind the list of edges to add to the graph.
# the bboxes here are manually determined
# in order to extract the nodes and find the links to create
# adding ferry lines basically
# based on the results from this map:

gb_nodes[id %in% ids,] |> st_as_sf(coords = c(2,3),crs = 27700) |> tmap::qtm()

study_area <- function(limits) {
  poly_1 <- make_poly(limits = limits) |> st_as_text()

  nodes_query_1 <- paste0("SELECT id, ST_X(geom) AS x,"
                          ,"ST_Y(geom) AS Y "
                          ,"FROM nodes WHERE "
                          ,"ST_Contains(ST_GeomFromText('"
                          ,poly_1
                          ,"',4326), geom);")

  area_1 <- DBI::dbGetQuery(conn = conn_db,nodes_query_1)

  area_1 |> st_as_sf(coords = c(2,3),crs = 4326) |> tmap::qtm()
}

append_edge <- function(edge_list,orig_id_,dest_id_) {
  edge_list$orig_id <- append(edge_list$orig_id, orig_id_)
  edge_list$dest_id <- append(edge_list$dest_id, dest_id_)
  return(edge_list)
}

####

new_edges <- list("orig_id" = c(),"dest_id" = c())

#### isle of wight
# limits_1 <- c(-1.56,50.69,-1.47,50.77)
# study_area(limits = limits_1)

new_edges$orig_id <- append(new_edges$orig_id, "4871473738")
new_edges$dest_id <- append(new_edges$dest_id, "1489908423")

#### isle of wight
# limit_2 <- c(-1.17,50.71,-1.06,50.79)
# study_area(limits = limit_2)

new_edges <- append_edge(new_edges,"437221614","30203413")

#### Rothesay island, Bute ??

# limit_3 <- c(-5.07,55.83,-4.87,55.88)
# study_area(limit_3)
new_edges <- append_edge(new_edges,"6483639130","10219179")

#### Hoy

# limits_4 <- c(-3.5953,58.5783,-3.2410,58.9819)
# study_area(limits_4)
new_edges <- append_edge(new_edges,"428652367","3483047391")
new_edges <- append_edge(new_edges,"3483047391","881176856")
new_edges <- append_edge(new_edges,"3483047391","10319966")

##### Hoy again

# limits_5 <- c(-3.16,58.62,-2.93,58.85)
# study_area(limits_5)
new_edges <- append_edge(new_edges,"618315066","1512647335")
new_edges <- append_edge(new_edges,"1347076390", "4377602426")

#### Shetland and Hoy
# easier to find the endpoints of ferry lines one by one because otherwise
# to much data is loaded at once
# limits_6 <- c(-2.9721,58.9801,-2.9498,58.9935)
# study_area(limits_6)

new_edges <- append_edge(new_edges,"254197133","2408374710")
new_edges <- append_edge(new_edges,"9847117365","1507541603")
new_edges <- append_edge(new_edges,"9847117365","1898620800")

#### Steornabhagh

# limits_7 <- c(-7.1616,57.5953,-7.1530,57.5989)
# study_area(limits_7)
new_edges <- append_edge(new_edges,"5310640485","457354875")
new_edges <- append_edge(new_edges,"457354875","2622696915")

#### Other remaining islands connected to the bigger one

# limits_8 <- c(-7.2098,57.6792,-6.9894,57.7767)
# study_area(limits_8)

# node to add because one of the msoas has no roads at all...
node_01 <- data.table(id = "100000000", x = -7.0794,y = 57.7392) |>
  st_as_sf(coords = c(2,3),crs = 4326) |>
  st_transform(27700) |>
  dplyr::mutate(x = st_coordinates(geometry)[,1]
                ,y = st_coordinates(geometry)[,2]) |>
  st_drop_geometry()

##

new_edges <- append_edge(new_edges,"922254994","7948732244")
new_edges <- append_edge(new_edges,"100000000","922254994")


#### getting the coordinates of the nodes:

conn_db <- DBI::dbConnect(RPostgreSQL::PostgreSQL()
                          ,dbname = "osm_gb")

nodes_coords_list <-  new_edges |> unlist() |> unname() |> unique()
nodes_coords_query <- paste0("SELECT id, ST_X(ST_Transform(geom,27700)) as x, ST_Y(ST_Transform(geom,27700)) as y FROM nodes "
                             ,"WHERE id IN ('"
                             ,paste0(nodes_coords_list, collapse = "', '")
                             ,"');")

nodes_coords <- dbGetQuery(conn = conn_db,nodes_coords_query) |> as.data.table()

nodes_coords[,id:=as.character(id)]

nodes_coords <- rbind(nodes_coords,node_01)

####

new_edges_dt <- as.data.table(new_edges)

new_edges_dt <- merge.data.table(new_edges_dt
                                 ,nodes_coords
                                 ,by.x = "orig_id"
                                 # ,suffixes =
                                 ,by.y = "id"
                                 ,sort = FALSE
                                 ,all.x = TRUE)

new_edges_dt <- merge.data.table(new_edges_dt
                                 ,nodes_coords
                                 ,by.x = "dest_id"
                                 ,by.y = "id"
                                 ,suffixes = c("_from","_to")
                                 ,sort = FALSE
                                 ,all.x = TRUE)

setnames(new_edges_dt,names(new_edges_dt),c("to","from","from_x","from_y","to_x","to_y"))

setcolorder(new_edges_dt,c("from","to","from_x","from_y","to_x","to_y"))

new_edges_dt |> fwrite("/Users/ivann/Documents/CASA_quant/data/extra_edges.csv",append = FALSE)

