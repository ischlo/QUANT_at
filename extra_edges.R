library(data.table)
library(sf)
library(Btoolkit)
library(DBI)
library(RPostgreSQL)
library(leaflet)
library(leafgl)

####
# FIND outliers through the detour method in script 'detour_outliers.R'
# and then use this script to create the extra edges.

####
conn_db <- DBI::dbConnect(RPostgreSQL::PostgreSQL()
                          ,dbname = "osm_gb")

dbListTables(conn = conn_db)

eval(
expression("conn_db")
)

#### Support functions

study_area <- function(limits
                       ,conn = conn_db) {
  require(leaflet)
  require(leafgl)
  # uses the leafGL library to render a high number of points allowing to load
  # a greater observation windiw and debugging faster

  poly_1 <- Btoolkit::make_poly(limits = limits) |> sf::st_as_text()

  nodes_query_1 <- paste0("SELECT id, ST_X(geom) AS x,"
                          ,"ST_Y(geom) AS Y "
                          ,"FROM nodes WHERE "
                          ,"ST_Contains(ST_GeomFromText('"
                          ,poly_1
                          ,"',4326), geom);")

  area_1 <- DBI::dbGetQuery(conn = conn_db,nodes_query_1) |>
    sf::st_as_sf(coords = c(2,3),crs = 4326)

  leaflet() %>%
    addProviderTiles(provider = "OpenStreetMap")  |>
    addGlPoints(data = area_1
                ,fillOpacity = 1
                ,fillColor = "black"
                ,radius = 9
                ,popup = area_1$id)


}

append_edge <- function(edge_list,orig_id_,dest_id_) {
  #  appends the new edge to the edge list.
  #  kinf off a la OOP class methods
  edge_list$orig_id <- append(edge_list$orig_id, orig_id_)
  edge_list$dest_id <- append(edge_list$dest_id, dest_id_)
  return(edge_list)
}

####

new_edges <- list("orig_id" = c(),"dest_id" = c())

#### isle of wight
#enter coordinates from osm bbox as you select an origin destination for the new links.
limits_1 <- c(-1.2166,50.7312,-1.1549,50.7426)
study_area(limits = limits_1)

# Ryde and fishbourne to portsmouth
new_edges <- append_edge(new_edges,"5605574131","2275907")
new_edges <- append_edge(new_edges,"5605574131","8082355")
new_edges <- append_edge(new_edges,"549571906","12891749")

#### isle of wight 2
# cowes to southampton
# limit_2 <- c(-1.4071,50.8932,-1.4006,50.8975)
# study_area(limits = limit_2)

new_edges <- append_edge(new_edges,"734780","3701966595")
new_edges <- append_edge(new_edges,"8069263322","3701966595")

# new_edges <- append_edge(new_edges,"437221614","30203413")

#### Rothesay island, Bute ??

# limit_3 <- c(-5.1631,55.9184,-5.1469,55.9250)
# study_area(limit_3)

new_edges <- append_edge(new_edges,"448434","10219179")
new_edges <- append_edge(new_edges,"445967","10219179")
new_edges <- append_edge(new_edges,"6483639130","10219179")

# rubodach - collintraive
new_edges <- append_edge(new_edges,"10299346","10299349")

#### Dunoon etc

# limity_3_5 <- c(-4.9317,55.9438,-4.8098,55.9879)
# study_area(limity_3_5)

new_edges <- append_edge(new_edges,"434134478","8372697690")
new_edges <- append_edge(new_edges,"434134478","1351354496")
new_edges <- append_edge(new_edges,"1048620","10299545")

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

#### Additional edges that were idetified from the detour index.

#  Scilly isles
# limits_9 <- c(-6.4270,49.8505,-6.2478,49.9866)
# study_area(limits_9)

new_edges <- append_edge(new_edges,"59938403","380752650")
new_edges <- append_edge(new_edges,"380752650","3520715373")
new_edges <- append_edge(new_edges,"380752650","2132421393")
new_edges <- append_edge(new_edges,"380752650","2406056170")
new_edges <- append_edge(new_edges,"380752650","380755375")
new_edges <- append_edge(new_edges,"380752650","2409581174")
new_edges <- append_edge(new_edges,"380752650","503600592")
new_edges <- append_edge(new_edges,"380752650","503600539")


## Aran and campbelltown

# limits_10 <- c(-5.4087,55.8644,-5.3126,55.8786)
# study_area(limits_10)

new_edges <- append_edge(new_edges,"921259760","7104734957")
new_edges <- append_edge(new_edges,"921259760","291781139")
new_edges <- append_edge(new_edges,"7104734957","291781139")
new_edges <- append_edge(new_edges,"738090921","9953646019")
new_edges <- append_edge(new_edges,"738090921","291756486")
new_edges <- append_edge(new_edges,"291756486","291742341")

### Jura and Islay
#
# limits_11 <- c(-6.2155,55.6140,-5.4630,56.0904)
# study_area(limits_11)

new_edges <- append_edge(new_edges,"2755468621","1149819226")
new_edges <- append_edge(new_edges,"1149819226","271517078")
new_edges <- append_edge(new_edges,"1149819226","1789028296")
new_edges <- append_edge(new_edges,"317347299","277883465")
new_edges <- append_edge(new_edges,"276605069","470272037")
new_edges <- append_edge(new_edges,"7785854418","6823737912")

#### getting the coordinates of the nodes:

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

new_edges_dt[,length:=sqrt((from_x-to_x)^2+(from_y-to_y)^2)]

# multiplying by a realistic detour factor.
new_edges_dt[,length:=length*1.4]

#### Visual inspection of the edges

new_edges_lines <- list("geom" = get_lines(from = new_edges_dt[,.(from_x,from_y)]
                                           ,to = new_edges_dt[,.(to_x,to_y)]
                                           ,crs = 27700)
                        ,"length" = new_edges_dt$length
                        ) |>
  as.data.table() |>
  st_as_sf()

tmap::tm_shape(new_edges_lines) +
  tmap::tm_lines(lwd = 1
                 ,col = "length"
                 ,palette = "viridis")

####
# make sure everything is good before saving
# new_edges_dt |> fwrite("/Users/ivann/Documents/CASA_quant/data/extra_edges_1.csv",append = FALSE)







