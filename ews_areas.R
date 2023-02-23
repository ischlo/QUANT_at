library(sf)
library(data.table)
library(tmap)
library(dplyr)
library(readr)
library(tidyr)
library(DBI)
library(RSQLite)
library(dbplyr)

# general info at:
# https://www.ons.gov.uk/methodology/geography/ukgeographies/censusgeographies/2011censusgeographies


#  probably better source :
# https://statistics.ukdataservice.ac.uk/dataset/2011-census-geography-boundaries-middle-layer-super-output-areas-and-intermediate-zones 
areas <- st_read("data/infuse_msoa_lyr_2011/infuse_msoa_lyr_2011.shp")

# downloaded from : 
# https://data.cambridgeshireinsight.org.uk/dataset/output-areas
england_wales_msoa <- 
  st_read("data/MSOA_EngWal_Dec_2011_Generalised_ClippedEW_0/Middle_Layer_Super_Output_Areas_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales.shp") |> 
  st_transform(4326)


# scotland_iz$InterZone

# for northern ireland, check here:
# https://www.nisra.gov.uk/support/geography 
# https://www.nisra.gov.uk/support/output-geography-census-2011/super-output-areas
ni_areas <- st_read("data/SOA2011_Esri_Shapefile_0/SOA2011.shp")

uk_areas <- c(areas$geo_code,ni_areas$SOA_CODE) |> unique()

uk_areas |> length()

####

england_wales_msoa |> st_geometry_type() |> unique()

england_wales_msoa |> head()

england_wales_msoa |> st_is_valid() |> summary()

england_wales_msoa$geometry <- england_wales_msoa$geometry |> st_make_valid()

england_wales_msoa$centroid_geom <- 
  england_wales_msoa$geometry |> 
  st_centroid()

####

scotland_iz |> st_geometry_type() |> unique()

scotland_iz |> head()

scotland_iz |> st_is_valid() |> summary()

scotland_iz$geometry <- scotland_iz$geometry |> st_make_valid()

object.size(scotland_iz)

scotland_iz_2001 |> st_geometry_type() |> unique()

scotland_iz_2001 |> head()

scotland_iz_2001 |> st_is_valid() |> summary()

scotland_iz_2001$geometry <- scotland_iz_2001$geometry |> st_make_valid()

### preview
tmap_mode("view")
scotland_iz[sample(nrow(scotland_iz), 100),] |> qtm()
england_wales_msoa[sample(nrow(england_wales_msoa), 100),] |> qtm()

# for some reason the Scotland data weights more that England and wales... maybe complexity of shapes

scotland_iz |> head(5)

england_wales_msoa |> head(5)

scotland_iz |> colnames()

england_wales_msoa |> colnames()

# manipulating data sets to combine them.
# creating columns: 
# id: integer identifier
# code: the code of the area, for scotland the interZone, for E&W the msoa
# name: name of the area
# geometry: the shape
# centroid_geom: centroid point

england_wales_msoa <- england_wales_msoa |> as.data.table()

scotland_iz <-  scotland_iz |> as.data.table()

scotland_iz <- scotland_iz |> dplyr::rename("code" = "InterZone"
                                            ,"name" = "Name")

england_wales_msoa <- england_wales_msoa[,.(code = msoa11cd
                                            ,name = msoa11nm
                                            ,geometry
                                            ,centroid_geom)]

class(scotland_iz)
class(england_wales_msoa)

ews_areas <- data.table::.rbind.data.table(england_wales_msoa,scotland_iz[,.(code,name,geometry,centroid_geom)])

class(ews_areas)

# area in square meters 
ews_areas[,area:=units::set_units(st_area(geometry),"km^2")]

# ews_areas[,hist(area,freq = FALSE,breaks = 100)]
# 
# # checking the outliers, big, uninhabitted zones in scotland 
# ews_areas[units::drop_units(area) > 2500,geometry] |> qtm()

ews_areas |> rlist::list.save("data/ews_areas.rds")
ews_areas |> sf::st_write("data/ews_areas.geojson", delete_dsn = TRUE)

ews_wkt <- ews_areas |> 
  mutate(centroid_wkt = st_as_text(centroid)) |> 
  st_drop_geometry() |> 
  mutate(centroid = NULL
         ,area = NULL)  

ews_wkt |> write_csv("data/ews_wkt.csv",append = FALSE)

# ews_wkt <- readr::read_csv("data/db_files/ews_wkt.csv")
# 
# ews_wkt |> nrow()
#
##################
