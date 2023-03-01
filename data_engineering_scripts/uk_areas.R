# This file assembles the geographies of interest.
# The whole of the uk is made (England, Wales, Scotland, NI)
# , as well as a separate file for great britain (England, Wales, Scotland)

# The idea is to have :
# - MSOA for england and Wales from 2011 census
# - IntermediateZone for scotland from 2001 census
# - SOA for northern ireland

# The geographies of the 2011 Scotish census were originally the same as the 2001 census,
# but have since been change. This change, however,
# does not seem to have been included in the uk level data that you find on WICID and UKDataServices
#
# any data set or variable here names centroid or centr, refers to population weighted centroid


library(sf)
library(data.table)
library(tmap)
library(dplyr)
library(readr)
library(tidyr)
library(DBI)
library(RSQLite)
library(dbplyr)
library(Btoolkit)

# general info at:
# https://www.ons.gov.uk/methodology/geography/ukgeographies/censusgeographies/2011censusgeographies


#  GOOD SOURCE for UK level which means the scotish census data does not correspond tou the flows:
# https://statistics.ukdataservice.ac.uk/dataset/2011-census-geography-boundaries-middle-layer-super-output-areas-and-intermediate-zones
# areas <- st_read("data/infuse_msoa_lyr_2011/infuse_msoa_lyr_2011.shp")


# reading in England and Wales

#  table from: https://data.cambridgeshireinsight.org.uk/dataset/output-areas
ew_msoa <-
  st_read("data/census_geographies/MSOA_EngWal_Dec_2011_Generalised_ClippedEW_0/Middle_Layer_Super_Output_Areas_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales.shp")

# data from: https://geoportal.statistics.gov.uk/search?q=msoa%202011
ew_msoa_centr <- st_read("data/census_geographies/MSOA_(Dec_2011)_Population_Weighted_Centroids_in_England_and_Wales.geojson")

ew_msoa |> head()
ew_msoa_centr |> head()

ew_msoa |> nrow()
ew_msoa_centr |> nrow()

st_crs(ew_msoa)
st_crs(ew_msoa_centr)

ew_msoa_centr %>% st_coordinates() %>% as.data.frame() %>% lapply(summary)

overlap(ew_msoa$msoa11cd
        ,ew_msoa_centr$msoa11cd
        ,frac = FALSE)

ew_msoa |> colnames()
ew_msoa_centr |> colnames()

## validating geometries

ew_msoa$geometry |> st_is_valid() |> summary()
ew_msoa_centr$geometry |> st_is_valid() |> summary()


## correcting the msoas geometry
ew_msoa$geometry <- st_make_valid(ew_msoa$geometry)

#selecting the columns of interest and renaming

ew_msoa <- ew_msoa |> select(msoa11cd,msoa11nm,geometry)

ew_msoa_centr <- ew_msoa_centr |>
  select(msoa11cd,geometry) |>
  mutate("centroid" = st_as_text(geometry)
         ,"country" = "e_w")

ew_areas <- inner_join(ew_msoa
                       ,ew_msoa_centr |> st_drop_geometry()
                       , by = "msoa11cd")

ew_areas <- ew_areas |> st_transform(4326)

ew_areas <- ew_areas |> st_cast("POLYGON")

ew_areas <- ew_areas |> rename(area_code = msoa11cd
                               ,area_name = msoa11nm)

ew_areas %>% st_coordinates() %>%
  as.data.frame() %>%
  lapply(summary)


ew_areas |> st_write("data/england_wales_msoa_2011.geojson"
                     ,delete_dsn = TRUE)

#### Scotland data

# scottish intermediate zones 2011 from: NOT USED
# https://www.data.gov.uk/dataset/133d4983-c57d-4ded-bc59-390c962ea280/intermediate-zone-boundaries-2011
# st_layers("data/SG_IntermediateZoneBdry_2011/SG_IntermediateZone_Bdry_2011.shp")
#
# scotland_iz <-
#   st_read("data/SG_IntermediateZoneBdry_2011/SG_IntermediateZone_Bdry_2011.shp") |>
#   st_transform(4326)

# data from: https://spatialdata.gov.scot/geonetwork/srv/fre/catalog.search#/search
#  this data is from 2001
scotland_iz <-
  st_read("data/census_geographies/SG_IntermediateZoneBdry_2001/SG_IntermediateZone_Bdry_2001.shp")

scotland_iz_centr <-
  st_read("data/census_geographies/SG_IntermediateZoneCent_2001/SG_IntermediateZone_Cent_2001.shp")

scotland_iz |> nrow()
scotland_iz_centr |> nrow()

scotland_iz |> head()
scotland_iz_centr |> head()


### validating geometries

scotland_iz$geometry |> st_is_valid() |> summary()

scotland_iz$geometry <- st_make_valid(scotland_iz$geometry)

scotland_iz_centr$geometry |> st_is_valid() |> summary()

scotland_iz %>% st_crs()
scotland_iz_centr %>% st_crs()

####

scotland_iz <- scotland_iz %>% st_transform(4326)
scotland_iz_centr <- scotland_iz_centr %>% st_transform(4326)

####

scotland_iz |> colnames()
scotland_iz_centr |> colnames()

scotland_iz <- scotland_iz |> select(IZ_CODE,IZ_NAME,geometry)

scotland_iz_centr <-  scotland_iz_centr |>
  select(IZ_CODE,geometry) |>
  mutate(centroid = st_as_text(geometry)
         ,"country" = "s")

overlap(scotland_iz$IZ_CODE
        ,scotland_iz_centr$IZ_CODE
        ,frac = FALSE)

scotland_areas <- inner_join(scotland_iz
                             ,scotland_iz_centr |> st_drop_geometry()
                             ,by = "IZ_CODE")

scotland_areas |> head()

scotland_areas <- scotland_areas |> rename(area_code = IZ_CODE
                                           ,area_name = IZ_NAME)

#  crs is 4326, cast into polygons and save

scotland_areas <- scotland_areas |> st_cast("POLYGON")

scotland_areas |> st_write("data/scotland_iz_2001.geojson"
                           ,delete_dsn = TRUE)

# GREAT BRITAIN : England, Scotland, WALES together

gb_areas <- bind_rows(ew_areas,scotland_areas)

#  saving great britain as geojson and as wkt

gb_areas |> st_write("data/gb_areas.geojson"
                     ,delete_dsn = TRUE)

# only gb is needed for centroids in wkt
gb_areas |> st_drop_geometry() |> write_csv("data/gb_areas_wkt.csv"
                                            ,append = FALSE)



# UNITED KINGDOM: adding northern ireland to the previous.
# for northern ireland, check here:
# https://www.nisra.gov.uk/support/geography
# https://www.nisra.gov.uk/support/output-geography-census-2011/super-output-areas
# ni_soa <- st_read("/Users/ivann/Documents/CASA/data/census_geographies/SOA2011_Esri_Shapefile_0/SOA2011.shp")
#
# ni_soa |> nrow()
# ni_soa |> head()
#
# ni_soa$geometry |> st_is_valid() |> summary()
# ni_soa$geometry <- ni_soa$geometry |> st_make_valid()

# LOOK FOR POPULATION WEIGHTED CENTROIDS for NI

