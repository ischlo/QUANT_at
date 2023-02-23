# THIS SCRIPT WILL VALIDATE THE FLOWS DATA SET BY COMPARING THE AREA CODES WITH THE GEOMETRY FILES
library(readr)
library(sf)
library(Btoolkit)

col_names <- c("residence"
               ,"workplace"
               ,"all"
               ,"home"
               ,"underground"
               , "train"
               , "bus"
               , "taxi"
               , "motorcycle"
               , "car_driver"
               ,"car_passenger"
               ,"bicycle"
               ,"foot"
               ,"other")

file_path <- "./data/WU03UK_msoa/wu03uk_msoa_v3.csv"

flows <- readr::read_csv(file = file_path
                         ,col_names = col_names)

flows |> colnames()

flows |> head()

flows |> nrow()

flows$residence |> unique() |> length()

#### validating the residence and workplace columns with the ews geofile

gb_areas <- read_csv("/Users/ivann/Documents/CASA/data/gb_areas_wkt.csv")

gb_areas |> nrow()

gb_areas_cd <- gb_areas$area_code

#  england and wales are 7201 geographies (MSOAs)
# 
#  for better data from scotland: maybe check 
# https://www.spatialdata.gov.scot/geonetwork/srv/api/records/389787c0-697d-4824-9ca9-9ce8cb79d6f5
# 1,279 intermediate zones should be in the 2011 scotish census

# Northern ireland separately for now
ni_soa <- st_read("/Users/ivann/Documents/CASA/data/census_geographies/SOA2011_Esri_Shapefile_0/SOA2011.shp")

overlap(gb_areas_cd
        ,flows$residence
        ,frac = FALSE)
# The number of intersecting values corresponds to ENGLAND + SCOTLAND + WALES 
#  OURA !

overlap(ni_soa$SOA_CODE
        ,flows$residence
        ,frac = FALSE)
#  YEAAHHHH, everything mathches

flows$all |> sum()




