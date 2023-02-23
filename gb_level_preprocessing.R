library(sf)
library(data.table)
library(tmap)
library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)
library(dbplyr)
library(Matrix)

# remotes::install_github("ischlo/cppSim")
# 
# library(cppSim)


# source this file to generate the data sets. Or read it in
# source("ews_areas.R")

ews_areas <- st_read("data/ews_areas.geojson") |> as.data.table()

#### uk od data :

data("flows_test")
data("distance_test")

# flows_test
# distance_test

profvis::profvis(expr={
  at_ews_model <- cppSim::run_model(flows_test
                                    ,distance_test)}
)

# optimise calibration... probably apply
at_benchmark_cppSIM <- 
  microbenchmark::microbenchmark(
    "cppSim" = cppSim::run_model(flows_test
                                 ,distance_test)
    ,times = 10)
