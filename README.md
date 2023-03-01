
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CASA_quant

<!-- badges: start -->
<!-- badges: end -->

The goal of CASA_quant is to document the process of building large
scale networks, routing and gravity modelling with R. Some of the data
necessary to run these scripts requires specific access authorizations
that can be obtained by registering with an institution on the
UKDataService website and then downloading the census flows at MSOA
level from WICID.

# Downloading networks

Up to a certain scale, like a big city, the regular packages handle the
task pretty well. One like [**osmnx**](https://github.com/gboeing/osmnx)
in python for example proved good to download graph format data (edges
and nodes separately with all the necessary data for each.)

# Routing

Routing is done with the
[**cppRouting**](https://github.com/vlarmet/cppRouting) package. For
that, one needs to find the nearest node in the network to the centroid
of the origin and destination geometries. In base R, this can be done
with the `st_nearest_feature` function from **sf**, which will work
efficiently up to a certain number of locations. A medium city size
network with around 200 origin/destination points seemed to be the limit
on the machine on which it was run.

# Gravtiy model

Done with the [**cppSim**](https://github.com/ischlo/cppSim) package,
available from github.

# Local test

The scripts most suited for a local test are in the
[small_area_run](https://github.com/ischlo/QUANT_at/tree/main/small_area_run)
folder. A tutorial will be made to explain that in more details.