
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
task pretty well. One like
[**osmnx**](https://github.com/gboeing/osmnx)\[@boeing_osmnx_2017\] in
python for example proved good to download graph format data (edges and
nodes separately with all the necessary data for each.)

# Routing

Routing is done with the
[**cppRouting**](https://github.com/vlarmet/cppRouting)
\[@larmet_cppR_2022\] package. For that, one needs to find the nearest
node in the network to the centroid of the origin and destination
geometries. In base R, this can be done with the `st_nearest_feature`
function from **sf** \[@pebesma_simple_2018\], which will work
efficiently up to a certain number of locations. A medium city size
network with around 200 origin/destination points seemed to be the limit
on the machine on which it was run.

# Gravtiy model

Done with the [**cppSim**](https://github.com/ischlo/cppSim) package,
available from github. See example in the package description.

# Local test

The scripts most suited for a local test are in the
[small_area_run](https://github.com/ischlo/QUANT_at/tree/main/small_area_run)
folder. A tutorial will be made to explain that in more details.

# References

Larmet V (2022). *cppRouting: Algorithms for Routing and Solving the
Traffic Assignment Problem*. R package version 3.1,
<https://CRAN.R-project.org/package=cppRouting>.

Pebesma, E., 2018. Simple Features for R: Standardized Support for
Spatial Vector Data. The R Journal 10 (1), 439-446,
<https://doi.org/10.32614/RJ-2018-009>

Boeing, G. 2017. “OSMnx: New Methods for Acquiring, Constructing,
Analyzing, and Visualizing Complex Street Networks.” Computers,
Environment and Urban Systems 65, 126-139.
<doi:10.1016/j.compenvurbsys.2017.05.004>
