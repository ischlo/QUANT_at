Set up
================

This file covers briefly the process that goes into storing the network
data in a postgres database, it is WORK IN PROGRESS.

# Downloading data

Find the
[Geofabrick](http://download.geofabrik.de/europe/great-britain.html)
file corresponding to the UK and download it.

# Data engineering

We will then process the data with the
[osmosis](https://wiki.openstreetmap.org/wiki/Osmosis) command line
tool.

## Setup database

Create a postgres data base by running `createdb osm_gb`. Then log into
it by entering `psql osm_gb`. Now we will run a few SQL commands. \###
Postgis

## Setup environment

On mac, you can set the limit of memory that the osmosis process will
use by running the following command in the terminal window in which you
are running `export JAVACMD_OPTIONS=-Xmx2G`.

## Process data

Run the following commands to extract the data of interest from the raw
file, namely the nodes of the road network and the edges corresponding
to them:

``` bash

# read_pbf script
osmosis --rb 'path_to_raw_osm_file.osm.pbf' \
--lp \
--tf accept-ways highway=* \
--tf reject-relations \
--used-node \
--write-xml 'path_of_processed_osm.xml'

# write into a database
osmosis --read-xml 'path_of_processed_osm.xml' \
--lp \
# optionall filtering out of certain road types
--tf reject-ways highway=motorway,motorway_link \ 
--used-node \
--write-pgsql database='db_name'
```
