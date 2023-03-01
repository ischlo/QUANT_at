
import numpy as np
import pandas as pd
import osmnx as ox
import geopandas as gpd

# 
# import networkx as nx
# from shapely.geometry import MultiPolygon
# from shapely.geometry import Polygon
# 
# from osmnx import distance
# from osmnx import downloader
# from osmnx import geocoder
# from osmnx import osm_xml
# from osmnx import projection
# from osmnx import settings
# from osmnx import simplification
# from osmnx import stats
# from osmnx import truncate
# from osmnx import utils
# from osmnx import utils_geo
# from osmnx import utils_graph

# import gc

ox.config(timeout=9000)

def ready_to_export(data):
    ''' data is blablabla '''
    
    keep = ['element_type','osmid','geometry','highway','bicycle','foot','cycleway'
            #,'pedestrian'
           ]
    
    data = data.reset_index()
    data = data[keep]
    data= data.astype({'highway':str
                    , 'bicycle':str
                    , 'foot':str
                    , 'cycleway':str
                    #, 'pedestrian': str
                      })
    
    return data
  

# we can calculate basic street network metrics and display average circuity
# stats = ox.basic_stats(haringey_ox)
# stats

# getting the data by tag for highways that can accomodate cycling, 
# from osm tags, those are: 

# highway=*

# tags_cycle = {
#   "highway" : ["cycleway","path","primary","secondary","tertiary","unclassified"
#                 ,"residential","primary_link","secondary_link","tertiary_link"
#                 ,"footway"]
#   ,"cycleway": True
#   ,"bicycle" : ["yes", "designated","permissive","destination","dismount"]
#   ,"surface":True
# }
# 
# # tags for the general road network containing all segments
# tags_highway = {
#   "highway" : True
# }
# 
# # tags for walkable segments
# # add tags from map features OSM
# tags_walk = {
#   "highway" : ["pedestrian"
#                ,"footway"
#                ,"path"
#                ,"cycleway"
#                ,"steps"
#                ,"unclassified"
#                ,"residential"
#                ,"living_street"
#                ,"track"
#                ,"footway"
#                ,"bridleway"
#                ,"steps"
#                ,"corridor"
#                ,"path"
#               ]
#   #,"foot" : ["yes", "designated","permissive"]
# }

area_custom_graph = ox.graph_from_place("Cambridgeshire, UK"
                                  ,network_type="all"
                                  ,buffer_dist = 5000
                                  ,simplify = True
                                  ,truncate_by_edge = True
                                  )

ox.save_graph_geopackage(area_custom_graph, "cambridgeshire_network.gpkg")

# area_custom_graph = ox.graph_from_bbox(58.620
#                                     ,55.292
#                                     ,-1.736
#                                     ,-6.372
#                                     ,network_type="all"
#                                     ,simplify = True
#                                     ,truncate_by_edge = True
#                                     ,clean_periphery = True
#                                     )

# ox.save_graph_geopackage(area_custom_graph, "custom_graph.gpkg")

xml_path = "/Users/ischlo/Documents/CASA/data/osmosis_network/reunion.osm"

reunion_ox_graph = ox.graph_from_xml(xml_path)

ox.save_graph_geopackage(reunion_ox_graph, "reunion_network.gpkg")
