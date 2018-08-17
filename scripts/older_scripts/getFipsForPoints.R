## ############################################################################
#
# Joining Census FIPS codes to points via point in polygon overlay
# 
# Author: Patty Frontiera pfrontiera@berkeley.edu 
# Date: 2014_04_15
# Last updated: 2014_04_17
#
# Purpose:
## This script takes as input a point file and a file of Census Tiger data
## performs a spatial overlay of the two 
## and adds the Census FIPS code to the point file attribute table
## Then writes the output to CSV
##
# #ASSUMPTION: input point data has fields "latitude" and "longitude"
##
## This same practice can be done much faster in ArcGIS or PostGIS
## but this method is fine for < 1 million records
## 
## This approach will work with on an offline server provided
## the data, R code and R libraries reside on the server.
##
## THIS IS SAMPLE CODE - you will need to make changes!
## ############################################################################

# clean start - remove any objects in the enviroment
rm(list = ls())

#LOAD LIBS
require(rgdal)
require(R.utils)

# ########################################################################################################
# USER SUPPLIED VALUES
# ########################################################################################################

working_dir <- "/Users/pattyf/geocoding/temp"

point_file <- "/Users/pattyf/geocoding/test_address_points.csv"
# You can download some sample point data from this url:
## https://gist.githubusercontent.com/pattyf/9091aca4d536e983beea/raw/65b4ca99a215b65cdd7c2406dfbac9749eb897f6/test_address_points

point_file_delimiter <- "|"  # I prefer this delimiter to a comma as address components often contain commas

point_file_crs <- "4326"  # These points use geographic coordinates with the WGS84 datum
# WGS 84 - coordinate reference system (crs) used by most GPS / Google maps etc
## AKA - spatial reference system or map projection or coordinate system
## See spatialreference.org - http://spatialreference.org/ref/epsg/4326/

# HEY: IMPORTANT
#ASSUMPTION: input point data has fields "latitude" and "longitude"

## Census block data - must point to file on your computer
#census_file <- '/Users/pattyf/Gisdata/Census/tabblock2010_06_pophu/tabblock2010_06_pophu.shp'

## Census blockgroup data - must point to file on your computer

#census_file <- '/Users/pattyf/Gisdata/Census/tl_2014_06_tract/tl_2014_06_bg.shp'

## Census tract data - must point to file on your computer
census_file <- '/Users/pattyf/Gisdata/Census/tl_2014_06_tract/tl_2014_06_tract.shp'
# CA block-level census data were downloaded from the census website, url below:
## http://www2.census.gov/geo/tiger/TIGER2014/TABBLOCK/tl_2014_06_tabblock10.zip
## Could automate the download but adds unneeded complexity to this script
## THIS IS BIG FILE = 415MB or so  
## You can download a smaller file by downloading larger census geographies 
### eg tracts level data
## http://www2.census.gov/geo/tiger/TIGER2014/TRACT/tl_2014_06_tract.zip
### or block group level data
### http://www2.census.gov/geo/tiger/TIGER2014/BG/tl_2014_06_bg.zip
## However, if you intersect points with the block level data 
## you get a FIPS CODE that includes the state, county, tract, blockgroup and block id
## You need to change this file if not doing CA 
## or if you want to change the input remote census data file, eg to smaller file like tracts
## See http://www2.census.gov for details
## Note there are several vintages (year versions) for each census products. For
## tracts, block groups, and blocks these don't change between census - there are only improvements/corrections
## If you are interested in comparisons over time (eg 2000 - 2010 census) get the harmonized data from NHGIS

#census_layer <- 'tabblock2010_06_pophu'  # The layer is the name of the feature layer within the file
# For shapefiles it is the same as the prefix of the shapefile

#census_layer <- 'tl_2014_06_bg'  #census blockgroup level data

census_layer <- 'tl_2014_06_tract' #census tract data

census_crs <- '4269'  # US Census Tigerline data use geographic coordintes with the NAD83 datum
# The EPGS code for which is 4269
# See http://spatialreference.org/ref/sr-org/4269/ for details.

census_geograhpy_type = "tracts" # one of tracts, blocks, or blockgroups

output_crs <- '4326' #WGS84
## USE '3310' for CA Teale Albers  - See http://spatialreference.org/ref/epsg/3310/
## Used for CA state-wide data processing (metric calculations)
## If the output CRS does not match the census CRS we will
## transform the data before saving to new file as last step

out_csv_file <-"point_data_withfips.csv" # The name of the output csv file
# Will be written to working_dir if full path  not specified

out_shapefile_prefix <- "point_data_withfips"
out_shapefile_directory <- "."  # The period indicates the current working dir.
# You can specify another directory as needed

debug <- 1  # We are just testing this script if debug is 1. If running for real, set this to 0
# When debug is 1 we only read in first 50 records from point file
# ########################################################################################################

# Load needed libraries
library(sp)
library(rgdal)
library("R.utils") # for file utils, like zip and unzipping files

# Set working directory for input and output where full path not given
setwd(working_dir)

# Read in point data
## In this exampe we have geocoded addresses - 355,054 addresses all in alameda county (would prefer a state sample)
## Format of these address data points in input file:
## Inaddress|street_address|street_name|latitude|country_code|fips_county|country_name|country_code3|longitude|region|locality|street_number|confidence|

if (debug == 1) {
  # When debug is 1 only read in first 50 records from point file
  point_data<-read.table(point_file,sep=point_file_delimiter, header=T, stringsAsFactors=,nrow=50)
} else {
  point_data<-read.table(point_file,sep=point_file_delimiter, header=T, stringsAsFactors=F)
}

# Convert data frame to a spatialpoints data frame object
coordinates(point_data) =~ longitude+latitude 

# Specify the CRS of the input point data
proj4string(point_data) = CRS(paste0("+init=epsg:",point_file_crs))

# Read the census block data into R
census_polys <- readOGR(census_file,census_layer)

# Specify the CRS of the input census data
proj4string(census_polys) = CRS(paste0("+init=epsg:",census_crs)) # define the projection

# CRS of both layers must match!
## If they do not then the point data should be transformed
## as it is much easier operation on points than polygons
if (point_file_crs != census_crs) {
  point_data <- spTransform(point_data,CRS(paste0("+init=epsg:",census_crs)))
}

#
# Spatial Intersection 
## Get fips code for each address point
## The block key from this dataset is col 5, which has the name BLOCKID10 (census 2010 block id)
ptm <- proc.time()  # Time this operation to get a sense of how it will scale to more points
if (census_geograhpy_type == "blocks") {
  point_data$fips_code <- over(point_data,census_polys)$BLOCKID10
}  
if (census_geograhpy_type == "tracts") {
  point_data$fips_code <- over(point_data,census_polys)$GEOID
}  
if (census_geograhpy_type == "blockgroups") {
  point_data$fips_code <- over(point_data,census_polys)$GEOID
}  

print(proc.time() - ptm)

# ###############################################
# Notes on output from testing
# ###############################################
## It took 18 minutes to intersect ~350,000 address points
## with census block-level data
##
##    user   system  elapsed 
## 1049.953   18.078 1072.092 
##
# How long does this operation take in ArcGIS? 
## on our geocoding server it took only
## 2 minutes using spatial intersect operation.
# ###############################################


# ###############################################
# Transform the data before saving if needed
# ###############################################
if (output_crs != census_crs) {
  point_data <- spTransform(point_data,CRS(paste0("+init=epsg:",output_crs)))
}


# ###############################################
# Save output to local files
# ###############################################
#
## as csv
write.csv(point_data@data,out_csv_file,row.names=FALSE)
#
## as shapefile
### note that field/col names longer than 8 characters will be truncated!
writeOGR(point_data, out_shapefile_directory, "out_shapefile_prefix", driver="ESRI Shapefile",overwrite_layer=TRUE)