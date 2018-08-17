#
# Geocoding with Tiger Geocoding Service
#
# pattyf@berkeley.edu, 05/2/2016
#
# Important note:
## You can only geocode 1000 addresses at a time
## so need to add code to loop or subset your files
##
## Documentation:
## 

#clean environment
rm(list=ls())

#Load libraries
library(httr)

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

# CLI format for CURL
#format of geocoding request for Tiger Geocoder
##curl http://geocoding.geo.census.gov/geocoder/geographies/addressbatch --form addressFile=@tiger/tiger_12addresses_to_geocode.csv --form benchmark=Public_AR_Census2010 --form vintage=Census2010_Census2010 
##
##curl http://geocoding.geo.census.gov/geocoder/geographies/addressbatch
##--form addressFile=@tiger/tiger_12addresses_to_geocode.csv 
##--form benchmark=Public_AR_Census2010 
##--form vintage=Census2010_Census2010
##-o output_file.csv

tiger_input_addressFile <- "tiger/tiger_12addresses_to_geocode.csv"

# let's take a look at the addresses that we will geocode
addresses_to_geocode <- read.csv(tiger_input_addressFile, stringsAsFactors = FALSE, col.names = c('id','street','city','state','zip'))

tiger_url <- "http://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

## WORKED - thanks to: http://stackoverflow.com/questions/26611289/curl-post-statement-to-rcurl-or-httr
## add verbose() to see details of process 
## Using httr I think
geocoded_addresses <- POST(tiger_url, encode="multipart", 
                           body=list(addressFile=upload_file(tiger_input_addressFile), 
                                     benchmark="Public_AR_Census2010",
                                     vintage="Census2010_Census2010"
                           )
)

#write raw output to file
capture.output(cat(content(geocoded_addresses)), file="test_out3.txt")

#read output file in to a data frame (not sure how to do these two in one step)
mylocs <- read.csv("test_out3.txt",header=FALSE)
head(mylocs)

#doh split the lat,long values into two separate columns
mylocs$lon = unlist(lapply(mylocs$V6, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][1]))
mylocs$lat = unlist(lapply(mylocs$V6, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][2]))

mylocs$lon <- as.numeric(mylocs$lon)
mylocs$lat <- as.numeric(mylocs$lat)

#lets plot it
library(ggplot2)
library(ggmap)

map <- get_map(location=c(lon=mean(mylocs$lon),lat=mean(mylocs$lat)), zoom=15)
ggmap(map) +
  geom_point(aes(x = lon, y = lat), size = 4, col="red", data = mylocs)  

