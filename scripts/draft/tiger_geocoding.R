#
# Geocoding with Tiger Geocoding Service
#

#clean environment
rm(list=ls())

#Load libraries
library(curl)

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

# CLI format for CURL
#format of geocoding request for Tiger Geocoder
#curl --form addressFile=@tiger_12addresses_to_geocode.csv --form benchmark=Public_AR_Census2010 --form vintage=Census2010_Census2010 http://geocoding.geo.census.gov/geocoder/geographies/addressbatch
### or this if saving to file as indicated by -o flag
#curl --form addressFile=@tiger_12addresses_to_geocode.csv --form benchmark=Public_AR_Census2010 --form vintage=Census2010_Census2010 http://geocoding.geo.census.gov/geocoder/geographies/addressbatch
##  -o geocoded_addresses_with_fips.csv

tiger_input_addressFile <- "tiger/tiger_12addresses_to_geocode.csv"

tiger_url_prefix <- "http://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

tiger_url_options <- "--form benchmark=Public_AR_Census2010 --form vintage=Census2010_Census2010"

tiger_url_address_options <- paste0("--form addressFile=@", tiger_input_addressFile)

tiger_url <- paste(tiger_url_prefix, tiger_url_options, tiger_url_address_options)

# worked: 
# curl --form addressFile=@tiger/tiger_12addresses_to_geocode.csv --form benchmark=Public_AR_Census2010 --form vintage=Census2010_Census2010 http://geocoding.geo.census.gov/geocoder/geographies/addressbatch
##curl http://geocoding.geo.census.gov/geocoder/geographies/addressbatch 
##--form addressFile=@tiger/tiger_12addresses_to_geocode.csv 
##--form benchmark=Public_AR_Census2010 
##--form vintage=Census2010_Census2010 


library(httr)

## WORKED - thanks to: http://stackoverflow.com/questions/26611289/curl-post-statement-to-rcurl-or-httr
## add verbose() to see details of process 
## Using httr I think
geocoded_addresses <- POST(tiger_url_prefix, encode="multipart", 
          body=list(addressFile=upload_file(tiger_input_addressFile), 
                    benchmark="Public_AR_Census2010",
                    vintage="Census2010_Census2010"
                    )
        )

#write raw output to file
capture.output(cat(content(geocoded_addresses)), file="test_out.txt")
#read output file in to a data frame (not sure how to do these two in one step)
mylocs <- read.csv("test_out.txt",header=FALSE)
head(mylocs)

#doh split the lat,long values into two separate columns
mylocs$lon = unlist(lapply(mylocs$V6, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][1]))
mylocs$lat = unlist(lapply(mylocs$V6, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][2]))

 