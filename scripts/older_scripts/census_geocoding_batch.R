#
# Batch Geocoding with the US Census Geocoding Service
#
# pattyf@berkeley.edu, 05/2/2016
#
## Documentation:
# http://geocoding.geo.census.gov/
# https://www.census.gov/geo/maps-data/data/geocoder.html
# http://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf

#Load libraries
library(httr) # to submit geocoding request
library(ggplot2) # to plot output
library(ggmap) # to plot output
library(leaflet) # for interactive plotting
library(stringr) # to format strings

#clean environment
rm(list=ls())

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

# our file of addresses that need to be geocoded
tiger_input_addressFile <- "tiger/tiger_12addresses_to_geocode.csv"

# the output file we will create
geocoded_output_file <- "geocoded_addresses_out.csv"

# The census geocoder can take as input a file of addresses to be geocode.
# This file can contain up to 1000 addresses.
# The census geocoder does not want column names in the file to be geocoded,
# but we want to add them when we read the data into R to make sense of the data.
#
# For info on the correct format for submitting a file of addresses see:
# https://www.census.gov/geo/maps-data/data/geocoder.html
# Five columns - No headers, comma separated EVEN IF DATA NOT AVAILABLE
# Unique ID, house number and street name, city, state, zipcode
# Two valid examples:
#1, 1600 Pennsylvania Ave NW, Washington, DC,
#2, 1600 Pennsylvania Ave NW,,,20502

# Read in the address that we will geocode
# note in this example we are adding column names but they are not in the file
addresses_to_geocode <- read.csv(tiger_input_addressFile, stringsAsFactors = FALSE, col.names = c('id','street','city','state','zip'))

# Look at the data
head(addresses_to_geocode)

# How many addresses?
num_addresses <- nrow(addresses_to_geocode)

# Now that we have looked at the data we are ready to geocode.
# First, remove the address data object
rm(addresses_to_geocode)


get_geocoded_addresses <- function(file_of_addresses) {
    # Create a function to submit a file of addresses to the census geocoder.
    
    # Identify the URL to which we will submit the geocoding request
    tiger_url <- "http://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
    
    # Make the request to the Census geocoding service
    # and store the results in the geocoded_addresses data frame
    #
    # The important parameters here are benchmark and vintage.
    # You can read about these in: http://www2.census.gov/geo/pdfs/education/brochures/CensusGeocoder.pdf
    # Or at: http://geocoding.geo.census.gov/ 
    # The benchmark is the date the data were last updated and the vintage is the year of the census data product it links to.
    # below we are querying the street database with the most current data for linking to the 2014 ACS data
    geocoded_addresses <- POST(tiger_url, encode="multipart", 
                           body=list(addressFile=upload_file(file_of_addresses), 
                                     benchmark="Public_AR_Current",
                                     vintage="ACS2014_Current"
                           )
    )
    
    
    # We need to reformat the data that was returned by the Census Geocoder
    # First, create a temporary file to store the geocoded address data  # create temp file
    mytempfile <- tempfile()
    
    #Write raw geocoded output to tempfile
    capture.output(cat(content(geocoded_addresses)), file=mytempfile)
    
    # Relable the output column names
    mycols <- c("id","in_address","match_status","match_type","matched_address","lon_lat","tlid","street_side", "state_fips", "county_fips","tract_fips", "block_fips")
    
    # Read the data back into a data frame from the temp file and use the new column names
    mylocs <- read.csv(mytempfile,header=FALSE, col.names = mycols)
    
    # Delete that temp file
    unlink(mytempfile)
    
    # The latitude and longitude coordinates for the geocoded addresses are in one column.
    # Split the lat,long values into two separate columns:
    mylocs$lon = unlist(lapply(mylocs$lon_lat, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][1]))
    mylocs$lat = unlist(lapply(mylocs$lon_lat, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][2]))
    
    # Make sure the lat and lon values are numeric
    mylocs$lon <- as.numeric(mylocs$lon)
    mylocs$lat <- as.numeric(mylocs$lat)
    
    # Finally, save the geocoded addresses to a file
    if (processed_rows == 0) {
      # If we only processed one file or the first of many, create a new file
      # create and write to the file
      write.csv(mylocs,file=geocoded_output_file, row.names=FALSE)
    } else {
      # Append to the file
      write.csv(mylocs,file=geocoded_output_file, row.names=FALSE, append=TRUE)
    }
    
    # Return the data frame of geocoded addresses
    return(mylocs)
}

# some counters to keep track of the number of addresses we need to process
# we can only batch geocode 1000 addresses at a time
read_rows <- 1000 
processed_rows <- 0

# Now that we have our function to submit addresses to the Census geocoder we can proceed
if (num_addresses < 1000) {
  # If the number of addresses to geocode is less than 1000, just go ahead and geocode them
  geocoded_df <- get_geocoded_addresses(tiger_input_addressFile)
  
} else {
  # Process 1000 addresses at a time
  # By reading in up to 1000 addresses to geocode from our address file
  # saving them to a file, geocoding that file
  # then write the results to our master geoceded addresses file

  while (processed_rows < num_addresses) {
    # if we still have addresses to geocode
    # read in the next 1000 from the file of addresses to be geocoded
    addresses_to_geocode <- read.csv(tiger_input_addressFile, stringsAsFactors = FALSE, nrows=read_rows, skip=processed_rows)
    
    # create a temporary file to hold the up to 1000 addresses to geocode
    temp_infile <- tempfile()
    
    # Save the up to 1000 addresses to geocode to a file
    write.csv(addresses_to_geocode,file=temp_infile, row.names=FALSE, col.names = FALSE)
    
    # geocode the batch of 1000 addresses in the temp file
    geocoded_df <- get_geocoded_addresses(temp_infile)
    
    # delete the temp file
    unlink(temp_infile)
    
    # increment the counters to see if there are more addresses to geocode
    processed_rows <- processed_rows + read_rows
  }
}

# We have now geocoded all of the addresses and saved them to a file.
# Let's read in the file of geocoded address
# and Plot them on a map using ggmap
# read in geocoded addresses
geocoded_results <- read.csv(geocoded_output_file,stringsAsFactors = FALSE)
head(geocoded_results) # take a look at the results

map <- get_map(location=c(lon=mean(geocoded_results$lon),lat=mean(geocoded_results$lat)), zoom=15)
ggmap(map) +
  geom_point(aes(x = lon, y = lat), size = 4, col="red", data = geocoded_results)  

# save the map image to a file
ggsave("mymap.png") 

# Now create an interactive map with Leaflet
map1 <- leaflet() %>% addTiles() %>%
  addCircleMarkers(data = geocoded_results, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste("<strong>Geocoded Address:</strong><br>", geocoded_results$matched_address),
                   color = "red", 
                   fillOpacity = 0.7)

map1 # view it

# You can save leaflet map to html file
# So that you can open and view it anytime
library(htmlwidgets)
saveWidget(map1, file="map1.html", selfcontained=FALSE)

