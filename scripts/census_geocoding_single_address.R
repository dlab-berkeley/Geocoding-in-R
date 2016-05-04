#
# Single Address Geocoding with the US Census Geocoding Service
#
# pattyf@berkeley.edu, 05/2/2016
#
## Documentation:
# http://geocoding.geo.census.gov/
# https://www.census.gov/geo/maps-data/data/geocoder.html
# http://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf

#clean environment
rm(list=ls())

#Load libraries
library(RJSONIO)
library(plyr)

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

# Identify the URL of the census geocoding service and related parameters
census_prefix <- "http://geocoding.geo.census.gov/geocoder/geographies/address?"
census_suffix <- "&benchmark=Public_AR_Current&vintage=ACS2014_Current&format=json"
# See http://geocoding.geo.census.gov/ for available benchmarks and vintages
# See also: http://www2.census.gov/geo/pdfs/education/brochures/CensusGeocoder.pdf
# for definition of benchmarks and vintages

# Identify the file of addresses that will be geocoded
census_input_addressFile <- "tiger/tiger_12addresses_to_geocode.csv"

# the output file we will create
geocoded_output_file <- "geocoded_addresses_single_out.csv"

# let's take a look at the addresses that we will geocode
addresses_to_geocode <- read.csv(census_input_addressFile, stringsAsFactors = FALSE, col.names = c('id','street','city','state','zip'))

# get the address in the format needed by the Census API GET call
addresses_to_geocode$census_format <- paste0(
  "street=",addresses_to_geocode$street,
  "&city=",addresses_to_geocode$city,
  "&state=",addresses_to_geocode$state,
  "&zip=",addresses_to_geocode$zip
)

census_geocode <- function(address) {
  
  #prepare the address so that it is url request ready
  address <- URLencode(address)
  
  #prepare the full Census Geocoding Request URL
  g_address <- paste0(census_prefix, address,census_suffix)
  
  # create an empty data frame to return
  answer <- data.frame(lat=NA, lon=NA, geoid=NA)
  
  out <- tryCatch(
    {
      # HTTP Requests can hang, fail, etc so we wrap
      # it in a tryCatch() function to handle problematic
      # addresses and keep on going... important with lots of addresses
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      message("Trying to Geocode with Census API")
      
      fromJSON(g_address) # result will be returned if no error
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("Address URL does not seem to exist:", g_address))
      message("Here's the original error message:")
      message(cond)
      # The return value in case of error is the NA data frame row
      return(answer)
    },
    warning=function(cond) {
      message(paste("Address URL caused a warning:", g_address))
      message("Here's the original warning message:")
      message(cond)
      # The return value in case of error is the NA data frame row
      return(answer)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message(paste("Processed Address:", g_address))
    }
  )  
  if (length(out$result$addressMatches) > 0) {
    # if we got a geocoded response
    # update the answer data frame
    answer$lon <- out$result$addressMatches[[1]]$coordinates[['x']]
    answer$lat <- out$result$addressMatches[[1]]$coordinates[['y']]
    answer$geoid <-out$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$GEOID
  } 
  return(answer)
  
}

## apply the geocoding function to the CSV file
#initialise a dataframe to hold the results
geocoded.df <- data.frame()

geocoded.df <- ldply(addresses_to_geocode$census_format,function(x) census_geocode(x))

#append the answer to the results file.
geocoded_addresses <- cbind(addresses_to_geocode, geocoded.df)

# take a look at our geocoded output
head(geocoded_addresses)

# Save geocoded addresses to a file
write.csv(geocoded_addresses, file=geocoded_output_file, row.names=FALSE)
