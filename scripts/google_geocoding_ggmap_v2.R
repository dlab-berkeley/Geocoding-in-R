# NOTE YOU MUST HAVE CURRENT DEV VERSION OF GGMAP
# Install updated version of ggmaps
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup")

library(ggmap)
library(purrr)
setwd("./")

# Replace with your google maps API key - don't share!!! 
register_google("YOUR_GOOGLE_API_KEY_HERE")

# Test geocoding
geocode("san francisco, ca", output="latlona")

# Readin sample data
# Oakland Liquor store subset
# oak_liquor_stores.csv
# Header looks like this:
# id	name	street	city	state	zip	type
sites <- read.csv("oakland_liquor_stores.csv", header = T, stringsAsFactors = F, strip.white = T)

head(sites)

# Create a single column version of the full address
sites$addr <- paste0(sites$street, " ", sites$city, " ",sites$state, " ", sites$zip)

##################################################
# Simple ggmap geocoding
##################################################
# This is the easiest way but it bombs on bad addresses
# and you lose all previous geocodes
# Uncomment to use!
## UNCOMMENT BEGIN BELOW
#geocoded_output_df <- geocode(sites$addr, output = "latlona")
#
#rename the columns
#colnames(geocoded_output_df) <-c("lon","lat","google_address")
#
# Append columns
#new_df <- cbind(sites,geocoded_output_df)
### UNCOMMENT END ABOVE

##################################################
#  ggmap geocoding - with error handling
##################################################
geocode_many <- function(id, addr) {
  # Function to iterate over and geocode a set of addresses and ids with google via ggmap package
  # that will return lat=NA, lon=NA, address = "not found" 
  # if google can't find input address
  # returns a data frame of geocoded addresses
  
  # Create empty data frame for results
  results_df <- data.frame()
  
  for (i in seq_len(length(addr))) {
    print(addr[i])
    
    x<- geocode(addr[i], output="latlona")
    
    if (is.na(x$lat)) {
      x$lat <- NA
      x$lon <- NA
      x$address <- "not found"
    }
    
    temp_df <- data.frame(
      ID = id[i],
      lat = x$lat,
      lon = x$lon,
      google_address = x$address
    )
    
    results_df <- rbind(results_df, temp_df)
  }

  return(results_df)
}

########################################################################
# Test function - assumes you have an id field with the column label id
########################################################################
# test the function
sites2 <- head(sites, 10) #take 10 sites
the_geocodes <- geocode_many(sites2$id, sites2$addr)
sites2 <- merge(sites2, the_geocodes, by.x="id", by.y = "ID", all.x = T)
head(sites2)

#################################################################
# Geocode ALL DATA - you can geocode 2,000 addr per day for free
#################################################################
# geocode the data - **Assumes unique id for each row - in a column labeled id**
the_geocodes <- geocode_many(sites$id, sites$addr)

# merge geocoded output with input data
sites <- merge(sites, the_geocodes, by.x="id", by.y = "ID", all.x = T)

head(sites)

#################################
## Add the FIPS code to each row
##################################

latlon2fips <- function(latitude, longitude) {
  # Return a 15 digit Census Geo identifier (geoid)
  # like this: "060650422121006"
  # 06 = state is first two digits
  # 065 = county digits 3 - 5
  # 42212 = census tract digits 6-11
  # 10 = block group digits 12-13
  # 06 = block digits 14-15
  fips <- ""
  if(is.na(latitude) | is.na(longitude)) { #minor validity checking
    return(fips)
  } else {
    url <- "https://geo.fcc.gov/api/census/block/find?latitude=%f&longitude=%f&showall=true&format=json"
    url <- sprintf(url, latitude, longitude)
    json <- RCurl::getURL(url)
    json <- RJSONIO::fromJSON(json)
    #print(json)
    fips <- as.character(json$Block['FIPS'])  #Block FIPS includes state, county, tract & blockgroup FIPS
    return(fips)
  }
}
# test
latlon2fips(NA,NA)

#sites2$census_geoids <- mapply(latlon2fips,sites2$lat,sites2$lon)

# Census GEOIDS for each lat/lon pair from FCC
sites$census_geoids <- mapply(latlon2fips,sites$lat,sites$lon)

# Write results to file
write.csv(sites, file="geocoded_ouput_data.csv", row.names = F)
#
