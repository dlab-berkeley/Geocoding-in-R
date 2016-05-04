#library(httr)
library(RJSONIO)
gurl <- "http://geocoding.geo.census.gov/geocoder/geographies/address?street=912+Kingston+Ave&city=Piedmont&state=CA&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&format=json"

bad_gurl <-"http://geocoding.geo.census.gov/geocoder/geographies/address?street=912+Kingston+Ave&city=donkey&state=CA&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&format=json"

tiger_prefix <- "http://geocoding.geo.census.gov/geocoder/geographies/address?"
tiger_suffix <- "&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&format=json"

#g_out <- GET(gurl)


g_out <- fromJSON(gurl)
str(g_out)

# take the first returned values in case > 1 matches
lon <- g_out$result$addressMatches[[1]]$coordinates[['x']]
lat <- g_out$result$addressMatches[[1]]$coordinates[['y']]
matchedAddress <- g_out$result$addressMatches[[1]]$matchedAddress
tractfips <- g_out$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$GEOID
blockfips <- g_out$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$GEOID

# another way
g_out2 <- unlist(g_out)
head(g_out2)
g_out2['result.addressMatches.coordinates.x']
#Now process a file of addresses:
tiger_input_addressFile <- "tiger/tiger_12addresses_to_geocode.csv"
# let's take a look at the addresses that we will geocode
addresses_to_geocode <- read.csv(tiger_input_addressFile, stringsAsFactors = FALSE, col.names = c('id','street','city','state','zip'))

addresses_to_geocode

addresses_to_geocode$tiger_format <- paste0(
  "street=",addresses_to_geocode$street,
  "&city=",addresses_to_geocode$city,
  "&state=",addresses_to_geocode$state,
  "&zip=",addresses_to_geocode$zip
                            )

# geocode a file of addresses - one at at time
tgeocode <- function(address){
  address <- URLencode(address)
  g_address <- paste0(tiger_prefix, address,tiger_suffix)
  print(g_address)
  
  
  g_out <- tryCatch(
    fromJSON(g_address) # result will be returned if no error
    
  )
  if (length(g_out$result$addressMatches) > 0) {
    print(g_out$result$addressMatches[[1]]$matchedAddress)
  } else{
    #no results
  }
}

## apply the geocoding function to the CSV file
library(plyr)
ldply(addresses_to_geocode$tiger_format,function(x) tgeocode(x))
#address <- c("The White House, Washington, DC","The Capitol, Washington, DC")
#locations  <- ldply(address, function(x) geoCode(x))
#names(locations)  <- c("lat","lon","location_type", "formatted")
#head(locations)

