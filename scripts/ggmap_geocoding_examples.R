#
# Geocoding with GGMAP & the Google Geocoding Service
# examples
#

#--------------------------------------
# libraries - 
# that may be needed for this tutorial 
# and how to install & load them:
#--------------------------------------
required.pkg <- c("htmlwidgets", "leaflet", "ggmap", "ggplot2","httr","acs","RJSONIO","RCurl","stringr","plyr", "rgeos","rgdal", "sp")
pkgs.not.installed <- required.pkg[!sapply(required.pkg, function(p) require(p, character.only=T))]
install.packages(pkgs.not.installed, dependencies=TRUE)

# Load all libraries them all at once.
lapply(required.pkg, library, character.only = TRUE) 

#-------------------------------------------------------
# Exploring Google Geocoder with ggmap package
#-------------------------------------------------------
library(ggplot2)
library(ggmap)

geocode("Barrows Hall, Berkeley, CA", source="google")


geocode("Barrows Hall, Berkeley, CA", source="google", output="latlon")
geocode("Barrows Hall, Berkeley, CA", source="google", output="latlona")
geocode("Barrows Hall, Berkeley, CA", source="google", output="more")
geocode("Barrows Hall, Berkeley, CA", source="google", output="all")

df <- data.frame(
  address = c(
    "1517 Shattuck Ave, Berkeley, CA 94709", 
    "Barrows Hall, Berkeley, CA", 
    "2332 Haste St, Berkeley, CA 94704"
  ),
  stringsAsFactors = FALSE
)

df

## Geocode the three Addresses

df2 <- geocode(df$address,source="google", output="more")

# just keep lat, lon, type, and loctype
df2 <- df2[,c(1:4)]

#look at output
df2

df3 <- data.frame(df,df2)

#look at output
df3

# Create a map of the geocoded output
map <- get_map(location=c(lon=mean(df3$lon), lat=mean(df3$lat)), zoom=14)
ggmap(map) +
  geom_point(aes(x = lon, y = lat), data=df3, size = 6, col="red" ) 

##--------------------------------
## Geocode a file of addresses
##--------------------------------
# get the input data
data <- read.csv(file="address_data/formatted/oak_liq_w_ids_types_headers.csv",stringsAsFactors=F)
head(data)

## We need one column with address (not multiple)
data$address <- with(data,paste(street,city,state,zip, sep=","))
head(data)

## Odd address formats can be a problem
data[19,8]
geocode(data[19,8], source="google", output="latlona")

# Fix that address by removing the "#D"
data[19,8]<-"7305 Edgewater Dr,Oakland,CA,94621"
data[19,8]
geocode(data[19,8], source="google", output="latlona")

# Geocode a file of addresses - loaded into data frame
geocoded_output <- geocode(data$address, output = "latlona", 
                           source = "google")

# Add output to input data
geocoded_output <- data.frame(data, geocoded_output)

head(geocoded_output) # check it

#Scaling up to more than 2500 records?
# Google limits free geocoding to 2500 addresses per day
geocodeQueryCheck() #how am I doing?

#---------------------------------------
# Get FIPS code to link to census data
#---------------------------------------
library(RCurl)
library(RJSONIO)
# FCC's Census Block Conversions API
# http://www.fcc.gov/developers/census-block-conversions-api

latlong2fips <- function(latitude, longitude) {
  #Source: https://gist.github.com/ramhiser/f09a71d96a4dec80994c
  url <- "http://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  print(url)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  as.character(json$Block['FIPS']) # Census 2010 Block FIPS Code
}

geocoded_output$fips<- mapply(latlong2fips,geocoded_output$lat,geocoded_output$lon)

# take a look
head(geocoded_output)

# Save geocoded output to a file
write.csv(geocoded_output,file="google_geocoded_output.csv", row.names=FALSE)

