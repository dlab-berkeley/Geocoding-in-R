library(plyr)
library(ggmap)
library(readxl)
library(stringr)

setwd("~/Documents/Dlab/consults/leora")

data <- read_excel("doh2.xlsx")
head(data) # take a look

# We need one address not multipart
data$address_long <- with(data,paste(address, city,"CA", zip, sep=","))
head(data)

# Geocode a file of addresses - loaded into data frame
geocoded_output <- geocode(data$address_long, output = "latlona", source = "google")

# check out any warnings
warnings_out <- warnings()
head(warnings_out)

# Add output to input data
geocoded_data <- data.frame(data, geocoded_output)
head(geocoded_data) # check it

# Subset based on what was/was not geocoded
not_geocoded <- subset(geocoded_data, is.na(lat))
yes_geocoded <- subset(geocoded_data, !is.na(lat))
nrow(geocoded_data) == (nrow(not_geocoded) + nrow(yes_geocoded))


# Create a map to check geocoded output
mymap <- get_map(location=c(lon=mean(yes_geocoded$lon), lat=mean(yes_geocoded$lat)), zoom=4)

ggmap(mymap) +
  geom_point(aes(x = lon, y = lat), data=yes_geocoded, size = 2, col="red" ) 

#Scaling up to more than 2500 records?
# Google limits free geocoding to 2500 addresses per day
geocodeQueryCheck() #how am I doing?

# Fix the records that were not geocoded
# because they are out of state
# so remove the ",CA" that we appended above

not_geocoded$add2 <- gsub(',CA,'," ", not_geocoded$address_long)

#DF[ , !(names(DF) %in% drops)]
not_geocoded <- not_geocoded[, !(names(not_geocoded) %in% c('lat','lon','address.1'))]
str(not_geocoded)

# try again
geoout2 <- geocode(not_geocoded$add2, output = "latlona", source = "google")
# check and address any warnings()

# Add output to input data
geocoded2 <- data.frame(not_geocoded, geoout2)
head(geocoded2) # check it

# fix bad ones identified via warnings()
# try again
geoout2 <- geocode(not_geocoded$add2, output = "latlona", source = "google")


# Add output to input data
geocoded2 <- data.frame(not_geocoded, geoout2)
head(geocoded2) # check it

nrow(geocoded_data) == (nrow(geocoded2) + nrow(yes_geocoded))

# Make sure ncols the same and then combine our outputs
# names(yes_geocoded)
# names(geocoded2)
#"primkey"      "address"      "zip"          "city"         "address_long" "add2"         "lon"          "lat"          "address.1" 
geocoded3 <- geocoded2[, !(names(geocoded2) %in% c('address_long'))]
#"primkey"   "address"   "zip"       "city"      "add2"      "lon"       "lat"       "address.1"
newnames <- c("primkey", "address","zip","city","address_long","lon","lat","address.1")
names(geocoded3) <- newnames

all_geocodes <- rbind(yes_geocoded, geocoded3)
nrow(all_geocodes) == nrow(data)
# fix the colnames
newnames2 <- c("primkey", "address","zip","city","geocoded_address","lon","lat","google_address")
names(all_geocodes) <- newnames2


 
#---------------------------------------
# Get FIPS code to link to census data
#---------------------------------------
library(RCurl)
library(RJSONIO)
# FCC's Census Block Conversions API
# http://www.fcc.gov/developers/census-block-conversions-api

latlong2fips <- function(latitude, longitude) {
  # After:  https://gist.github.com/ramhiser/f09a71d96a4dec80994c
  
  thecode <- "none"
  
  if ( !is.numeric(latitude) | !is.numeric(longitude) ) {
    return(thecode)
  }
  if ( is.na(latitude) | is.na(longitude) ) {
    return("NANA")
  }
  url <- "http://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  print(url)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  #{"Block":{"FIPS":"240054114062015"},"County":{"FIPS":"24005","name":"Baltimore"},"State":{"FIPS":"24","code":"MD","name":"Maryland"},"status":"OK","executionTime":"103"}
  #print(json$status)
  if (json$status == "OK") {
    thecode <- as.character(json$Block['FIPS']) # Census 2010 Block FIPS Code
  } 
  return(thecode)
}

na_geocodes <- subset(all_geocodes, is.na(lat))
nrow(na_geocodes)
not_na_geocodes <- subset(all_geocodes, !is.na(lat))
nrow(not_na_geocodes)
nrow(not_na_geocodes) + nrow(na_geocodes) == nrow(all_geocodes)

#all_geocodes$fips<- mapply(latlong2fips,all_geocodes$lat,all_geocodes$lon)
not_na_geocodes$fips<- mapply(latlong2fips,not_na_geocodes$lat,not_na_geocodes$lon)

# take a look
head(all_geocodes)

# add a fips code to the 2 recs without lat/lon
na_geocodes$fips <- "none"

#combine the two data frames
geocodes_w_fips <- rbind(not_na_geocodes, na_geocodes)

# FIPS CODES
# digits 1-2: state
# digits 3-5: county
# digits 6-11: tract
# digits 12-13: blockgroup
# digits 14-15: blockgroup block
geocodes_w_fips$fips_tract <- substr(geocodes_w_fips$fips,1,11)
geocodes_w_fips$fips_tract_only <- substr(geocodes_w_fips$fips,6,11)

# Save geocoded output to a file
# Need to quote output so leading zeros in fips codes preserved
write.table(geocodes_w_fips,file="google_geocoded_output.csv", row.names=FALSE, quote = T, sep=",")
