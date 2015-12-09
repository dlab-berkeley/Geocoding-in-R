# geocoding with ggmap
## http://zevross.com/blog/2014/03/19/geocoding-with-rs-ggmap-package/
## 
### Clear workspace
rm(list=ls())

library(ggmap)

### Set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

#Try 1 address
geocode(location="7305 Edgewater Dr #D Oakland CA 94621", source="google")
geocode(location="7305 Edgewater Dr Oakland CA 94621", source="google")
geocode(location="7305 Edgewater Dr Oakland CA 94621", source="google", output="latlona")
geocode(location="7305 Edgewater Dr Oakland CA 94621", source="google", output="more")
#try 
?geocode

# Geocoding script for large list of addresses.
# get the input data
data <- read.csv(file="address_data/formatted/oak_liq_w_ids_types_headers.csv",stringsAsFactors=F)
head(data)
data$address <- with(data,paste(street,city,state,zip, sep=" "))
head(data)

# data[19,8]<-"7305 Edgewater Dr Oakland CA 94621"  ## Why do we need to do this??

mylocs <- geocode(data[,8], output = "latlona", source = "google")
head(mylocs)
# if you want more output try
#geocoded_output <- geocode(data[,3], output = "more")
#write.csv(geocoded_output,file="geocoded_output.csv", row.names=FALSE)

#lets plot it
library(ggplot2)
library(ggmap)

map <- get_map(location=c(lon=mean(mylocs$lon), lat=mean(mylocs$lat)))
ggmap(map) +
  geom_point(aes(x = lon, y = lat), data = mylocs, size = 6, col="red" )  


## TRY - what's different?
map <- get_map(location=c(lon=mean(mylocs$lon), lat=mean(mylocs$lat)), zoom=13)
ggmap(map) +
  geom_point(aes(x = lon, y = lat), data = mylocs, size = 6, col="red" ) 


#TRY


#try 
?geocode
