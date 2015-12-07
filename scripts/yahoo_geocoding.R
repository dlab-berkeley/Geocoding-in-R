# Geocoding with Yahoo Placefinder
# 2000 addresses per day limit

#following zev ross blog
# http://zevross.com/blog/2015/05/19/scrape-website-data-with-the-new-r-package-rvest/
#You need to first apply for an account on https://developer.yahoo.com

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

# If you haven't already installed rydn get it now using devtools
# devtools::install_github("trestletech/rydn")
library(rydn)

# readin your keys from YDN (don't share)
source("keys/ydn_keys.R")
#mykey="dj0.......................00Zg--"
#mysecret ="00....................8"

#test it
myloc <- find_place("Barrows Hall, Berkeley,ca",key=mykey,secret=mysecret)
myloc #see what was returned

# TO interpret response see: https://developer.yahoo.com/boss/geo/docs/supported_responses.html

#convert strings to numerics
myloc$longitude <- as.numeric(myloc$longitude)
myloc$latitude <- as.numeric(myloc$latitude)

# work with subset of the returned info
myloc_sub <- myloc[1 ,c("quality",  "latitude", "longitude", "radius")] #subset

#lets plot it
library(ggplot2)
library(ggmap)

map <- get_map(location=c(lon=as.numeric(myloc_sub$longitude),lat=as.numeric(myloc_sub$latitude)), zoom=17)
ggmap(map) +
  geom_point(aes(x = longitude, y = latitude), size = 6, col="red", data = myloc)  


# lets plot it interactively via rleaflet
