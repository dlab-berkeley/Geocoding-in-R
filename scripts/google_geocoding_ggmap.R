#
# Using GGMAP to geocode with the Google Geocoding API
# last updated 08/16/2018
#
# Notes:
# 1. Read the docs at: https://developers.google.com/maps/documentation/geocoding/start
# 2. As of July 2018 you may need to register for a new Google Geocoding API key
#    and associate it with a credit card.
#    The documentation indicates you get $200 of free Google API access per month.
#    That would be 40,000 free geocodes per month if that were the only thing you used it for.
#    So - use with care, protect your API keys so others don't use them.
#

library(ggmap)

setwd("~/Documents/Dlab/workshops/2018/RGeocoding")

#mykey <- "AIzaSyxxxxxxxxxxxxxxxxxxxxxxxxxOQyOFWrTw"

register_google(key=mykey)

# Geocode a city
geocode("San Francisco, CA")

# Geocode a state
geocode("California")

# Geocode a landmark
Geocode("Golden Gate Bridge")

# Reverse Geocode
revgeocode(c(-122.4194,37.77493), output="more")

# Geocode a data frame of addresses
address_data <- read.csv("address_data/formatted/oak_liq_w_ids_types_headers.csv")

# Full addres format: 100 Bolyston St, Boston, MA 01952
address_data$full_address <- paste0(address_data$street, ", " , address_data$city, ", " , address_data$state, " ", address_data$zip)

#?geocode
# See the google documentation to interpret all of the results
geocoded_output <- geocode(address_data$full_address, output = "more", source = "google", key=mykey)


