# FCC's Census Block Conversions API
# Get Block FIPS for lat/lon
## After: https://gist.githubusercontent.com/ramhiser/f09a71d96a4dec80994c/raw/d3e1d9fc1e7f38b2a402eee3237221fa9a47d1da/latlong2fips.r

latlon2fips <- function(latitude, longitude) {
  url <- "https://geo.fcc.gov/api/census/block/find?latitude=%f&longitude=%f&showall=true&format=json"
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  as.character(json$Block['FIPS'])  #Block FIPS includes state, county, tract & blockgroup FIPS
  
}