
# This script provides an example of accessing the ESRI world geocoding service.

# This code borrows HEAVILY (almost completely) and with thanks from
# Claudia Engel: https://github.com/cengel/ArcGIS_geocoding
# See her repo for more details and other examples

##################################
## Single Line Geocode Function ##
##################################
# The function takes:
# - one address at a time as one string (SingleLine)
# - token - which you get from developers.arcgis.com
#           see: https://developers.arcgis.com/rest/geocode/api-reference/geocoding-authenticate-a-request.htm
# - TRUE/FALSE - allow to return Postal codes if a full street address match cannot be found (default is TRUE)
#
# The function returns:
# lon, lat -    The primary x/y coordinates of the address returned by the geocoding service in WGS84 
# score -       The accuracy of the address match between 0 and 100.
# locName -     The component locator used to return a particular match result
# status -      Whether a batch geocode request results in a match (M), tie (T), or unmatch (U)
# matchAddr -   Complete address returned for the geocode request.
# side -        The side of the street where an address resides relative to the direction 
#               of feature digitization
# addressType - The match level for a geocode request. "PointAddress" is typically the 
#               most spatially accurate match level. "StreetAddress" differs from PointAddress 
#               because the house number is interpolated from a range of numbers. "StreetName" is similar,
#               but without the house number.

geocodeSL <- function (address, token, postal = TRUE){
  require(httr)
  
  # ESRI geolocator
  gserver <-"http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/geocodeAddresses"
             
  # template for SingleLine format
  pref <- "{'records':[{'attributes':{'OBJECTID':1,'SingleLine':'"
  suff <- "'}}]}"
  
  # url
  url <- URLencode(paste0(gserver, "?addresses=", pref, address, suff, "&token=", token, ifelse(postal, "&f=json", "&f=json&category=Address")))
  
  # submit
  rawdata <- GET(url)
  
  # parse JSON and process result
  res <- content(rawdata, "parsed", "application/json")
  resdf <- with(res$locations[[1]], {data.frame(lon = as.numeric(location$x),
                                                lat = as.numeric(location$y),
                                                score = score, 
                                                locName = attributes$Loc_name,
                                                status = attributes$Status,
                                                matchAddr = attributes$Match_addr,
                                                side = attributes$Side,
                                                addressType = attributes$Addr_type)})
  return(resdf)
}

#######################################
## Multi Line Batch Geocode Function ##
#######################################
# The function takes:
# - ID variable to identify records, must be numeric and should be unique
# - multiple addresses as vectors, separated into: Street, City, State, Zip
# - token - which you get from developers.arcgis.com
#           see: https://developers.arcgis.com/rest/geocode/api-reference/geocoding-authenticate-a-request.htm
#
# It can take a maximum of 1000 addresses. If more, it returns an error.
#
# The function returns a data frame with the following fields:
# ID -          Result ID can be used to join the output fields in the response to the attributes 
#               in the original address table.
# lon, lat -    The primary x/y coordinates of the address returned by the geocoding service in WGS84 
# score -       The accuracy of the address match between 0 and 100.
# locName -     The component locator used to return a particular match result
# status -      Whether a batch geocode request results in a match (M), tie (T), or unmatch (U)
# matchAddr -   Complete address returned for the geocode request.
# side -        The side of the street where an address resides relative to the direction 
#               of feature digitization
# addressType - The match level for a geocode request. "PointAddress" is typically the 
#               most spatially accurate match level. "StreetAddress" differs from PointAddress 
#               because the house number is interpolated from a range of numbers. "StreetName" is similar,
#               but without the house number.

geocodeML_batch <- function(id, street, city, state, zip, token){
  require(httr)
  require(rjson)
  
  # check if we have more than 1000, if so stop.
  if (length(id) > 1000){
    print(paste("length is: ", length(id)))
    stop("Can only process up to 1000 addresses at a time.")}
  
  # check if id is numeric
  if (!is.numeric(id)) {
    stop("id variable needs to be numeric.")
  }
  
  # make data frame
  adr_df <- data.frame(OBJECTID = id, 
                       Street = street,
                       City = city,
                       State = state,
                       Zip = zip)
  
  # make json
  tmp_list <- apply(adr_df, 1, function(i) list(attributes = as.list(i)))
  
  # need to coerce ID back to numeric
  tmp_list <- lapply(tmp_list, function(i) { i$attributes$OBJECTID <- as.numeric(i$attributes$OBJECTID); i })
  adr_json <- toJSON(list(records = tmp_list))
  
  # Identify the geocoding web service URL
  gserver <-"http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/geocodeAddresses"
  
  # submit
  req <- POST(
    url = gserver, 
    body = list(addresses = adr_json, f="json", token=token),
    encode = "form")
  #stop_for_status(req) # error check
  
  # process and parse
  res <- content(req, "parsed", "application/json")
  resdfr <- data.frame()
  for (i in seq_len(length(res$locations))){
    d <- with(res$locations[[i]], {data.frame(ID = attributes$ResultID,
                                              lon = as.numeric(location$x),
                                              lat = as.numeric(location$y),
                                              score = score, 
                                              locName = attributes$Loc_name,
                                              status = attributes$Status,
                                              matchAddr = attributes$Match_addr,
                                              side = attributes$Side,
                                              addressType = attributes$Addr_type)})
    resdfr <- rbind(resdfr, d)
  }
  return(resdfr)
}

#--------------------------------------------------------------------------------------
# Some code to use the above functions
#--------------------------------------------------------------------------------------

# set your access token
myToken <- "enter your long ugly ESRI geocoding access token here"

# ---------------------------
# GEOCODE A SINGLE ADDRESS
# ---------------------------
geocode_output <-geocodeSL("1600 Pennsylvania Avenue NW, Washington, DC", myToken, postal = TRUE)
  
# -----------------------------
# GEOCODE A BATCH OF ADDRESSES
# -----------------------------

# make up a data frame with some addresses:
adr_df <- data.frame(
  ID = 1:3,
  street = c('450 Serra Mall', '1600 Amphitheatre Pkwy', '1355 Market Street Suite 900'), 
  city = c('Stanford', 'Mountain View', 'San Francisco'), 
  state = 'CA', 
  zip = c('94305', '94043', '94103'))


# Batch geocode your dataframe of addresses with the following function
adr_gc <- geocodeML_batch(adr_df$ID, adr_df$street, adr_df$city, adr_df$state, adr_df$zip, myToken)

# join back with original data
merge(adr_df, adr_gc, by = "ID", all.x = T)