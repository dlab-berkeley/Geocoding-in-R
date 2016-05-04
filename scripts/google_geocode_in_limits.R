#
# Purpose: Use GGMAP to geocode with source="google" source" option
# within google daily query limits 
# Subject to Google Terms of Use: http://developers.google.com/maps/terms
# You need to re-run the script when your number of needed geocodes exceeds the query limit
# pattyf@berkeley.edu, 12/09/2015
# created for dlab.berkeley.edu tutorial as example
#

address_file<- "address_data/formatted/oak_liq_w_ids_types_headers.csv"
address_file_copy <-"address_data/formatted/oak_liq_w_ids_types_headers_copy.csv"
address_file_geocoded <- "address_data/formatted/oak_liq_w_ids_types_headers_geocoded.csv"
# DO ONCE - make a copy of the data with the addresses properly formatted
if (!file.exists(address_file_copy)) {
  data <- read.csv(file=address_file,stringsAsFactors=F) # read data
  data$address <- with(data,paste(street,city,state,zip, sep=",")) #add single column address
  data[19,8]<-"7305 Edgewater Dr,Oakland,CA,94621"  ## Specific to this data
  
  write.csv(data,file=address_file_copy, row.names=FALSE)
}

# Read in the copy of the data to be geocoded
data <- read.csv(file=address_file_copy,stringsAsFactors=F) # read data
maxrecs <- as.numeric(geocodeQueryCheck())
if (maxrecs > nrow(data)) {
  maxrecs = nrow(data)
}
maxrecs

if (maxrecs > 0) {
  #create two subsets
  not_geocoded <- slice(data,1:maxrecs)
  geocode_later <- slice(data,maxrecs+1:n())
  
  #save to file what we will geocode later
  write.csv(geocode_later,file=address_file_copy, row.names=FALSE)
  rm(geocode_later)
  
  geocoded_output <- geocode(not_geocoded$address, output = "latlona", source = "google")
  
  geocoded_output <- data.frame(not_geocoded, geocoded_output) # combine the input data with geocoded results
  
  #save output
  if (file.exists(address_file_geocoded)) {
    write.table(geocoded_output,file=address_file_geocoded, col.names=FALSE, row.names=FALSE, sep=",", append=TRUE)
  } else {
    write.table(geocoded_output,file=address_file_geocoded, row.names=FALSE, sep=",")
  }
}

#check file with geocoded data
doh <- read.csv(file=address_file_geocoded,stringsAsFactors=F) # read data
doh
