#
# Batch Geocoding with the US Census Geocoding Service
#
# pattyf@berkeley.edu, 05/2/2016
#
# Important note:
## You can only geocode 1000 addresses at a time
## so need to add code to loop or subset your files
##
## Documentation:
## 
#Load libraries
library(httr) # to submit geocoding request
library(ggplot2) # to plot output
library(ggmap) # to plot output
library(leaflet) # for interactive plotting
library(stringr)

#clean environment
rm(list=ls())

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

# our file of addresses that need to be geocoded
tiger_input_addressFile <- "tiger/tiger_12addresses_to_geocode.csv"

# the output file we will create
geocoded_output_file <- "geocoded_addresses_out.csv"

# CLI format for CURL
#format of geocoding request for Tiger Geocoder
##curl http://geocoding.geo.census.gov/geocoder/geographies/addressbatch --form addressFile=@tiger/tiger_12addresses_to_geocode.csv --form benchmark=Public_AR_Census2010 --form vintage=Census2010_Census2010 
##
##curl http://geocoding.geo.census.gov/geocoder/geographies/addressbatch
##--form addressFile=@tiger/tiger_12addresses_to_geocode.csv 
##--form benchmark=Public_AR_Census2010 
##--form vintage=Census2010_Census2010
##-o output_file.csv

# The census geocoder does not want column names in the file to be geocoded
# but we want them to make sense of the data when we view it in R
# For info on the correct format for submitting a file of addresses see:
# https://www.census.gov/geo/maps-data/data/geocoder.html
# Five columns - No headers, comma separated EVEN IF DATA NOT AVAILABLE
# Unique ID, house number and street name, city, state, zipcode
# Two valid examples:
#1, 1600 Pennsylvania Ave NW, Washington, DC,
#2, 1600 Pennsylvania Ave NW,,,20502

## Take a look at the addresses that we will geocode
addresses_to_geocode <- read.csv(tiger_input_addressFile, stringsAsFactors = FALSE, col.names = c('id','street','city','state','zip'))

#how many addresses?
num_addresses <- nrow(addresses_to_geocode)

#remove the address data object
rm(addresses_to_geocode)

get_geocoded_addresses <- function(file_of_addresses) {
    tiger_url <- "http://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
    geocoded_addresses <- POST(tiger_url, encode="multipart", 
                           body=list(addressFile=upload_file(file_of_addresses), 
                                     benchmark="Public_AR_Census2010",
                                     vintage="Census2010_Census2010"
                           )
    )
    
    # Output column names
    mycols <- c("id","in_address","match_status","match_type","matched_address","lon_lat","tlid","street_side", "state_fips", "county_fips","tract_fips", "block_fips")
    
    #read output file in to a data frame (not sure how to do these two in one step)
    # create temp file
    mytempfile <- tempfile()
    #write raw output to tempfile
    # content(geocoded_addresses, "text", encoding = "UTF-8")

    capture.output(cat(content(geocoded_addresses)), file=mytempfile)
    #read the data into a data frame
    mylocs <- read.csv(mytempfile,header=FALSE, col.names = mycols)
    #delete tempfile
    unlink(mytempfile)
    
    # split the lat,long values into two separate columns
    mylocs$lon = unlist(lapply(mylocs$lon_lat, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][1]))
    mylocs$lat = unlist(lapply(mylocs$lon_lat, function (x) strsplit(as.character(x), ",", fixed=TRUE)[[1]][2]))
    
    mylocs$lon <- as.numeric(mylocs$lon)
    mylocs$lat <- as.numeric(mylocs$lat)
    
    # save geocoded addresses to a file
    if (skip_rows == 0) {
      # create and write to the file
      write.csv(mylocs,file=geocoded_output_file, row.names=FALSE)
    } else {
      #append to the file
      write.csv(mylocs,file=geocoded_output_file, row.names=FALSE, append=TRUE)
    }
    return(mylocs)
}

# some counters to keep track of the number of addresses we need to process
# we can only batch geocode 1000 addresses at a time
skip_rows <- 0
read_rows <- 1000 
processed_rows <- 0

if (num_addresses < 1000) {
  # geocode them
  my_results <- get_geocoded_addresses(tiger_input_addressFile)
} else {
  #process 1000 addresses at a time

  while (processed_rows < num_addresses) {
    addresses_to_geocode <- read.csv(tiger_input_addressFile, stringsAsFactors = FALSE, nrows=read_rows, skip=skip_rows)
    temp_infile <- tempfile()
    # save geocoded addresses to a file
    write.csv(addresses_to_geocode,file=temp_infile, row.names=FALSE, col.names = FALSE)
    my_results <- get_geocoded_addresses(temp_infile)
    unlink(temp_infile)
    skip_rows <- skip_rows + read_rows
    
  }
}

# Use ggmap to plot geocoded addresses
# as red dots on a google map image
map <- get_map(location=c(lon=mean(my_results$lon),lat=mean(my_results$lat)), zoom=15)
ggmap(map) +
  geom_point(aes(x = lon, y = lat), size = 4, col="red", data = my_results)  

#------------------------------------------------------
# Data Linkage Example:
# Link geocoded addresses to census data
#------------------------------------------------------
library(acs)
source("keys/census_api_key.R")
api.key.install(key=my_census_api_key)

geo<-geo.make(state=c("CA"),county=c(1), tract="*")
  
# !!!! important note -- the package has not been updated to 2013
# data so I'm using the five year span that ends in 2012

income<-acs.fetch(endyear = 2014, span = 5, geography = geo, table.number = "B19001", col.names = "pretty")
attr(income, "acs.colnames")

income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("B19001. Household Income in the Past 12 Months (in 2014 Inflation-Adjusted Dollars): Total:" ,
                                           "B19001. Household Income in the Past 12 Months (in 2014 Inflation-Adjusted Dollars): $200,000 or more")], 
                        stringsAsFactors = FALSE)

income_df <- select(income_df, 1:3)
rownames(income_df)<-1:nrow(income_df)
names(income_df)<-c("GEOID", "total", "over_200")
income_df$percent <- 100*(income_df$over_200/income_df$total)

# read in geocoded addresses
geocoded_output_file <- "geocoded_addresses_out.csv"
my_results <- read.csv(geocoded_output_file,stringsAsFactors = FALSE)

# Create the Key on which we will join the geocoded addresses to the
# Census data - this is the FIPS code, often called the GEOID
my_results$GEOID <- paste0(str_pad(my_results$state_fips, 2, "left", pad="0"), 
                           str_pad(my_results$county_fips, 3, "left", pad="0"), 
                           str_pad(my_results$tract_fips, 6, "left", pad="0"))

# Now Join the census data to the geocoded addresses by the GEOID
my_results2 <- merge(my_results,income_df, by="GEOID")

# Map the results with Leaflet for Interactive mapping
# This way we can click on any address and see the census data value.
#popup = paste("<strong>Address:</strong><br>", my_results2$matched_address,"<br>Percent Below Poverty Line:", my_results2$pctpov), 
leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = my_results2, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste("<strong>Address:</strong><br>", my_results2$matched_address,"<br>Percent of Households<br>above $200k:", my_results2$percent),
                   color = "red", 
                   fillOpacity = 0.7)


#-----------------------------------------------------------------------
# Spatial Overlay #1
# Question:  
#-----------------------------------------------------------------------
library(sp)
library(rgdal)
library(rgeos)

# read in geocoded addresses
geocoded_output_file <- "geocoded_addresses_out.csv"
my_results <- read.csv(geocoded_output_file,stringsAsFactors = FALSE)
head(my_results) # take a look at the results

#what is the type of object
class(my_results)

#let's make it spatial points data frame
coordinates(my_results) <- ~lon+lat
class(my_results)
#plot the points
plot(my_results)

# Alameda Community College Districts
# Format: ESRI Shapefile
# Source: https://data.acgov.org/Geospatial-Data/Community-College-Districts-within-Alameda-County/bdqp-je9q
alameda_ccds <- readOGR(dsn="./shapefiles/AlamedaCommunityCollegeDistricts", layer="geo_export_ffa93779-e8e7-4680-a57c-75b25ae5830c") # Read it into R.
class(alameda_ccds) # what is the data object type?
plot(alameda_ccds)  #plot the CCDs
points(my_results, col="red") # add the geocoded points to the plot

head(alameda_ccds@data) #look at the attributes that describe each polygon

# Let's use the rGEOS over function to find out
# the CCD of each of our addresses
# over stands for spatial overlay
address_ccd <-over(my_results,alameda_ccds)

# over requires both data sets to be spatial objects (they are) 
# with the same coordinate reference system (CRS)
# what is the CRS of the CCDs?
alameda_ccds@proj4string # or proj4string(alameda_ccds)

#what is the CRS of our geocoded points?
my_results@proj4string # undefined

#Let's set the CRS of our points to that of the CCDs
# Why is that ok? the geocoded points are NAD83 CRS if Census geocoder was used, 
# WGS84 (same as the CCDs) if Google geocoder was used.
# However in USA those are for the most part identical (may be a few meters off)
proj4string(my_results) <- CRS(proj4string(alameda_ccds))

#make sure they are the same
proj4string(alameda_ccds) == proj4string(my_results) 

# Now try the overlay operation again:
address_ccd <-over(my_results,alameda_ccds)
address_ccd

# Now we can join the ccd district name (dist_name) to our geocoded addresses
# first, subset the
ccd_df <- address_ccd[c('dist_name')]

#now make sure it is a character string not a factor
str(ccd_df)
ccd_df[] <- lapply(ccd_df, as.character)
str(ccd_df)
#now set NAs to a default value
ccd_df[c("dist_name")][is.na(ccd_df[c('dist_name')])] <- "unknown"
head(ccd_df) # take a look
#join it to our geocoded data
my_results <- cbind(my_results, ccd_df)
#view results
head(my_results)

# Plot it - leaflet Interactive mapping
leaflet() %>% addTiles() %>%
  setView(lng = mean(my_results$lon), lat = mean(my_results$lat), zoom = 16) %>%
  addCircleMarkers(data = my_results, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste("<strong>Address:</strong><br>", my_results3$matched_address,"
                                 <br><strong>Communit College District:</strong><br>", my_results$dist_name),
                   color = "red", 
                   fillOpacity = 0.9)

#
# Question: How many addresses are in each CCD?
#
# create a cross-tab from our overlay (over) operation
addressByCCD_df <- as.data.frame(table(address_ccd$dist_name))

#look at it
head(addressByCCD_df)

#relabel the columsn
names(addressByCCD_df)[names(addressByCCD_df)=="Var1"] <- "ccd_name"
names(addressByCCD_df)[names(addressByCCD_df)=="Freq"] <- "address_count"

#look at it again
head(addressByCCD_df)

#-----------------------------------------------------------------------
# Spatial Overlay #2
# Question: What addresses are within 1000 meters of a school?
#-----------------------------------------------------------------------

library(sp)
library(rgdal)
library(rgeos)


# read in geocoded addresses
geocoded_output_file <- "geocoded_addresses_out.csv"
my_results <- read.csv(geocoded_output_file,stringsAsFactors = FALSE)
head(my_results) # take a look at the results

#what is the type of object
class(my_results)

#let's make it spatial points data frame
coordinates(my_results) <- ~lon+lat
class(my_results)
#plot the points
plot(my_results)

#create a spatialpoints dataframe object from our geocoded address locations
class(my_results)
coordinates(my_results) <- ~lon+lat
class(my_results)

#what is the coordinate system of our data?
my_results@proj4string #undefined

alameda_schools <- readOGR(dsn="./shapefiles/Alameda County Schools", layer="geo_export_c08c26d7-65c8-4b7f-8675-fac05e9b6dca") # Read it into R.
class(alameda_schools)
alameda_schools@proj4string # or proj4string(alameda_schools)

#let's set the CRS of the geocoded points to that of the alameda schools
proj4string(my_results) <- CRS(proj4string(alameda_schools))


#make sure they are the same
proj4string(alameda_schools) == proj4string(my_results) 

#now that both are in the same coordinate space let's transform them to a planar projected CRS
#http://spatialreference.org/ref/epsg/32610/
my_results_utm10 <- spTransform(my_results, CRS("+init=epsg:32610"))
alameda_schools_utm10 <- spTransform(alameda_schools, CRS("+init=epsg:32610"))

# Let's assume
# A sex offender cannot live within 1,000 feet of any school, childcare facility, or place where children gather.
# 1000 feet = 305 meters
alschools_buf <-gBuffer(alameda_schools_utm10, byid=TRUE,width=305)
plot(alschools_buf)
points(my_results_utm10, col="red")

in_school_zone <- over(my_results_utm10,alschools_buf)
in_school_zone$site
in_buf <- in_school_zone[c('site'),]
in_buf[] <- lapply(in_buf, as.character)
in_buf[c("site")][is.na(in_buf[c('site')])] <- "Not within school zone"

#join it to our geocoded data
my_results3 <- cbind(my_results, in_buf)

#plot it using ggmaps - static map
map <- get_map(location=c(lon=mean(my_results3$lon),lat=mean(my_results3$lat)), zoom=15)
ggmap(map) +
  geom_point(aes(x = x, y = y), size = 4, col="black", data = alameda_schools@data) +
  geom_point(aes(x = lon, y = lat), size = 4, col="blue", data = my_results3[my_results3$site == 'Not within school zone',]) +
  geom_point(aes(x = lon, y = lat), size = 5, col="red",  data = my_results3[!my_results3$site == 'Not within school zone',])

#plot it - leaflet Interactive mapping
leaflet() %>% addTiles() %>%
  setView(lng = mean(my_results3$lon), lat = mean(my_results3$lat), zoom = 16) %>%
  addCircleMarkers(data = my_results3, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste("<strong>Address:</strong><br>", my_results3$matched_address,"<br>In School Zone:", my_results3$site),
                   color = "red", 
                   fillOpacity = 0.9) %>%
   addCircleMarkers(data = my_results3[my_results3$site == 'Not within school zone',], lng = ~lon,
                 lat = ~lat, radius = 5, stroke=F,
                 popup = paste("<strong>Address:</strong><br>", my_results3$matched_address,"<br>In School Zone:", my_results3$site),
                 color = "blue", 
                 fillOpacity = 0.7) %>%
   addMarkers(data= alameda_schools, lng= ~x, lat=~y,
                   popup = paste("<strong>School:</strong><br>", alameda_schools$site),
                   )
