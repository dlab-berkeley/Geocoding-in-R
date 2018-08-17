#
# Spatial Analysis of Addresses Geocoded with Census Geocoder
#
# pattyf@berkeley.edu, 5/3/2016

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


#------------------------------------------------------
# Data Linkage Example:
# Link geocoded addresses to census data
#------------------------------------------------------
#clean environment
rm(list=ls())

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

# There are many online services for downloading census data. 
# You can use one of these to download census data and then read the data into R.
# Or you can use a library like "acs" to make a request of the census online data service API from within R.
# You can link the census data to your geocoded addresses by the FIPS code.
# The FIPS code, also called GEOID, identifies the census geography to which the tabular data have been aggregated.
# For example, the specific census tract or blockgroup.
# This requires the geocoded addresses to have FIPS codes to link to the census data.
# If you use the Census Geocoding Service you get the FIPS codes with your geocoded output.
# If you use another service that does not give you the FIPS code, you can use the FCC census api or spatial overlay to get 
# the FIPS code for each geocoded address.

# The code below uses the acs library and is a modification of the following blog post which has great examples and more details:
# http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3

# Use the ACS (American Community Survey) library to fetch census tract level data for our geocoded addresses.
library(acs)
library(stringr) # to format strings

# You need a census api key to use this library. You can get it in a few minutes at:
# http://api.census.gov/data/key_signup.html

# I keep my key in a file that I source to read into R
# My file has one line and looks like this (NOT MY REAL KEY):
# my_census_api_key <- "f2666666666666666666666666666632" 
source("keys/census_api_key.R")

# Activate the key
api.key.install(key=my_census_api_key)

# Identify the census geography of interest
geo<-geo.make(state=c("CA"),county=c(1), tract="*")

# Fetch the census data of interest
income<-acs.fetch(endyear = 2014, span = 5, geography = geo, table.number = "B19001", col.names = "pretty")

# Take a look at the specific ACS columns returned
attr(income, "acs.colnames")

# Convert the census data to a data frame, (1) keeping only the columns of interest and (2) creating the FIPS key
income_df <- data.frame(paste0(str_pad(income@geography$state, 2, "left", pad="0"), 
                               str_pad(income@geography$county, 3, "left", pad="0"), 
                               str_pad(income@geography$tract, 6, "left", pad="0")), 
                        income@estimate[,c("B19001. Household Income in the Past 12 Months (in 2014 Inflation-Adjusted Dollars): Total:" ,
                                           "B19001. Household Income in the Past 12 Months (in 2014 Inflation-Adjusted Dollars): $200,000 or more")], 
                        stringsAsFactors = FALSE)

# take a look at it
head(income_df)

# add row numbers to the data frame
rownames(income_df)<-1:nrow(income_df)

# relabel the columns
names(income_df)<-c("GEOID", "total", "over_200")

# create a new variable - percent of incomes that are over 200k in the census tract
income_df$percent <- 100*(income_df$over_200/income_df$total)

# take a look at it
head(income_df)

# Read in geocoded addresses
geocoded_output_file <- "geocoded_addresses_out.csv"
geocoded_results <- read.csv(geocoded_output_file,stringsAsFactors = FALSE)

# take a look at them
head(geocoded_results)

# Create the Key on which we will join the geocoded addresses to the
# Census data - this is the FIPS code, often called the GEOID
geocoded_results$GEOID <- paste0(str_pad(geocoded_results$state_fips, 2, "left", pad="0"), 
                           str_pad(geocoded_results$county_fips, 3, "left", pad="0"), 
                           str_pad(geocoded_results$tract_fips, 6, "left", pad="0"))

# take a look at them
head(geocoded_results)

# Now Join the census data to the geocoded addresses by the GEOID
geocoded_results <- merge(geocoded_results,income_df, by="GEOID")

# take a look at them
head(geocoded_results)

# Map the results with Leaflet for Interactive mapping
# This way we can click on any address and see the census data value.
leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = geocoded_results, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste0("<strong>Geocoded Address:</strong><br>", geocoded_results$matched_address,
                                 "<br><strong>Percent of Households above $200k:</strong> ", round(geocoded_results$percent,2), "%"),
                   color = "red", 
                   fillOpacity = 0.7)


#----------------------------------------------------------------------------------------
# Spatial Overlay #1
# Question:  What is the Community College District for each of our geocoded addresses?
#----------------------------------------------------------------------------------------
#clean environment
rm(list=ls())

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

library(sp)
library(rgdal)
library(rgeos)

# read in geocoded addresses
geocoded_output_file <- "geocoded_addresses_out.csv"
geocoded_results <- read.csv(geocoded_output_file,stringsAsFactors = FALSE)
head(geocoded_results) # take a look at the results

# what is the type of object
class(geocoded_results)

# let's make it spatialPointsDataFrame
coordinates(geocoded_results) <- ~lon+lat
class(geocoded_results)

# plot the points
plot(geocoded_results)

# Get the Alameda Community College Districts data
# Format: ESRI Shapefile
# Source: https://data.acgov.org/Geospatial-Data/Community-College-Districts-within-Alameda-County/bdqp-je9q

# Read downloaded shapefile into R
alameda_ccds <- readOGR(dsn="./shapefiles/AlamedaCommunityCollegeDistricts", layer="geo_export_ffa93779-e8e7-4680-a57c-75b25ae5830c") 
class(alameda_ccds) # what is the data object type?
plot(alameda_ccds)  #plot the CCDs
points(geocoded_results, col="red") # add the geocoded points to the plot

head(alameda_ccds@data) #look at the attributes that describe each polygon

# Let's use the rGEOS over function to find out
# the CCD of each of our addresses
# over stands for spatial overlay
address_ccd <-over(geocoded_results,alameda_ccds)

# That didn't work
# "over" requires both data sets to be spatial objects (they are) 
# with the same coordinate reference system (CRS)
# What is the CRS of the CCDs?
alameda_ccds@proj4string # or proj4string(alameda_ccds)

# What is the CRS of our geocoded points?
geocoded_results@proj4string # undefined

# Let's set the CRS of our points to that of the CCDs
# Why is that ok? the geocoded points are NAD83 CRS if Census geocoder was used, 
# WGS84 (same as the CCDs) if Google geocoder was used.
# However in USA those are for the most part identical (may be a few meters off)
proj4string(geocoded_results) <- CRS(proj4string(alameda_ccds))

# make sure the CRSs are the same
proj4string(alameda_ccds) == proj4string(geocoded_results) 

# Now try the overlay operation again:
address_ccd <-over(geocoded_results,alameda_ccds)
address_ccd # take a look at the output

# Now we can join the CCD district name (dist_name) to our geocoded addresses
# first, subset the overlay results
ccd_df <- address_ccd[c('dist_name')]

# Make sure the CCD dist_name is a character string not a factor
str(ccd_df)
ccd_df[] <- lapply(ccd_df, as.character)
str(ccd_df)

# Set NAs to a default value
ccd_df[c("dist_name")][is.na(ccd_df[c('dist_name')])] <- "unknown"
head(ccd_df) # take a look

# Join the CCD data to our geocoded data
geocoded_results <- cbind(geocoded_results, ccd_df)

# view results
head(geocoded_results)

# Plot it - leaflet Interactive mapping
leaflet() %>% addTiles() %>%
  setView(lng = mean(geocoded_results$lon), lat = mean(geocoded_results$lat), zoom = 16) %>%
  addCircleMarkers(data = geocoded_results, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste("<strong>Geocoded Address:</strong><br>", geocoded_results$matched_address,"
                                 <br><strong>Communit College District:</strong><br>", geocoded_results$dist_name),
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
addressByCCD_df

#-----------------------------------------------------------------------
# Spatial Overlay #2
# Question: What addresses are within 1000 meters of a school?
#-----------------------------------------------------------------------
#clean environment
rm(list=ls())

#set working directory
setwd("~/Documents/Dlab/dlab_workshops/rgeocoding")

library(sp)
library(rgdal)
library(rgeos)


# Read in geocoded addresses
geocoded_output_file <- "geocoded_addresses_out.csv"
geocoded_results <- read.csv(geocoded_output_file,stringsAsFactors = FALSE)
head(geocoded_results) # take a look at the results

# what is the type of object
class(geocoded_results)

#create a spatialPointsDataFrame object from our geocoded address locations
coordinates(geocoded_results) <- ~lon+lat
class(geocoded_results)

#plot the points
plot(geocoded_results)

#what is the coordinate system of our data?
geocoded_results@proj4string #undefined

# Get the Alameda County Schools data
# Format: ESRI Shapefile
# Source: https://data.acgov.org/
# Read downloaded shapefile into R
alameda_schools <- readOGR(dsn="./shapefiles/AlamedaCountySchools", layer="geo_export_c08c26d7-65c8-4b7f-8675-fac05e9b6dca") 
# plot it
plot(alameda_schools)

# What class of data object is it?
class(alameda_schools)

# What is its CRS?
alameda_schools@proj4string # or proj4string(alameda_schools)

# Let's set the CRS of the geocoded points to that of the alameda schools
# Why is that ok? the geocoded points are NAD83 CRS if Census geocoder was used, 
# WGS84 (same as the CCDs) if Google geocoder was used.
# However in USA those are for the most part identical (may be a few meters off)
proj4string(geocoded_results) <- CRS(proj4string(alameda_schools))

# make sure they are the same
proj4string(alameda_schools) == proj4string(geocoded_results) 

# now that both are in the same coordinate space let's transform them to a 2D projected CRS
# Here we use UTM zone 10N, WGS84
# http://spatialreference.org/ref/epsg/32610/
geocoded_results_utm10 <- spTransform(geocoded_results, CRS("+init=epsg:32610"))
alameda_schools_utm10 <- spTransform(alameda_schools, CRS("+init=epsg:32610"))

# Let's assume we are investinging the addressesrelative to schools and that
# we want to see if any of these addresses are within 1000 feet of a school.
# 1000 feet = 305 meters

# Create a polygon from each point location that is the 1,000 foot buffer around the school
# We submit the buffer distance in meters because meters are the units of the CRS.
alschools_buf <-gBuffer(alameda_schools_utm10, byid=TRUE,width=305)

#plot the buffers
plot(alschools_buf)

# add the geododed address points
points(geocoded_results_utm10, col="red")

# "over" (overlay) operation to see what geocoded addresses are within school buffer zones
in_school_zone <- over(geocoded_results_utm10,alschools_buf)

#take a look at the output
in_school_zone

# create a new data frame that just has our column of interest - schoolname (site)
in_buf <- in_school_zone[c('site')]

# Make sure the schoolname (site) is a character string not a factor
in_buf[] <- lapply(in_buf, as.character) 

# Replace NAs with a default value
in_buf[c("site")][is.na(in_buf[c('site')])] <- "Not within school zone"

# take a look at our data
in_buf

# Join it to our geocoded data
geocoded_results <- cbind(geocoded_results, in_buf)

# Take a look
head(geocoded_results)

#plot it using ggmaps - static map
map <- get_map(location=c(lon=mean(geocoded_results$lon),lat=mean(geocoded_results$lat)), zoom=15)
ggmap(map) +
  geom_point(aes(x = x, y = y), size = 4, col="black", data = alameda_schools@data) +
  geom_point(aes(x = lon, y = lat), size = 4, col="blue", data = geocoded_results[geocoded_results$site == 'Not within school zone',]) +
  geom_point(aes(x = lon, y = lat), size = 5, col="red",  data = geocoded_results[!geocoded_results$site == 'Not within school zone',])

# plot it - leaflet Interactive mapping
# First subset the data to keep the code clearer
geocoded_resultsInZone <- subset(geocoded_results, site != 'Not within school zone')
geocoded_resultsOutsideZone <- subset(geocoded_results, site == 'Not within school zone') 

leaflet() %>% addTiles() %>%
  setView(lng = mean(geocoded_results$lon), lat = mean(geocoded_results$lat), zoom = 16) %>%
  addCircleMarkers(data = geocoded_resultsInZone, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste0("<strong>Address:</strong><br>",  geocoded_resultsInZone$matched_address,
                                  "<br><strong>In School Zone: </strong>",  geocoded_resultsInZone$site),
                   color = "red", 
                   fillOpacity = 0.9) %>%
  addCircleMarkers(data = geocoded_resultsOutsideZone, lng = ~lon,
                   lat = ~lat, radius = 5, stroke=F,
                   popup = paste0("<strong>Address: </strong><br>", geocoded_resultsOutsideZone$matched_address,
                                  "<br><strong>In School Zone: </strong>",  geocoded_resultsOutsideZone$site),
                   color = "blue", 
                   fillOpacity = 0.7) %>%
  addMarkers(data= alameda_schools, lng = ~x, lat =~y,
             popup = paste("<strong>School:</strong><br>", alameda_schools$site),
  )

#---------------------------------------------------------------------------
# sessionInfo()
#---------------------------------------------------------------------------
#R version 3.2.2 (2015-08-14)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: OS X 10.9.5 (Mavericks)

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] htmlwidgets_0.5 rgeos_0.3-14    rgdal_1.0-4     sp_1.2-1        leaflet_1.0.0   ggmap_2.6.1     ggplot2_2.1.0   httr_1.1.0      acs_2.0        
#[10] XML_3.98-1.3    plyr_1.8.3      stringr_1.0.0  

#loaded via a namespace (and not attached):
#  [1] Rcpp_0.12.4         bitops_1.0-6        tools_3.2.2         digest_0.6.8        jsonlite_0.9.19     gtable_0.1.2        lattice_0.20-33    
#[8] png_0.1-7           mapproj_1.2-4       curl_0.9.6          yaml_2.1.13         proto_0.3-10        RgoogleMaps_1.2.0.7 maps_3.0.0-2       
#[15] grid_3.2.2          R6_2.1.2            jpeg_0.1-8          RJSONIO_1.3-0       reshape2_1.4.1      magrittr_1.5        scales_0.3.0       
#[22] htmltools_0.2.6     mime_0.4            geosphere_1.4-3     colorspace_1.2-6    labeling_0.3        stringi_1.0-1       RCurl_1.95-4.8     
#[29] munsell_0.4.2       rjson_0.2.15 