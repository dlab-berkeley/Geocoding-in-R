#
# census data with 
# tigris and acs packages
# after
# http://rstudio-pubs-static.s3.amazonaws.com/90665_de25062951e540e7b732f21de53001f0.html
# https://github.com/walkerke/tigris
# http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/

# download US Census TIGER Data, eg
# http://www2.census.gov/geo/tiger
# Then go to: http://www.census.gov/geo/maps-data/data/tiger-line.html (read how do i choose...)
# Use the web interface to download
#

library(tigris)
library(sp)

cenpolys <- tracts(state = 'CA', county = c('Alameda'))

plot(dfw)

source("keys/census_api_key.R")
api.key.install(my_census_api_key)
income_data <- acs.fetch(endyear = 2012, 
                         geography = geo.make(state = "CA", 
                                              county = c("Alameda"), 
                                              tract = "*"), 
                         variable = "B19013_001")

str(income_data@geography) #see how state and county are formmated
#need to create the key on which to join spatial and demo data

income_df <- data.frame(paste0("0",as.character(income_data@geography$state), 
                               "00",as.character(income_data@geography$county), 
                               income_data@geography$tract), 
                        income_data@estimate)

colnames(income_df) <- c("GEOID", "hhincome")

censusT <- geo_join(dfw, income_df, "GEOID", "GEOID")

library(classInt)
library(RColorBrewer)
myclass <- classIntervals(censusT$hhincome, 9, style = "jenks")
colpal <- findColours(myclass, brewer.pal(5, "OrRd"))
plot(censusT, border="grey", col=colpal)

plotData <- fortify(censusT, data=censusT@data, region="GEOID")
head(plotData) # take a look at the result of the fortify command

ggplot() +  geom_polygon(data=plotData, aes(x=long, y=lat, group=group))
# Map it.
map <- get_map("Berkeley", zoom=10)  
ggmap(map) + geom_polygon(data=plotData, aes(x=long, y=lat, group=group))

# Add transparency to better see the reference basemap.
ggmap(map) +
  geom_polygon(data=plotData, aes(x=long, y=lat, group=group), alpha=0.5)

#Now, join the census data to the geo data frame.
plotData <- merge(plotData,censusT@data, by.x="id", by.y="GEOID")
head(plotData) # now you can see the attribute data re-joined to the geographic data

#map it - color regions by census variable
ggmap(map) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group, 
                                    fill = hhincome), color = "black", size = 0.25) +
  coord_map()

# get rid of tracts with NA
#censusT <- censusT[!is.na(censusT$hhincome),]
# Too dark - try this
library(scales) #for pretty_breaks
myplot <- ggmap(map) +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = hhincome)) +
  coord_map() +
  scale_fill_distiller(palette = "Greens",
                       breaks = pretty_breaks(n = 8)) +
  guides(fill = guide_legend(reverse = TRUE))

myplot

p2 <- subset(plotData,ALAND > 0)
censusT <-p2
ggmap(map) +
  geom_polygon(data = p2, aes(x = long, y = lat, group = group, 
                                    fill = hhincome), color = "black", size = 0.25) +
  coord_map()

map <- get_map(location=c(lon=mean(geocoded_output$lon), lat=mean(geocoded_output$lat)), zoom=12)
myplot <- ggmap(map) +
  geom_polygon(data = p2, aes(x = long, y = lat, group = group,
                                    fill = hhincome), alpha=0.75) +
  geom_point(aes(x = lon, y = lat), data=geocoded_output, size = 6, col="red" ) +
  coord_map() +
  scale_fill_distiller(palette = "Greens",
                       breaks = pretty_breaks(n = 8)) +
  guides(fill = guide_legend(reverse = TRUE))

myplot

map <- get_map(location=c(lon=mean(geocoded_output$lon), lat=mean(geocoded_output$lat)), zoom=12)
ggmap(map) +
  geom_point(aes(x = lon, y = lat), data=geocoded_output, size = 6, col="red" ) 
