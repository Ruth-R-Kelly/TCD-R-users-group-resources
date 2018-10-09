## Code demonstrating the attributes of shapefiles, known as 'SpatialPolygonsDataFrame'
## 'SpatialPointsDataFrame' 's in R.  And some very basic plotting. 

# setwd("C:/R/TCD-R-users-group-resources/GIS")


#install.packages("GISTools")

library('GISTools')

# Read in the data
data(vulgaris)

class(us_states)

plot(us_states)

### reset the margins to make plot bigger.
par(mar = c(0,0,2,0))

#### add colour
plot(us_states, col = "grey80")

### change background colour

plot(us_states, col = "grey80", bg = "light blue")

####  Add title
title("US states")

### Adding layers.. 

## eg point data from vulgaris

plot(vulgaris, add = T, col = 'purple', pch = 16)

## reset plot
plot(us_states)

####  Understanding data objects.. 

class(us_states)
# [1] "SpatialPolygonsDataFrame"
# attr(,"package")
# [1] "sp"


### There's lots in a SpatialPolygonsDataFrame
str(us_states)

### The bits you're most likely to want are the 'data', the bounding box 'bbox' 
### and the projection system information

head(us_states@data)

us_states@bbox

us_states@proj4string


#### Similarly for a SpatialPointsDataFrame, you'll find these objects too.
class(vulgaris)
# [1] "SpatialPointsDataFrame"
# attr(,"package")
# [1] "sp"

head(vulgaris@data)

vulgaris@bbox

#### coordinate system is in proj4string
vulgaris@proj4string


########## plotting using data attributes of points

#### add colour
plot(us_states, col = "grey80")

### change background colour

plot(vulgaris, pch = 16, col = vulgaris$Year, add = T)


########## plotting using data attributes of polygons - Choropleths!

head(us_states@data)
#

choropleth(us_states, us_states$POP1997)

##### for nice colours the RColourBrewer package is handy

library(RColorBrewer)

### reset the margins to make plot bigger.
par(mar = c(3,3,2,0))

par(mfrow = c(1,1))

display.brewer.all()

### set colours for your population plot data

cols1 <- auto.shading(us_states$POP1997, cols = brewer.pal(5,"Spectral"))

### plot with test new colours.. 

choropleth(us_states, us_states$POP1997, shading = cols1)

###########

### add a legend to choro.pleth

bbox(us_states)

choro.legend(-141, 33, cols1, cex = 0.5)

######################

##### Some other useful R library sources of premade maps 
### include: mapdata, maps, rworldmaps


