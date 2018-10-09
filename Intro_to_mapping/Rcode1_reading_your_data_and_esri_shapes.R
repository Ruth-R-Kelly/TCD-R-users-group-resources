### Code for creating a spatial object from excel data in R - Ruth Kelly 13/12/2017

library(maptools)
library(rgdal)
library(sp)

### read in your data file as from .csv or text file as usual. 
## e.g.

all_sites_year1 <- read.csv("all_sites_year2.csv")

### view this to see if it has loaded properly
summary(all_sites_year1)
head(all_sites_year1)

### Turn this into a SpatialPointsDataFrame,
## using the function ' coordinates'in the package 'sp'
## You do this by 'telling' R which columns in the dataframe contain the 
## coordinate locations. 

### The objects after the <- are the names of the columns containing the 
### coordinate data in the dataframe.  
### Don't forget the ~ symbol after the  <- 

coordinates(all_sites_year1) <- ~ x_coordina + y_coordina


### Check this is indeed a points file now. 
class(all_sites_year1)
str(all_sites_year1)
head(all_sites_year1)

head(all_sites_year1@data)

all_sites_year1@bbox

all_sites_year1@proj4string 

plot(all_sites_year1, col = all_sites_year1$Burnt, pch = 16)


### If you want to export this as a shapefile you will need...
#### The functions 'readOGR' and 'writeOGR' let you read and write 
### Esri shapefiles for compatibility ArcView (in package rgdal)

## Eg. To write our points object - all_sites_year1 - as a .shp file use:

writeOGR(obj=all_sites_year1, dsn=".", layer="all_sites1", driver="ESRI Shapefile")


### similarly you can read in .shp files from your working directory using: 

counties <- readOGR(dsn = ".", layer = "ni_counties")

plot(counties)


plot(all_sites_year1, col = all_sites_year1$Burnt, pch = 16, add = T)

###### To set a coordinate system based on another layer.. 

all_sites_year1@proj4string  <- counties@proj4string

###############
