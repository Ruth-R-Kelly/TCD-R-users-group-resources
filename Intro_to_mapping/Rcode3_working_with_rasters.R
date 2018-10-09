### Code for loading raster maps from .asc (ascii files) 
## and also downloading raster datasets of global climate and altitude data 

setwd("C:/R/GIS")

library(maptools)
library(raster)
library(sp)

### If you already have the map you want in a raster format - .asc is the most common one.
### You can read this in using the function 'readAsciiGrid' in the package 'maptools'

alt1 <- readAsciiGrid("alt1_test.asc")

### the most intuitive plotting is achieved 
par(mar = c(1, 1, 1, 1))
image(alt1)
plot(alt1)


#### raster objects also have boundary boxes, coordinate systems 
### and data which are accessed in the same way as for shp files.
### i.e. 

alt1@bbox
alt1@proj4string
summary(alt1@data)


### The function 'getData' in the package 'raster' is very 
### useful for getting global climate data (from http://www.worldclim.org/bioclim)
### country boundaries and altitude

### e.g. for altitude

world_clim1 <- getData("worldclim",var="bio",res=10)


### this has downloaded the full set of global bioclimatic variables at 
## a 10minute resolution to the working directory on my computer. 
## You can also request res = 0.5, 2.5 or 5 minutes but these take
## longer to download so I haven't used them here. 

names(world_clim1)
# [1] "bio1"  "bio2"  "bio3"  "bio4"  "bio5"  "bio6"  "bio7"  "bio8"  "bio9"  "bio10" "bio11"
# [12] "bio12" "bio13" "bio14" "bio15" "bio16" "bio17" "bio18" "bio19"

### take a look at :  http://www.worldclim.org/bioclim to see what these represent

### e.g. bio1 is the mean annual temperature 

## plotted as 

image(world_clim1)

### etc.. 

par(mfrow = c(1,1))

### Altitude files are supplied on a country by country basis, 
### based on the country ISO codes - you can get these codes using

country_codes <- getData('ISO3')

country_codes

## e.g. Indonesia = IND  

### Altitude for Indonesia can then be imported as .. 

Alt_ind <- getData('alt', country='IND', mask=TRUE)

summary(Alt_ind)
###############################

