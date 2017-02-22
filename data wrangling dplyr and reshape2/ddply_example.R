set.seed(1234)
library(plyr)
library(dplyr)
library(reshape2)


# example data:
# I randomly generate a dataset as the example data
# in this data, there are 5 sampling site, in each site there are three blocks, and one block have 6 plot with 4 subplots embedded in it
# taxon is the species
# biomass is the biomass for each species
# time is the sampling time
mydata<-expand.grid(
  site_code = paste(letters[1:5],letters[6:10],sep=''),
  block     = 1:3,
  plot      = 1:6,
  subplot   = 1:4,
  taxon     = paste('species.',1:20,sep=''),
  time      = 0:12
)
mydata<-transform(mydata,biomass=rexp(nrow(mydata)))

# let's see how we can use ddply to calculate the temporval variability of the total biomass in each subplot

#method 1
# we can use twice ddply - without self-defined function
#sum the total mass for each subplot in each sampling time
sum.biomass.each.time<-ddply(
  mydata,
  ~ site_code + block + plot + subplot + time,
  summarize,
  total.mass = sum(biomass)
)

sum.biomass.each.time

# then we calculate the temporal variability using sum.biomass.each.time
tv.total.mass.for.subplots.1<-ddply(
  sum.biomass.each.time,
  ~ site_code + block + plot + subplot,
  summarize,
  temporal.variability = sd(total.mass)/mean(total.mass)
)

# method 2
# we can also using functions defined by ourselves. this is extremly useful when you want to do some complicated job for the subset of the data
# tv.f is a function to calculate the temporal variability of the toal biomass for a given subplot
tv.f<-function(x){
  #input x is the subset of your big dataset
  #I first select the three columns including taxon, time, and biomass
  x.selected<-select(x,taxon,time,biomass)
  #then I make this three-column data to a matrix format with taxon
  # as row and time as column
  x.selected<-acast(x.selected,taxon~time,value.var='biomass')
  # then i calculate the total biomass at each time point
  total.mass<-colSums(x.selected)
  temporal.variability<-sd(total.mass)/mean(total.mass)
  setNames(temporal.variability, 'temporal.variability')
}

tv.total.mass.for.subplots.2<-ddply(
  mydata,
  ~ site_code + block + plot + subplot,
  tv.f  #note, when you include your own functions in the ddply, you need to drop the term summarize
)


# check if the two approaches give same result
identical(tv.total.mass.for.subplots.1,tv.total.mass.for.subplots.2)
# return true

