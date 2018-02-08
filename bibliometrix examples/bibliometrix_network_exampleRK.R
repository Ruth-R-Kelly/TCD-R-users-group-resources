### Playing with the package 'bibliometrix' to see how api's work and make a 
## collaborator network plot. This is a really nice package and there's lots to do
## with it.  See : http://www.bibliometrix.org/
## Also there's a nice function map here. . 
## http://www.bibliometrix.org/bibliometrixfunctions2.jpg

####install.packages("bibliometrix")
### note there are a lot of dependencies to download for this!

library(bibliometrix)

### get your api key from 
### https://dev.elsevier.com/sc_apis.html

### and insert it below where the code reads "your_api_here"

apikey1 <- "your_api_here"

### to find your scopus ID - use.. 

d <- data.frame(last_name = "Kelly", first_name = "Ruth", affiliation =  "Belfast")

IDx <- idByAuthor(df = d, api_key = apikey1)


### retrieve all scopus records for that author Id 
m <- retrievalByAuthorID(IDx$id, apikey1, remove.duplicated = TRUE)

names(m)

### create network object based on collaborations between authors.. 
AuthorNet <- biblioNetwork(m$M, "collaboration", "authors")

str(AuthorNet)

net <- networkPlot(AuthorNet, n = 100, Title = "Ruth's collaboration network", type = "kamada",
                   size = 7, labelsize = 0.6)


#######  write to png.. 


png(filename = "Network_plot1.png",
    width = 480, height = 480)

net <- networkPlot(AuthorNet, n = 100, Title = "Ruth's collaboration network", type = "kamada",
                   size = 7, labelsize = 1)

dev.off()

######

####  There are lots of packages you can use to make nice plots from the 'AuthorNet' object. 
## I also hear that program 'gephi' has a friendly user interface. But this is where I 
## decided to get back to my other work. If you make nicer plots, please add some code for me :)
