## Session 10 - Further Data Visualisation in R ##

## Learning ggplot2

If you are new to ggplot2 you are better off starting with a systematic introduction, rather than trying to learn from reading individual documentation pages. Currently, there are three good places to start:

  1.  The [data visualisation][r4ds-vis] and
[graphics forcommunication][r4ds-comm] chapters in
[R for data science][r4ds]. R for data science is designed to
give you a comprehensive introduction to the
[tidyverse](http://tidyverse.org), and these two chapters will
you get up to speed with the essentials of ggplot2 as quickly as
possible.

1.  If you'd like to take an interactive online course, try
[Data visualisation with ggplot2][datacamp] by Rick Scavetta
on datacamp.

1.  If you want to dive into making common graphics as quickly
as possible, I recommend [The R Graphics Cookbook][cookbook]
by Winston Chang. It provides a set of recipes to solve common
graphics problems. A 2nd edition will is due out in 2017.

If you've mastered the basics and want to learn more, read [ggplot2: Elegant Graphics for Data Analysis][ggplot2-book]. It describes the theoretical underpinnings of ggplot2 and shows you how all the pieces fit together. This book helps you understand the theory that underpins ggplot2, and will help you create new types of graphic specifically tailored to your needs. The book is not available for free, but you can find the complete source for the book at <https://github.com/hadley/ggplot2-book>.

## Getting help

There are two main places to get help with ggplot2:

  1.  The [ggplot2 mailing list][ml] is a friendly place to ask any
questions about ggplot2. You must be a member to post messages,
but anyone can read the archived discussions.

1.  [stackoverflow](so) is a great source of answers to common ggplot2
questions. It is also a great place to get help, once you have
created a reproducible example that illustrates your problem.

[ggplot2-book]: http://amzn.to/2fncG50
[gg-book]: http://amzn.to/2ef1eWp
[ml]: https://groups.google.com/forum/?fromgroups#!forum/ggplot2
[so]: http://stackoverflow.com/questions/tagged/ggplot2?sort=frequent&pageSize=50
[cookbook]: http://amzn.to/2dVfMfn
[r4ds]: http://r4ds.had.co.nz
[r4ds-vis]: http://r4ds.had.co.nz/data-visualisation.html
[r4ds-comm]: http://r4ds.had.co.nz/graphics-for-communication.html
[datacamp]: https://www.datacamp.com/courses/data-visualization-with-ggplot2-1



############################################################################
setwd("C:/R/Rusers_group_testing/ggplot2rk")

library(ggplot2)
library("ggplot2")
library("datasets")



################################
####### scatterplot example ####
################################

# 
data(iris)

iris$abundance <- rpois(nrow(iris), 20)
str(iris)

Piris <- ggplot(iris, aes(x = Sepal.Width, y =  Sepal.Length))

Piris2 <- Piris + geom_point()
Piris2 

Piris3 <- Piris2 + geom_line()
Piris3 

Piris4 <- Piris2 + geom_smooth()
Piris4

##### try adding different colours for each species.  
####  We can do this by adding colour = iris$Species into our prevous code
Piris <- ggplot(iris, aes(x = Sepal.Width, y =  Sepal.Length, colour = Species))
Piris2 <- Piris + geom_point()

Piris3 <- Piris2 + geom_smooth(method = "lm")
Piris3

### We can also adjust sizes, colours, shapes etc. for specific inside geoms.. e.g. 
names(iris)
Piris <- ggplot(iris, aes(x = Sepal.Width, y =  Sepal.Length, colour = Species))
Piris2 <- Piris + geom_point(aes(shape = iris$Species, size = iris$abundance/2,
                                 alpha = 1/10))
Piris2 

### Note: alpha adds transparency.. 

####### a more complicated barplot example which incorporates themes and guides..




# First create dataframe for each country with area (km2) and population according to the country's last census 

Cam <- read.csv("Central_america.csv")
summary(Cam)

#######################
## Order of arguments in ggplot = dataframe, aes (aesthetics), then geom (geometric object)

## We can add 'layers' to our plot using the '+' operator 
## geom_bar is our first layer - this calls a barplot

p1 <- ggplot(Cam, aes(x = country, y = Density)) + 
  geom_bar(stat = "identity", fill = "light blue", colour = "black")

# stat = "identity" - maps the value of the data to the y aesthetic (the alternative being 'stat = "bin", which maps the number of cases)
# "fill" = fill colour - can use web colours in R
# "colour" = outline colour

p1

## Change the y label using ylab()
p2 <- p1 + ylab("Population density")

p2

## Add title using ggtitle(). (*km^2 creates superscript 2)
p3 <- p2 + ggtitle(expression(paste("Average population per " *km^2)))

p3

###  make the title go to the centre of the plot
p4 <- p3 + theme(plot.title = element_text(hjust = 0.5))
p4

## Add values to the bars - vjust alters the height of the numbers (vertical justification))
p5 <- p4 + geom_text(aes(label=round(Density,1)), vjust=1.2, size = 3)
p5

### We use theme() to change the general look of the plot ####

## To get rid of the grey background grid
p6 <- p5 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
               theme(panel.background = element_blank())

p6

## get rid of the x axis label
p6 <- p5 + theme(axis.title.x=element_blank())

p6

## Bold face and larger text (+ vjust to move the bigger label away from the plot)

p7 <- p6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))

p7

## Axis text bold face, black and bigger

p8 <- p7 +  theme(axis.text.x=element_text(face="bold", size=8, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))


p8

## Change plot title
p9 <- p8 + theme(plot.title = element_text(face="bold", size=25))

p9


################################
#### All together now.. ####
###############################

p <- ggplot(Cam, aes(x = country, y = Density)) + 
          geom_bar(stat = "identity", fill = "light blue", colour = "black") 

p <- p +  ylab("Population density") +
  ggtitle(expression(paste("Average population per " *km^2))) +
  geom_text(aes(label=Density), vjust=1.2, size = 3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5)) +
  theme(axis.text.x=element_text(face="bold", size=8, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=8, colour = "black")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=15, hjust = 0.5)) 

print(p)


#### an example of a coefficient plot showing model outputs

All_mods <- read.csv("pollinator_mods.csv")

summary(All_mods)


### Plot of model coefficients


All_mods <- read.csv("pollinator_mods.csv")

summary(All_mods)


### Plot of model coefficients
zp1 <- ggplot(All_mods, aes(colour = Species, shape = Species))

zp2 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/4), lty = 2) + 
  geom_pointrange(aes(x = modelName, y = Coefficient, 
                      ymin = Confint_L,
                      ymax = Confint_U),show.legend=T,
                  lwd = 0.5, position = position_dodge(width = 2/3)) + 
  ggtitle("Visiting pollinator abundance") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip() + theme_bw() + 
  theme(axis.title.y=element_blank()) + 
  theme(plot.title = element_text(face="bold", size=15, hjust = 0.5)) + 
  #zp1 <- zp1 + scale_colour_manual(values = c("dark red", "tomato2", "goldenrod1"))
  #zp1 <- zp1 + scale_colour_manual(values = c("dark blue", "#3182bd", "#9ecae1"))
  #zp1 <- zp1 + scale_colour_manual(values = c( "dark green", "#31a354", "#a1d99b"))
  #zp1 <- zp1 + scale_colour_grey(start = 0, end = 0.8)
  theme(legend.text = element_text(face = "italic", size = 8))  + 
  scale_colour_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(size = 8)) + 
  theme(axis.text.y = element_text(size = 8)) + 
  theme(axis.title.x = element_text(size = 8)) + 
  theme(plot.title = element_text(size = 11)) + 
  guides(colour = guide_legend(title = NULL))  + 
  guides(shape = guide_legend(title = NULL))

print(zp2)

## Have fun plotting :)  There are many more examples available online!