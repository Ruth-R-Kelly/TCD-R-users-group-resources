### Code for analysis of vegetation community based, 
# including data transformation, RDA, permutation testing and 
# plotting..

# Data and background are given in the paper, which is also in this folder: 

# Kelly, R., Boston, E., Montgomery, W.I. and Reid, N., 2016. The
# role of the seed bank in recovery of temperate heath and blanket 
# bog following wildfires. Applied Vegetation Science, 19(4), pp.620-633.

### set your working directory as appropriate
setwd("C:/R/TCD-R-users-group-resources/MultivariateOrdinationApproaches")

### clear work environment
rm(list = ls())


library("vegan") # load multivariate library
library("MuMIn") # load package for standardizing datasets

#### read in data table ####
veg <- read.table("veg_new2.csv", sep = ",", header = T)

## look at the column names and data structure
names(veg)
str(veg)

###  select columns relating to topic species community ####
veg_spe <- veg[,17:68]

ncol(veg_spe)
# [1] 52


#### select columns relating to the environment ####
env <- veg[,1:16]
str(env)
names(env)
# [1] "Date2"     "Burnt"     "Quadrat"   "X"         "Y"        
# [6] "Site"      "Dung"      "Heat.load" "Habitat"   "pH"       
# [11] "P"         "N"         "Slope"     "Max_age"   "Altitude" 
# [16] "Aspect" 

#### In this study we used the amount of Dung as a proxy for grazing. 
### Here, I set levels for this factor to create on ordered factor
str(env$Dung)
env$Dung2 <- ordered(env$Dung,
                     levels = c("None", "Rare","Occaisional", "Frequent", "Abundant")) 

class(env$Dung2)
str(env$Dung2)

### Dung2 is an ordered factor now.. 
names(env)
env$Date

##### Set date to Julian date ####
# (i.e. day of the year from 1st of January)
x <- as.POSIXlt(env$Date2, format = "%d/%m/%y")
env$DateJ <- x$yday
summary(env$DateJ)

#####
names(env)
# [1] "Date2"     "Burnt"     "Quadrat"   "X"         "Y"        
# [6] "Site"      "Dung"      "Habitat"   "Type"      "Burn_type"
# [11] "pH"        "P"         "N"         "Slope"     "Max_age"  
# [16] "Altitude"  "Aspect"    "Dung2"     "DateJ"       
######################################################

#### standardize and centre environmental variables ####
#to units of standard deviation

env2 <- stdize(env, binary = "omit")
env2

names(env2)
# [1] "Date2"       "Burnt"       "Quadrat"     "z.X"         "z.Y"        
# [6] "Site"        "Dung"        "z.Heat.load" "Habitat"     "z.pH"       
# [11] "z.P"         "z.N"         "z.Slope"     "z.Max_age"   "z.Altitude" 
# [16] "z.Aspect"    "z.Dung2"     "z.DateJ"   


##### Transform the species dataset using a 'hellinger' ####
## transformation to make it suitable for RDA analysis

hell_veg <- decostand(veg_spe, "hell")

###### Model selection using step-wise procedure ####


#### first build your global model with all variables ####
mod_hell <- rda(hell_veg~Burnt*Habitat + Dung2*Burnt + z.Slope*Burnt + 
                  z.Heat.load + z.Altitude + z.DateJ  + z.pH + z.P + z.N 
                + Condition(Site), data= env2, scale = T )

#### Define the minimum model - here this is just sites differ ####
min_hell <- rda(hell_veg~Condition(Site), data = env2, scale = T)

#### select best model using forward step-wise selection, ####
# i.e. variables are added to the minimum model from the 
# global model if they significantly improve the model fit.

mod_dir_forward <-ordistep(min_hell, scope = formula(mod_hell), direction = "forward")

# Note: sometimes this final model includes an interaction of Habitat*Burnt
# and usually not, it is a feature of permutation based tests that they
# change slightly if the result is borderline. Here I have omitted this 
# interaction as there is very little support for including it. 

mod_veg <-mod_dir_forward
mod_veg

# Call: rda(formula = hell_veg ~ Condition(Site) + z.pH + Habitat +
# Burnt + z.P + z.Altitude + z.Heat.load, data = env2, scale = T)
# 
# Inertia Proportion Rank
# Total         52.0000     1.0000     
# Conditional    5.8523     0.1125    5
# Constrained    8.4389     0.1623    7
# Unconstrained 37.7088     0.7252   46
# Inertia is correlations 
# 
# Eigenvalues for constrained axes:
#   RDA1   RDA2   RDA3   RDA4   RDA5   RDA6   RDA7 
# 3.7770 1.3002 1.0105 0.8188 0.7015 0.4346 0.3963 
# 
# Eigenvalues for unconstrained axes:
#   PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8 
# 3.125 2.710 2.048 1.755 1.676 1.579 1.546 1.483 
# (Showed only 8 of all 46 unconstrained eigenvalues)


#### Testing significant of variables by permutation ####

anova.cca(mod_veg, by = "margin")
#Permutation test for rda under reduced model
# Marginal effects of terms
# 
# Model: rda(formula = hell_veg ~ Condition(Site) + z.pH + Habitat + Burnt + z.P + z.Altitude + z.Heat.load, data = env2, scale = T)
#                Df    Var      F N.Perm  Pr(>F)   
#    z.pH         1  2.885 8.3406    199 0.00500 **
#   Habitat       2  1.857 2.6840    199 0.00500 **
#   Burnt         1  1.276 3.6871    199 0.00500 **
#   z.P           1  0.712 2.0568    199 0.01000 **
#   z.Altitude    1  0.592 1.7125    299 0.01667 * 
#   z.Heat.load   1  0.549 1.5862    999 0.03300 * 
#   Residual    109 37.709                         
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### significance of full model by permutation ####

anova.cca(mod_veg)
# Permutation test for rda under reduced model
# Permutations stratified within '"site"'
# 
# Model: rda(formula = hell_veg ~ Condition(Site) + z.pH + Habitat + Burnt + z.P + z.Altitude + z.Heat.load, data = env2, scale = T)
# Df    Var      F N.Perm Pr(>F)   
# Model      7  8.439 3.4848    199  0.005 **
#   Residual 109 37.709                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### significance of axes by permutation ####

anova(mod_veg, by = "axis")
# Model: rda(formula = hell_veg ~ Condition(Site) + z.pH + Habitat + Burnt +      z.P + z.Altitude + z.Heat.load, data = env2, scale = T)
# Df    Var       F N.Perm Pr(>F)   
# RDA1       1  3.777 10.9178    199  0.005 **
#   RDA2       1  1.300  3.7582    199  0.005 **
#   RDA3       1  1.010  2.9209    199  0.005 **
#   RDA4       1  0.819  2.3667    199  0.005 **
#   RDA5       1  0.702  2.0278    199  0.005 **
#   RDA6       1  0.435  1.2563     99  0.190   
# RDA7       1  0.396  1.1455     99  0.300   
# Residual 109 37.709                         
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#### Individual species ####

#### Another interesting thing can be to look at the variance that the 
#model explains in terms of each species.  This can be done with the 
# function 'inertcomp' (see ? intertcomp)

table2 <- inertcomp(mod_veg, display = c("species"),
                    statistic = c("explained"), proportional = TRUE)

write.csv(table2, "var_explained_veg_rda_hell.csv")

formula(mod_veg)

##### If there is a particular variable of interest we can look at this
### after accounting for everything else by putting all the other
### variables in as conditional variables. 
#  This is equivalent to running an RDA with just burning on the residuals
# of a model containing all the other variables. 

burn_only <-  rda(formula = hell_veg ~ Condition(Site + z.pH + Habitat + z.P + z.Altitude + z.Heat.load)  + Burnt, data = env2)
burn_only

# Call: rda(formula = hell_veg ~ Condition(Site + z.pH + Habitat +
# z.P + z.Altitude + z.Heat.load) + Burnt, data = env2)
# 
# Inertia Proportion Rank
# Total         0.41950    1.00000     
# Conditional   0.13170    0.31394   11
# Constrained   0.04084    0.09736    1
# Unconstrained 0.24696    0.58870   46
# Inertia is variance 
# 
# Eigenvalues for constrained axes:
#   RDA1 
# 0.04084 
# 
# Eigenvalues for unconstrained axes:
#   PC1      PC2      PC3      PC4      PC5      PC6      PC7      PC8 
# 0.057894 0.035916 0.031868 0.024652 0.015608 0.013612 0.010146 0.008284 
# (Showed only 8 of all 46 unconstrained eigenvalues)


#### We can use inertcomp again to see how individual species in the 
# model respond to this variable after accounting for everything else. 

table4 <- inertcomp(burn_only, display = c("species"),
                    statistic = c("explained"), proportional = TRUE)

sp_scores <- scores(burn_only, display ="sp")

table5 <- cbind(table4, sp_scores[,1])
write.csv(table5, "var_explained_veg_rda_hell_burn_only.csv")

################### plotting... #####
##

### ordination plots are notoriously ugly and inevitably I end up
# adjusting the fonts and labels in a pdf editor before publication. 
# but here's an attempt at a less ugly version!

### get the proportion of total variance explained by each axes 
## this should be reported in the axis labels. 

mod_veg

#var axes RDA1 = 3.77/52, 7.3
#var axes RDA2 =  1.3002/52, 2.5

sp_scores <- scores(mod_veg, display = "sp", scaling = 3)
write.csv(sp_scores, "veg_mod_sp_scores_scaling3.csv")

#### Here, I cheated and moved the species order, label positions,
## formatted labels, counted quadrats in excel to make the next file
## "species_more_than5percent_veg_2quads_ormore.csv"

#### take a look at the excel files in this folder to see what I've done. 
### I will come back and code this at some later point. 


sp_scores5 <- read.csv("species_more_than5percent_veg_2quads_ormore.csv")
sp_above <- sp_scores5[sp_scores5$pos == "A",]
sp_above

sp_b <- sp_scores5[sp_scores5$pos == "B",]
sp_b

sp_l<- sp_scores5[sp_scores5$pos == "L",]
sp_r <- sp_scores5[sp_scores5$pos == "R",]



## to make pdf plot.. 
pdf("veg_vr1.pdf",   width = 3.1, height = 3.1, pointsize = 6)


par(mar=c(5.1,5.1,4.1,3.1))
plot(mod_veg, scaling = 3, type = "n", xlim = c(-1, 2), ylim = c(-1.5,1.2),
     xlab = ("RDA1 (7.3%)"), ylab = ("RDA2 (2.5%)"), axes = F, cex.lab = 1.6)

box()

axis(1, at = seq(from = -1, to =2, by = 1), tck = 0.01, cex.axis = 1.2)
axis(2, at = seq(from = -1, to =1, by = 1), tck = 0.01, cex.axis = 1.2, las = 1)


text(mod_veg, display = "bp", select = c(T,F,F,T,T,T,T), labels = c("pH","", "", "Burning", "P","Altitude", "Heat load"),
     lwd = 1, font = 2,
     col = "grey30", scaling = 3, cex = 1, arrow.mul = 1.3)
#####################

#####################
points(sp_scores5[,2], sp_scores5[,3], pch = 16, col = "black", cex = 0.5)
text(sp_above[,2], sp_above[,3], cex = 0.5, labels = sp_above$Species, pos = 3, offset = 0.3, font = 3)
text(sp_l[,2], sp_l[,3], cex = 0.5, labels = sp_l$Species, pos = 2, offset = 0.3, font = 3)
text(sp_r[,2], sp_r[,3], cex = 0.5, labels = sp_r$Species, pos = 4, offset = 0.3, font = 3)
text(sp_b[,2], sp_b[,3], cex = 0.5, labels = sp_b$Species, pos = 2, offset = 0.3, font = 3)


##############################
dev.off()

