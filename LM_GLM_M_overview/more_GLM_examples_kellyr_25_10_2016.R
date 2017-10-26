#### Example datasets and code for GLM's with different response distributions
## Ruth Kelly 25/10/2017

###  Datasets "chem_example.csv" and "birds.csv" are from my own peatland fieldwork
### Amphibian roadkill dataset is from Zuur et al. 2009 "Mixed effects models and extensions in ecology with R"



### load dataset

chem <- read.csv("chem_example.csv")

##### This dataset contains data on soil chemistry for quadrats within 6 sites. 
##### The variable of interest is burning, in particular it's effect on soil chemistry


names(chem)
str(chem)
summary(chem)

#### Let's look at P - Phosphorus

summary(chem$P)
### Note all positive and continous
hist(chem$P, breaks = 20)

#### an optimist fits a normal model first.. 

mod_norm <- glm(P~Burnt*Habitat + Site, data = chem)

hist(residuals(mod_norm))
shapiro.test(residuals(mod_norm))

# Shapiro-Wilk normality test
# 
# data:  residuals(mod_norm) 
# W = 0.9391, p-value = 3.212e-05

##### not normal...

plot(mod_norm)


##### Try a gamma distribution with a log link

mod_log <- glm(P~Burnt*Habitat + Site, data = chem, family = "Gamma"(link = "log"))
plot(mod_log)

res1 <- residuals(mod_log, type = "deviance")



################## 

summary(mod_log)

library("car")
Anova(mod_log, type = "III")

### GLM with count data ----

#### First example: Australian amphibian roadkill, (ARR) from Zuur et al. 2009

AAR <- read.table("RoadKills.txt", header = T)

names(AAR)
str(AAR)
summary(AAR)

hist(AAR$TOT.N)
class(AAR$TOT.N)
summary(AAR$TOT.N)


##### Poisson model as a starting point for count data (integers/positive)

mod_ARR <- glm(TOT.N ~ D.PARK, data = AAR, family = "poisson"(link = "log"))
summary(mod_ARR)

theta <- mod_ARR$deviance/mod_ARR$df.residual
theta

### check number of zeros in dataset

length(which(AAR$TOT.N == 0))

### rule of thumb for this is: we want theta = 1, theta < 2 is okay,  
### theta >2 and <15 try quassipoisson, theta > 15 try negative binomial
### or if you prefer (as I do) go straight to negative binomial at theta > 2

######### function for negative binomial is glm.nb 

?glm.nb


mod_nbin <- glm.nb(TOT.N ~ D.PARK, data = AAR) ## note no family specification

theta <- mod_nbin$deviance/mod_nbin$df.residual
theta

####### negative binomial solves our overdispersion problem...


### MODEL VALIDATION 

# 1 plot deviance residuals vs. fitted values


plot(residuals(mod_nbin, type = "deviance"), fitted(mod_nbin))

# 2 plot deviance residuals against your independent variables

plot(residuals(mod_nbin, type = "deviance"), AAR$D.PARK)

# If you have variables that you have not included, or information of times or spatial locations
# also plot these against the residual deviance.. 


#### a little plotting

pred_ambh <- predict(mod_nbin, type = "response", se.fit = TRUE)
str(pred_ambh)

#### plot with standard error
plot(AAR$TOT.N~AAR$D.PARK, ylab = "No. of roadkill", xlab = "Distance from park")
lines(pred_ambh$fit~AAR$D.PARK, type = "l", col = "red", lwd = 1.5)
lines(pred_ambh$fit +pred_ambh$se.fit~AAR$D.PARK, type = "l", col = "blue", lwd = 1, lty = 2) # upper standard error
lines(pred_ambh$fit -pred_ambh$se.fit~AAR$D.PARK, type = "l", col = "blue", lwd = 1, lty = 2) # lower standard error

### with 95% confidence
lines(pred_ambh$fit +pred_ambh$se.fit*1.96~AAR$D.PARK, type = "l", col = "black", lwd = 1, lty = 3) # upper confidence interval
lines(pred_ambh$fit -pred_ambh$se.fit*1.96~AAR$D.PARK, type = "l", col = "black", lwd = 1, lty = 3) # lower confidence interval

########## Another count example (zero-inflation) -----

birds<- read.csv("birds.csv")

str(birds)
names(birds)
summary(birds)

#####
hist(birds$MP, breaks = 10)
summary(birds$MP)

length(birds$MP)
length(which(birds$MP==0))

####

mp_mod <- glm(MP~Burnt*Habitat + Site, data = birds, family = "poisson"(link = "log"))

##### check for overdispersion

theta <- mp_mod$deviance/mp_mod$df.residual
theta

##### let's look at zero-inflation

hist(birds$MP)

summary(birds$MP)

prop_0 <- length(which(birds$MP==0))/length(birds$MP)
prop_0

######  try the model with no 0 data ....

mp_mod_2 <- glm(MP~Burnt*Habitat + Site, data = birds[birds$MP != 0,], family = "poisson"(link = "log"))


#### overdispersion test

theta <- mp_mod_2$deviance/mp_mod_2$df.residual
theta

#### zero inflation causes a large amount of the over-dispersion, hence fit Zeroinflated model
install.packages("pscl")
library("pscl")

ZAPmod <- hurdle(MP~Burnt*Habitat + Site|Burnt*Habitat + Site, data = birds, dist = "poisson", link = "logit")


### compare AIC with non-zeroaltered
AIC(ZAPmod)
AIC(mp_mod)


summary(ZAPmod)


########### a presence absence example

MP_PA[MP_PA > 0] <- 1
birds$MP_PA <- MP_PA

#### a presence absence model.. 


mp_mod_pa <- glm(MP_PA~Burnt*Habitat + Site, data = birds, family = "binomial"(link = "logit"))
mp_mod_pa

theta <- mp_mod_pa$deviance/mp_mod_pa$df.residual
theta

################## rule of thumb is if theta > 1.5 try quassi-binomial

#####################