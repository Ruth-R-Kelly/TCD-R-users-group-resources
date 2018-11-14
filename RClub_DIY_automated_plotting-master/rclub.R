
# R CLUB 14th NOV 2018

# DIY automated data visualisation

# All code and functions in rclub.R and rclub.RData written by Annabel Smith, unless otherwise indicated

# Please report bugs or problems to: annabel@smithecology.org

# ------------------------------->
# --- SET-UP:

# Load data and functions:
load("rclub.RData")

# Load analysis packages:
library(lme4)
library(AICcmodavg) # this has the predictSE function that we'll use

# ------------------------------->
# --- MODEL ESTIMATES with PREDICT:

# The enormous plant invader model:
summary(top.step2)

# The data have been removed (unpublished):
# It consists of 8792 observations and 45 columns containing response and predictor variables:
head(coloniseSc,2)

# Here is a small sample so you can see the structure:
head(coloniseSc_sample,2); dim(coloniseSc_sample)

# Let's make it simpler for the demonstration (model already run):
m1<-glmer(pr ~ yr*lifespan + (1 + NO3soil | sp) + (1 | plot/plsub), data=coloniseSc, family=binomial)
summary(m1)

# Use predict to get estimates for the model, for annual and perennial plants in all years:

# Start by making a 'newdata' frame:
nd1<-data.frame(yr=seq(0,20,1),lifespan=rep(c("A","P"),each=21))
pr1<-predictSE(m1,newdata=nd1,type="response")

# calculate approximate CIs:
res1<-data.frame(nd1,fit=pr1$fit,se=pr1$se.fit,lci=pr1$fit-(pr1$se.fit*1.98),uci=pr1$fit+(pr1$se.fit*1.98))

# make a simple plot:
dev.new(title="",width=8,height=8, dpi=80, pointsize=18)
par(mar=c(5,5,1,1))
plot(res1$yr[res1$lifespan=="A"], res1$fit[res1$lifespan=="A"],type="l",lty=1, ylim=c(0, 1), xlab="Year", ylab="Probability of colonisation")
lines(res1$yr[res1$lifespan=="P"], res1$fit[res1$lifespan=="P"],lty=2)
pg.ci("yr","res1","lifespan",col=rgb(0,0,0,0.15))
legend("topleft",legend=c("Annual","Perennial"),lty=c(1,2))

# ------------------------------->
# --- MODEL ESTIMATES by HAND:

# Make sure we know how predict works:
summary(m1)$coefficients

# We have a 20 year data set, and want to know how perennial and annual invaders colonise over this time period. 

# Year was coded from zero onwards, for scaling and ease of interpretation
all_years<-unique(coloniseSc$yr)

# model estimates:
summary(m1)$coefficients
ests<-summary(m1)$coefficients[,1]
ests

# to get estimates on the "response" scale, predict backtransforms estimates, depending on the type of model used, e.g. binomial (logit), poisson (log), gaussian (normal)

# our model is binomial, so we'll use the inverse of the logit function:

# inverse logit function for binomial models:
# logit() == log(x/(1 - x))

iv1<-function(x) 1/(1 + exp(-x))
iv2<-function(x) exp(x)/(1+exp(x))

# check:
iv1(ests[1])
iv2(ests[1])

# do the calculations:

# year zero, perennial:
iv1(ests[1])

# year one, perennial:
iv1(ests[1] + ests[2])

# year two, perennial:
iv1(ests[1] + (ests[2]*2))

# year fifteen, perennial:
iv1(ests[1] + (ests[2]*15))

# all years, perennial:
iv1(ests[1] + (ests[2]*seq(0,20,1)))

# make sure they're the same as what we found with predict:
res1$fit[res1$lifespan=="P"]

# do the same for annual
ests

# annual, year zero:
iv1(ests[1] + ests[3])

# annual, year one
iv1(ests[1] + ests[2] + ests[3] + ests[4])

# annual, year 15, etc:
iv1(ests[1] + (ests[2]*15) + ests[3] + (ests[4]*15))

# all years, annual:
iv1(ests[1] + (ests[2]*seq(0,20,1)) + ests[3] + (ests[4]*seq(0,20,1)))

# make sure they're the same as what we found with predict:
res1$fit[res1$lifespan=="A"]

# ------------------------------->
# --- ESTIMATES FROM THE BIG MODEL:

# now that we can use predict with confidence, we can automate plotting from the big model....

# The enormous plant invader model:
summary(top.step2)

# We want to make a plot for each variable that had a signficant effect on plant invasion

# three functions were used to:

# (1) extract and tidy the coefficient table from the model summary (coef.ext)
# (2) calculate approximate CIs (CItrans)
# (3) combine the two (coef.sum)

# Check each one to see what they do:
coef.ext
CItrans
coef.sum 

# This is the resulting coefficient table:
head(step2_top_coef)

# this is a hand-made table, which we will use to store the estimates for each term:
nd.s3

# Next, we'll get estimates with estimates.nd function for term:
estimates.nd

# This one looks complex, but that reflects the complexity of, not just this model, but of the entire analytical approach in which we were plotting many models, each with several terms of different types (e.g. numeric, factor, main effects, interactions)

# However, the concept is straight forward - we're doing exactly what we did above with predictSE, but for each term separately

# The estimates.nd function loops through each term in the model to estimate its effect on plant invasion using predictSE. 

# The newdata frame is made each time for each term, keeping all other variables constant and at zero. This means that we can interpret each term in the model, independently of the other terms.

# There are a series of if functions to determine if the term is a factor or a numeric variable and if it is a main effect or an interaction.

# Getting estimates from the model is not challenging. The challenge here was that the newdata frame has be be different for each term. Most of what the estimates.nd function does is format the newdata frame.  

# The arguments are: the model, the xaxis.term, the data scaled and the data unscaled

# The estimates are stored as objects for each term, and can be accessed by the name in the nd.s3 table. You could also store them as a list or however you prefer. 

# (uses full data set, so will not run, but it is save in workspace):

for (i in 1:length(nd.s3[,1])){
	
	if (nd.s3$interaction[i]=="main"){
		assign(nd.s3$df.name[i],estimates.nd("top.step2",xaxis.term=nd.s3$xaxis.term[i],modelled.data="coloniseSc",unscaled.data="col.unscaled"))
		} # close if main

	if (nd.s3$interaction[i]=="int"){
	assign(nd.s3$df.name[i],estimates.nd("top.step2", xaxis.term=nd.s3$xaxis.term[i],int.term=nd.s3$int.term[i],modelled.data="coloniseSc",unscaled.data="col.unscaled"))
		} # close if interaction

	} # close i for data frame loop


# ------------------------------->
# --- PLOT:

# The plot code will use a hand-made table, based on nd.s3 above, but with columns added for the axis labels. These have been formatted exactly as I want them to appear on the plots. It contains all of the terms in the model that were significant. These are the ones we want to plot:
step2.plot2

# As we saw in the section above, the estimates for each term were stored as objects named in the 'data' column of the table, e.g:
head(get(as.character(step2.plot2$data[1])))

# The code will use the pg.ci function that I wrote to plot the CIs as polygons
pg.ci

# But other than that, the rest is really simple base code; the axes are already formatted in the table above

# This is mac code, so might not look pretty in R Studio or other platforms. I have included a PDF in the zip folder so you can see what it should look like

dev.new(title="",width=11.69,height=8.27, dpi=64, pointsize=19)
par(mfrow=c(3,4),mar=c(4,4,1.5,1), pch=20, las=1, bty="l", mgp=c(2.7,1,0),oma=c(0,0,1,0))

for (i in 1:length(step2.plot2[,1])){
	
	data.thisrun<-get(as.character(step2.plot2$data[i]))
	plot.x<-data.thisrun[,as.character(step2.plot2$x.variable[i])]
	plot.y<-data.thisrun[,"fit"]

	plot(plot.x, plot.y, type="n", ylab="Invader occupancy", xlab="", ylim=c(0,1))
	
	if(step2.plot2$x.label[i]=="Soil NO3") title(xlab=expression(paste("Soil ", NO[3]),mgp=c(2.2,1,0))) else title(xlab=as.character(step2.plot2$x.label[i]),mgp=c(2.2,1,0))

	if (step2.plot2$interaction[i]=="main"){
pg.ci(as.character(step2.plot2$x.variable[i]),as.character(step2.plot2$data[i]),colour=rgb(0,0,0,0.1))
lines(plot.x, plot.y)
	} # close if main

	if (step2.plot2$interaction[i]=="int"){
		pg.ci(as.character(step2.plot2$x.variable[i]),as.character(step2.plot2$data[i]),x.subset=as.character(step2.plot2$x.subset[i]),colour=rgb(0,0,0,0.1))

	sub1<-unique(data.thisrun[,as.character(step2.plot2$x.subset[i])])[1]
	sub2<-unique(data.thisrun[,as.character(step2.plot2$x.subset[i])])[2]

	linex.lev1<-data.thisrun[,as.character(step2.plot2$x.variable[i])][data.thisrun[,as.character(step2.plot2$x.subset[i])]==sub1]
	liney.lev1<-data.thisrun$fit[data.thisrun[,as.character(step2.plot2$x.subset[i])]==sub1]

	lines(linex.lev1, liney.lev1, lty=1)

	linex.lev2<-data.thisrun[,as.character(step2.plot2$x.variable[i])][data.thisrun[,as.character(step2.plot2$x.subset[i])]==sub2]
	liney.lev2<-data.thisrun$fit[data.thisrun[,as.character(step2.plot2$x.subset[i])]==sub2]

	lines(linex.lev2, liney.lev2, lty=2)

	par(xpd=NA)
legend(par("usr")[1],1.1,legend=c(as.character(step2.plot2$x1.label[i]),as.character(step2.plot2$x2.label[i])),lty=c(1,2),bty="n")
par(xpd=F)

	} # close if interaction

	par(xpd=NA)
	text(par("usr")[1],par("usr")[4]+0.15,paste("(",letters[i],")",sep=""),cex=1.2)
	par(xpd=F)

} # close plot for


# ------------------------------->
# --- MORE INTERACTIONS:

head(st1sc,2) # data, predictors scaled
head(st1,2) # data unscaled

gdmod<-lm(ar_adapt~st*native, data=st1sc)
summary(gdmod); nobs(gdmod)

# model estimates:
head(st.ests); dim(st.ests)

# coefficients:
st.coef

dev.new(title="",width=7,height=7, dpi=80, pointsize=18)
par(mfrow=c(1,1),mgp=c(3.5,1,0),mar=c(5,5,1,1))

plot(st.ests$pred.data[st.ests$native=="native"],st.ests$fit[st.ests$native=="native"], type="l", ylim=c(min(c(st.ests$lci,st1sc$ar_adapt)),max(c(st.ests$uci,st1sc$ar_adapt))),las=1, ylab="Genetic diversity", xlab="",xaxt="n")
lines(st.ests$pred.data[st.ests$native=="non_native"],st.ests$fit[st.ests$native=="non_native"],lty=2)

xax<-seq(round(min(st1sc[,"st"]),0),round(max(st1sc[,"st"]),0),length.out=5)
xaxl<-round(seq(round(min(st1[,"st"]),0),round(max(st1[,"st"]),0),length.out=5),0)

axis(side=1, at=xax, labels=xaxl)
title(xlab="Temperature seasonality",mgp=c(2.7,1,0))
pg.ci("pred.data","st.ests",x.subset="native",colour=rgb(0,0,0,0.1))

points(st1sc[st1sc$native=="native","st"],st1sc[st1sc$native=="native","ar_adapt"],col="black",pch=20)
points(st1sc[st1sc$native=="non_native","st"],st1sc[st1sc$native=="non_native","ar_adapt"],col="red", pch=20)

legend("bottomright",legend=c("native","non native"),lty=c(1,2))
legend("bottomright",legend=c("",""),pch=20, col=c("black","red"),bty="n",inset=c(0.26,0))









































