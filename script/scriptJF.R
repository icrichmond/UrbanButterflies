#importing data
butterfly <-read.csv(file.choose())
#checking data
butterfly
ls.str()
summary(butterfly)
attach(butterfly)
===================================================================
#Initial data visualization

#correlation of the plant data with mary annes
cor.test(butterfly$portionbloom_native, butterfly$X.native,
 alternative="two.sided", method="pearson")
#checking corellation of independent variables
plot(X.bloom_area ~ portionbloom_native)
cor.test(X.bloom_area, portionbloom_native,
 alternative="two.sided", method="pearson")
plot(portionbloom_native ~ plantsbloom_richness)
cor.test(portionbloom_native, plantsbloom_richness,
 alternative="two.sided", method="pearson")
plot(X.bloom_area ~ plantsbloom_richness)
cor.test(X.bloom_area, plantsbloom_richness,
 alternative="two.sided", method="pearson")
plot(X.bloom_area ~ sampling.area)
cor.test(X.bloom_area, sampling.area,
 alternative="two.sided", method="pearson")
plot(sampling.area ~ plantsbloom_richness)
cor.test(sampling.area, plantsbloom_richness,
 alternative="two.sided", method="pearson")
plot(sampling.area ~ portionbloom_native)
cor.test(sampling.area, portionbloom_native,
 alternative="two.sided", method="pearson")
#does not look like there is any corilation beteween the independent variables
__________________________________________________________________________
#checking normality with frequency distribution histograms

#plants portion native in bloom
hist(portionbloom_native)
require(ggplot2)
citation("ggplot2")
mygraph <- ggplot(butterfly, aes(x = portionbloom_native))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$portionbloom_native),
 sd = sd(butterfly$portionbloom_native)
 ),
 color = "red")
mygraph
#more chacks for narmality
qqnorm(portionbloom_native)
qqline(portionbloom_native)
shapiro.test(portionbloom_native)
#plants seem normal
#percent area in bloom
#Normality
require(ggplot2)
mygraph <- ggplot(butterfly, aes(x = X.bloom_area))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$X.bloom_area),
 sd = sd(butterfly$X.bloom_area)
 ),
 color = "red")
mygraph
qqnorm(X.bloom_area)
qqline(X.bloom_area)
shapiro.test(X.bloom_area)
#seems to be normal
#plant richness
#Normality
require(ggplot2)
mygraph <- ggplot(butterfly, aes(x = plantsbloom_richness))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$plantsbloom_richness),
 sd = sd(butterfly$plantsbloom_richness)
 ),
 color = "red")
mygraph
qqnorm(plantsbloom_richness)
qqline(plantsbloom_richness)
shapiro.test(plantsbloom_richness)
#plant richness native
#Normality
require(ggplot2)
mygraph <- ggplot(butterfly, aes(x = plantsbloomnrichness))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$plantsbloomnrichness),
 sd = sd(butterfly$plantsbloomnrichness)
 ),
 color = "red")
mygraph
qqnorm(plantsbloomnrichness)
qqline(plantsbloomnrichness)
shapiro.test(plantsbloomnrichness)
#portion native richness
#Normality
require(ggplot2)
mygraph <- ggplot(butterfly, aes(x = portionbloom_richnessnative))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$portionbloom_richnessnative),
 sd = sd(butterfly$portionbloom_richnessnative)
 ),
 color = "red")
mygraph
qqnorm(portionbloom_richnessnative)
qqline(portionbloom_richnessnative)
shapiro.test(portionbloom_richnessnative)
#Butterfly shannon index
#Normality
mygraph <- ggplot(butterfly, aes(x = Butterfly_shannon))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$Butterfly_shannon),
 sd = sd(butterfly$Butterfly_shannon)
 ),
 color = "red")
mygraph
qqnorm(residuals(Butterfly_shannon))
qqline(Butterfly_shannon)
shapiro.test(Butterfly_shannon)
#the data seems to stray from normal try log transformation
qqnorm(log(Butterfly_shannon))
qqline(log(Butterfly_shannon))
shapiro.test(log(Butterfly_shannon))
#log transformation was less normal should I do a quadratic transformation
qqnorm((Butterfly_shannon)^2)
qqline((Butterfly_shannon)^2)
shapiro.test(Butterfly_shannon^2)
mygraph <- ggplot(butterfly, aes(x = Butterfly_shannon^2))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$Butterfly_shannon^2),
 sd = sd(butterfly$Butterfly_shannon^2)
 ),
 color = "red")
mygraph
#this is more normal but isnt used often
#shannon eveness
qqnorm(shannon_eveness)
qqline(shannon_eveness)
shapiro.test(shannon_eveness)
mygraph <- ggplot(butterfly, aes(x = shannon_eveness))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$shannon_eveness),
 sd = sd(butterfly$shannon_eveness)
 ),
 color = "red")
mygraph
#simpson index
#normality
mygraph <- ggplot(butterfly, aes(x = simpson_index))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$simpson_index),
 sd = sd(butterfly$simpson_index)
 ),
 color = "red")
mygraph
qqnorm(simpson_index)
qqline(simpson_index)
shapiro.test(simpson_index)
#simpson dominance
mygraph <- ggplot(butterfly, aes(x = simpsons._dominance))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$simpsons._dominance),
 sd = sd(butterfly$simpsons._dominance)
 ),
 color = "red")
mygraph
qqnorm(simpsons._dominance)
qqline(simpsons._dominance)
shapiro.test(simpsons._dominance)
# simpsons eveness
mygraph <- ggplot(butterfly, aes(x = simpsons._eveness))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$simpsons._eveness),
 sd = sd(butterfly$simpsons._eveness)
 ),
 color = "red")
mygraph
qqnorm(simpsons._eveness)
qqline(simpsons._eveness)
shapiro.test(simpsons._eveness)
#
#butterfly abundance
hist(Butterfly_abundance)
hist(sqrt(Butterfly_abundance))
hist(log(Butterfly_abundance))
#Normality
mygraph <- ggplot(butterfly, aes(x = Butterfly_abundance))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$Butterfly_abundance),
 sd = sd(butterfly$Butterfly_abundance)
 ),
 color = "red")
mygraph
qqnorm(Butterfly_abundance)
qqline(Butterfly_abundance)
shapiro.test(Butterfly_abundance)
# fallows assumptions
# sqrt butterfly abundance
mygraph <- ggplot(butterfly, aes(x = sqrt(Butterfly_abundance)))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(sqrt(Butterfly_abundance)),
 sd = sd(sqrt(Butterfly_abundance))
 ),
 color = "red")
mygraph
qqnorm(sqrt(Butterfly_abundance))
qqline(sqrt(Butterfly_abundance))
shapiro.test(sqrt(Butterfly_abundance))
# butterfly richness
#Normality
mygraph <- ggplot(butterfly, aes(x = Butterfly_richness))
mygraph <- mygraph +
 geom_density() +
 geom_rug() +
 geom_histogram(aes(y = ..density..),
 color = "black",
 alpha = 0.3) +
 stat_function(fun = dnorm,
 args = list(
 mean = mean(butterfly$Butterfly_richness),
 sd = sd(butterfly$Butterfly_richness)
 ),
 color = "red")
mygraph
qqnorm(Butterfly_richness)
qqline(Butterfly_richness)
shapiro.test(Butterfly_richness)
#fallows assumptions

#compairing disturbance and butterfly diversity
boxplot(Butterfly_abundance, notch=FALSE)
boxplot(sqrt(Butterfly_abundance), notch=FALSE)
boxplot(Butterfly_richness, notch=FALSE)
===========================================================================
#Analysis, setting up a general linear model and testing assumptions.


#for shannon analysis
#selecting model
shannonfullmodel<-lm(Butterfly_shannon ~ portionbloom_native + 
 disturbance_category + X.bloom_area + plantsbloom_richness + sampling.area)
library(MASS)
citation("MASS")
stepshannon <- stepAIC(shannonfullmodel, direction="both")
stepshannon$anova # display results
summary (stepshannon)
#using AICc
install.packages ("AICcmodavg", dependencies=TRUE)
local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
+ if(nchar(pkg)) library(pkg, character.only=TRUE)})
citation("AICcmodavg")
#making list of models
shannonmodel <- list( )
shannonmodel[[1]]<- lm(Butterfly_shannon ~ disturbance_category)
shannonmodel[[2]]<-lm(Butterfly_shannon ~ portionbloom_native)
shannonmodel[[3]]<-lm(Butterfly_shannon ~ X.bloom_area)
shannonmodel[[4]]<-lm(Butterfly_shannon ~ plantsbloom_richness)
shannonmodel[[5]]<-lm(Butterfly_shannon ~ portionbloom_native + disturbance_category)
shannonmodel[[6]]<-lm(Butterfly_shannon ~ disturbance_category + X.bloom_area)
shannonmodel[[7]]<-lm(Butterfly_shannon ~ disturbance_category + plantsbloom_richness)
shannonmodel[[8]]<-lm(Butterfly_shannon ~ portionbloom_native + 
 disturbance_category + X.bloom_area)
shannonmodel[[9]]<-lm(Butterfly_shannon ~ disturbance_category +
 X.bloom_area + plantsbloom_richness)
shannonmodel[[10]]<-lm(Butterfly_shannon ~ portionbloom_native + 
 disturbance_category + plantsbloom_richness)
shannonmodel[[11]]<-lm(Butterfly_shannon ~  sampling.area)
shannonmodel[[12]]<-lm(Butterfly_shannon ~  disturbance_category +
 sampling.area)
#creating table
aictab(shannonmodel, modnames = NULL, second.ord = TRUE, nobs = NULL,
       sort = TRUE,)
#forward
model.nullshannon<-lm((Butterfly_shannon) ~ 1, data = butterfly)
stepshannon <- stepAIC(model.nullshannon, scope=~. + portionbloom_native + 
 disturbance_category + X.bloom_area + plantsbloom_richness + sampling.area)
stepshannon$anova
#manual forward
shannonmodel1<-lm(Butterfly_shannon ~ disturbance_category)
shannonmodel2<-lm(Butterfly_shannon ~ portionbloom_native)
shannonmodel3<-lm(Butterfly_shannon ~ X.bloom_area)
shannonmodel4<-lm(Butterfly_shannon ~ plantsbloom_richness)
shannonmodel5<-lm(Butterfly_shannon ~ portionbloom_native + disturbance_category)
shannonmodel6<-lm(Butterfly_shannon ~ disturbance_category + X.bloom_area)
shannonmodel7<-lm(Butterfly_shannon ~ disturbance_category + plantsbloom_richness)
shannonmodel8<-lm(Butterfly_shannon ~ portionbloom_native + 
 disturbance_category + X.bloom_area)
shannonmodel9<-lm(Butterfly_shannon ~ disturbance_category +
 X.bloom_area + plantsbloom_richness)
shannonmodel10<-lm(Butterfly_shannon ~ portionbloom_native + 
 disturbance_category + plantsbloom_richness)
summary(shannonmodel1)
summary(shannonmodel2)
summary(shannonmodel3)
summary(shannonmodel4)
summary(shannonmodel5)
summary(shannonmodel6)
summary(shannonmodel7)
summary(shannonmodel8)
summary(shannonmodel9)
summary(shannonmodel10)
summary(shannonfullmodel)
#checking assumptions shannonmodel1
par(mfrow = c(2, 2), las=1)
plot(shannonmodel1)
library(lmtest)
citation("lmtest")
bptest(shannonmodel1)
shapiro.test(residuals(shannonmodel1))
#assumption of homoscedactisity is not good and there is an outlier.
#t.test for unequal variances model 1
t.test(Butterfly_shannon ~ disturbance_category, alternative='two.sided',
 conf.level=.95, var.equal=FALSE)
-----------------------------------
#boxplot figure
par(mar=c(3,5,1,1),lwd=1)
Boxshan<- boxplot(Butterfly_shannon~disturbance_category, 
 ylab='Shannon Wiener Diversity Index', names=c('Undisturbed','Disturbed'),
 notch=FALSE, cex=1, cex.lab=1, cex.axis=1)
box(lwd=1)
my_ci<-'-0.0467 to 1.457'
my_p<-0.0613
mylabel=bquote(italic(R)^2==.(format(my_p,digits=3)))
rp=vector('expression',2)
rp[1]=substitute(expression(italic(CI)==MYVALUE),list(MYVALUE=format(my_ci,dig=3)))[2]
rp[2]=substitute(expression(italic(p)==MYOTHERVALUE),list(MYOTHERVALUE=format(my_p,digits=3)))[2]
legend('topright',cex=1,legend=rp,bty='n',y.intersp=1)

sd(disturbedsites$Butterfly_shannon)
sd(undistsites$Butterfly_shannon)
q <- mean(disturbedsites$Butterfly_shannon)
w <-mean(undistsites$Butterfly_shannon)
w-q

mshan<- mean(Butterfly_shannon)
deltashan<- w-q
deltashan/mshan*100

#checking effect of outlier
butsub<- butterfly[-c(1,8),]
butsub
shannonsubmod<- lm(Butterfly_shannon ~ disturbance_category, data=butsub)
shannonsubmodlist<- list( )
shannonsubmodlist[[1]]<- lm(Butterfly_shannon ~ disturbance_category, data=butsub)
shannonsubmodlist[[2]]<-lm(Butterfly_shannon ~ portionbloom_native, data=butsub)
shannonsubmodlist[[3]]<-lm(Butterfly_shannon ~ X.bloom_area,data=butsub)
shannonsubmodlist[[4]]<-lm(Butterfly_shannon ~ plantsbloom_richness,data=butsub)
shannonsubmodlist[[5]]<-lm(Butterfly_shannon ~ portionbloom_native + disturbance_category, data=butsub)
shannonsubmodlist[[6]]<-lm(Butterfly_shannon ~ disturbance_category + X.bloom_area, data=butsub)
shannonsubmodlist[[7]]<-lm(Butterfly_shannon ~ disturbance_category + plantsbloom_richness, data=butsub)
shannonsubmodlist[[8]]<-lm(Butterfly_shannon ~ portionbloom_native + 
 disturbance_category + X.bloom_area, data=butsub)
shannonsubmodlist[[9]]<-lm(Butterfly_shannon ~ disturbance_category +
 X.bloom_area + plantsbloom_richness, data=butsub)
shannonsubmodlist[[10]]<-lm(Butterfly_shannon ~ portionbloom_native + 
 disturbance_category + plantsbloom_richness, data=butsub)
shannonsubmodlist[[11]]<-lm(Butterfly_shannon ~  sampling.area, data=butsub)
shannonsubmodlist[[12]]<-lm(Butterfly_shannon ~  disturbance_category +
 sampling.area, data=butsub)
aictab(shannonsubmodlist, modnames = NULL, second.ord = TRUE, nobs = NULL,
       sort = TRUE,)
summary(shannonsubmod)
------------------------------------
#figure assumptions
par(mfrow = c(2,2), las=1)
plot(shannonsubmod)

bptest(shannonsubmod)
shapiro.test(residuals(shannonsubmod))
t.test(Butterfly_shannon ~ disturbance_category, alternative='two.sided',
 conf.level=.95, var.equal=TRUE, data=butsub)
-------------------------------------
#figure
par(mar=c(3,5,1,1),lwd=2)
Boxshansub<- boxplot(Butterfly_shannon~disturbance_category, 
 ylab='Shannon Wiener Diversity Index', names=c('Undisturbed','Disturbed'),
 notch=FALSE, cex=1, cex.lab=1, cex.axis=1, data=butsub)
box(lwd=1)
my_ci<-'0.607 to 1.75'
my_p<-'1.96e-06'
mylabel=bquote(italic(R)^2==.(format(my_p,digits=3)))
rp=vector('expression',2)
rp[1]=substitute(expression(italic(CI)==MYVALUE),list(MYVALUE=format(my_ci,dig=3)))[2]
rp[2]=substitute(expression(italic(p)==MYOTHERVALUE),list(MYOTHERVALUE=format(my_p,digits=3)))[2]
legend('topright',cex=1,legend=rp,bty='n',y.intersp=1)
#Fixes assumptions and changes it to being significant however outliers 
#were 33% of my disturbed data
___________________________________________________________________________
#analysis with root abundance
#fullmodel
fullmodel1<-lm((Butterfly_abundance^1/2) ~ portionbloom_native + 
 disturbance_category + X.bloom_area + plantsbloom_richness + sampling.area)
# Stepwise Regression model selection for abundance
#backwards
step1<- stepAIC(fullmodel1, direction="both")
step1$anova # display results
write.csv(step1$anova)
#forward
model.null<-lm(sqrt(Butterfly_abundance) ~ 1, data = butterfly)
step <- stepAIC(model.null, scope=~. + portionbloom_native + 
 disturbance_category + X.bloom_area + plantsbloom_richness + sampling.area,
 direction="forward")
step$anova 
#model selection with AICc
#making list of models
abundancemodel <- list( )
abundancemodel[[1]]<-lm(Butterfly_abundance ~ disturbance_category)
abundancemodel[[2]]<-lm(Butterfly_abundance ~ portionbloom_native)
abundancemodel[[3]]<-lm(Butterfly_abundance ~ X.bloom_area)
abundancemodel[[4]]<-lm(Butterfly_abundance ~ plantsbloom_richness)
abundancemodel[[5]]<-lm(Butterfly_abundance ~ portionbloom_native + disturbance_category)
abundancemodel[[6]]<-lm(Butterfly_abundance ~ disturbance_category + X.bloom_area)
abundancemodel[[7]]<-lm(Butterfly_abundance ~ disturbance_category + plantsbloom_richness)
abundancemodel[[8]]<-lm(Butterfly_abundance ~ portionbloom_native + 
 disturbance_category + X.bloom_area)
abundancemodel[[9]]<-lm(Butterfly_abundance ~ disturbance_category +
 X.bloom_area + plantsbloom_richness)
abundancemodel[[10]]<-lm(Butterfly_abundance ~ portionbloom_native + 
 disturbance_category + plantsbloom_richness)
abundancemodel[[11]]<-lm(Butterfly_abundance ~  sampling.area)
abundancemodel[[12]]<-lm(Butterfly_abundance ~  disturbance_category + X.bloom_area +
 sampling.area)
#creating table
aictab(abundancemodel, modnames = NULL, second.ord = TRUE, nobs = NULL,
       sort = TRUE,)
#final model
#this was selected by AIC
model1<-lm(sqrt(Butterfly_abundance) ~ X.bloom_area + disturbance_category + 
    sampling.area)
summary(model1) 
#this was selected by AICc
model2<-lm(sqrt(Butterfly_abundance) ~ X.bloom_area + disturbance_category)
summary(model2)
#acording to model selection model1 is the best fit
#checking assumptions on best fit model1
par(mfrow = c(2, 2), las=1)
plot(model1)
library(lmtest)
bptest(model1)
dwtest(model1)
resettest(model1)
shapiro.test(residuals(model1))

#model2 assumptions figure
------------------------------
par(mfrow = c(2, 2), las=1)
plot(model2)

library(lmtest)
bptest(model2)
dwtest(model2)
resettest(model2)
shapiro.test(residuals(model2))
------------------------------
#Graphing model bloom area #figure not used
mydata<-butterfly
require(ggplot2)
myplot<-ggplot(data=mydata, aes(x=X.bloom_area,
y=sqrt(Butterfly_abundance)))+facet_grid(.~disturbance_category)+geom_point()
myplot
myplot<-myplot+
 stat_smooth(method = lm, se=FALSE)
myplot
update_labels(myplot, list(
 y = expression(sqrt~(abundqnce)),
 x = expression(portion_blooming)))
#graphing site area
mydata<-butterfly
require(ggplot2)
myplot<-ggplot(data=mydata, aes(x=sampling.area,
y=sqrt(Butterfly_abundance)))+facet_grid(.~disturbance_category)+geom_point()
myplot
myplot<-myplot+
 stat_smooth(method = lm, se=FALSE)
myplot
update_labels(myplot, list(
 y = expression(sqrt~(abundqnce)),
 x = expression(Sampling Area)))
#probably dont need to report this graph. use maryannes graph below
#ancova on abundance
require(car)
Anova(model2, type = 3)
-----------------------------------
#checking regression %bloom and graphing from maryanne
#figure
par(mar=c(5,6,1,1), mfrow=c(1,1))
mod1 = lm(sqrt(Butterfly_abundance)~X.bloom_area, 
 data=subset(butterfly,disturbance_category==1))
plot(X.bloom_area,sqrt(Butterfly_abundance), #cex=1.5,cex.lab=2,cex.axis=2,
 pch = 20, type = 'p', col=as.factor(disturbance_category),
 las = 1,xlab=expression("Percent of area in bloom"),
 ylab=expression(sqrt("Butterfly abundance")))
summary(mod1)
#box(lwd=4)
modsum=summary(mod1)
r2=modsum$adj.r.squared
modsum$coefficients
my.p=modsum$coefficients[2,4]
mylabel=bquote(italic(R)^2==.(format(r2,digits=3)))
rp=vector('expression',2)
rp[1]=substitute(expression(italic(R)^2==MYVALUE),list(MYVALUE=format(r2,dig=3)))[2]
rp[2]=substitute(expression(italic(p)==MYOTHERVALUE),list(MYOTHERVALUE=format(my.p,digits=3)))[2]
legend('right',cex=1,legend=rp,bty='n',y.intersp=1)
legend('topleft',c("Undisturbed","Disturbed"),cex=1,pch=20,col=c("black","red"),bty="n")
swp<-subset(X.bloom_area, disturbance_category==1)
lines(swp,fitted(mod1),col="black",lwd=2)
mod2 = lm(sqrt(Butterfly_abundance)~X.bloom_area, 
 data=subset(butterfly,disturbance_category==2))
modsum2=summary(mod2)
r2=modsum2$adj.r.squared
modsum2$coefficients
my.p=modsum2$coefficients[2,4]
mylabel2=bquote(italic(R)^2==.(format(r2,digits=3)))
rp=vector('expression',2)
rp[1]=substitute(expression(italic(R)^2==MYVALUE),list(MYVALUE=format(r2,dig=3)))[2]
rp[2]=substitute(expression(italic(p)==MYOTHERVALUE),list(MYOTHERVALUE=format(my.p,digits=3)))[2]
swp<-subset(X.bloom_area, disturbance_category==2)
lines(swp,fitted(mod2),col="red", lwd=2)
legend('bottomright',cex=1,legend=rp,bty='n',y.intersp=1, col=33)
------------------------------------
#figure
#regression with all together
par(mar=c(5,5,2,1),mfrow=c(1,1))
moda = lm(sqrt(Butterfly_abundance)~X.bloom_area)
plot(X.bloom_area,sqrt(Butterfly_abundance),
 pch = 20, type = 'p', col=as.factor(disturbance_category),
 las = 1,xlab=expression("Percent of area in bloom"),
 ylab=expression(sqrt("Butterfly abundance")))
modsuma=summary(moda)
r2=modsuma$adj.r.squared
modsuma$coefficients
my.p=modsuma$coefficients[2,4]
mylabel=bquote(italic(R)^2==.(format(r2,digits=3)))
rp=vector('expression',2)
rp[1]=substitute(expression(italic(R)^2==MYVALUE),list(MYVALUE=format(r2,dig=3)))[2]
rp[2]=substitute(expression(italic(p)==MYOTHERVALUE),list(MYOTHERVALUE=format(my.p,digits=3)))[2]
legend('topleft',cex=1,legend=rp,bty='n',y.intersp=1.5)
legend('bottomright',c("Undisturbed","Disturbed"),cex=1,pch=20,col=c("black","red"),bty="n")
jes<-(X.bloom_area)
lines(jes,fitted(moda),col="black")

summary(mod2)
# checking means
disturbedsites<- subset(butterfly, disturbance_category=="2")
undistsites<- subset(butterfly, disturbance_category=="1")
md<-mean(disturbedsites$Butterfly_abundance)
ud<-mean(undistsites$Butterfly_abundance)
ud-md
summary(ud)
summary(md)
sd(disturbedsites$Butterfly_abundance)
sd(undistsites$Butterfly_abundance)
md
ud
mabun<- mean(Butterfly_abundance)
dabun<-ud-md
dabun/mabun*100
mabun
#for CI of difference
t.test(Butterfly_abundance ~ disturbance_category, alternative='two.sided',
 conf.level=.95 , var.equal=TRUE)
#comparing flowers in bloom vs disturbance
t.test( X.bloom_area ~ disturbance_category, alternative='two.sided',
 conf.level=.95 , var.equal=TRUE)
---------------------------------------
#figure
par(mar=c(3,5,1,1),lwd=2)
boxplot(X.bloom_area ~ disturbance_category, ylab = 'Area in bloom', 
 names=c('Undisturbed','Disturbed'), notch=FALSE,cex.lab=1,cex.axis=1)
box(lwd=1)
my_cir<-'0.250 to 9.84'
my_pr<-0.0402
mylabel=bquote(italic(R)^2==.(format(my.p,digits=3)))
rp=vector('expression',2)
rp[1]=substitute(expression(italic(CI)==MYVALUE),list(MYVALUE=format(my_cir,dig=3)))[2]
rp[2]=substitute(expression(italic(p)==MYOTHERVALUE),list(MYOTHERVALUE=format(my_pr,digits=3)))[2]
legend('topright',cex=1,legend=rp,bty='n',y.intersp=1)

mean(disturbedsites$X.bloom_area)
mean(undistsites$X.bloom_area)
sd(disturbedsites$X.bloom_area)
sd(undistsites$X.bloom_area)

____________________________________________________________________________
#richness model and analysis
richfullmodel<-lm(Butterfly_richness ~ portionbloom_native + 
 disturbance_category + X.bloom_area + plantsbloom_richness + sampling.area)
#model selection
#backwards
richstep<- stepAIC(richfullmodel, direction="both")
richstep$anova
write.csv(richstep$anova)
#foreward
richmodelnull<- lm(Butterfly_richness~ 1)
richstep<- stepAIC(richmodelnull, scope=.~ + portionbloom_native + 
 disturbance_category + X.bloom_area + plantsbloom_richness + sampling.area)
richstep$anova
#model selection with AICc
#making list of models
richnessmodel <- list( )
richnessmodel[[1]]<-lm(Butterfly_richness ~ disturbance_category)
richnessmodel[[2]]<-lm(Butterfly_richness ~ portionbloom_native)
richnessmodel[[3]]<-lm(Butterfly_richness ~ X.bloom_area)
richnessmodel[[4]]<-lm(Butterfly_richness ~ plantsbloom_richness)
richnessmodel[[5]]<-lm(Butterfly_richness ~ portionbloom_native + disturbance_category)
richnessmodel[[6]]<-lm(Butterfly_richness ~ disturbance_category + X.bloom_area)
richnessmodel[[7]]<-lm(Butterfly_richness ~ disturbance_category + plantsbloom_richness)
richnessmodel[[8]]<-lm(Butterfly_richness ~ portionbloom_native + 
 disturbance_category + X.bloom_area)
richnessmodel[[9]]<-lm(Butterfly_richness ~ disturbance_category +
 X.bloom_area + plantsbloom_richness)
richnessmodel[[10]]<-lm(Butterfly_richness ~ portionbloom_native + 
 disturbance_category + plantsbloom_richness)
richnessmodel[[11]]<-lm(Butterfly_richness ~  sampling.area)
richnessmodel[[12]]<-lm(Butterfly_richness ~  disturbance_category + X.bloom_area
 sampling.area)
#creating table
aictab(richnessmodel, modnames = NULL, second.ord = TRUE, nobs = NULL,
       sort = TRUE,)
#manual
richmodel1<-lm(Butterfly_richness ~ disturbance_category)
richmodel2<-lm(Butterfly_richness ~ portionbloom_native)
richmodel3<-lm(Butterfly_richness ~ X.bloom_area)
richmodel4<-lm(Butterfly_richness ~ plantsbloom_richness)
richmodel5<-lm(Butterfly_richness ~ portionbloom_native + disturbance_category)
richmodel6<-lm(Butterfly_richness ~ disturbance_category + X.bloom_area)
richmodel7<-lm(Butterfly_richness ~ disturbance_category + plantsbloom_richness)
richmodel8<-lm(Butterfly_richness ~ portionbloom_native + 
 disturbance_category + X.bloom_area)
richmodel9<-lm(Butterfly_richness ~ disturbance_category +
 X.bloom_area + plantsbloom_richness)
richmodel10<-lm(Butterfly_richness ~ portionbloom_native + 
 disturbance_category + plantsbloom_richness)
richmodel11<-lm(Butterfly_richness ~ disturbance_category + X.bloom_area + 
 disturbance_category:X.bloom_area)
anova(richmodel1,richmodel2,richmodel3,richmodel4,richmodel5,richmodel6,richmodel7,richmodel8,richmodel9,richmodel10)
summary(richmodel1)
summary(richmodel6)
scatterplot(Butterfly_richness ~ plantsbloom_richness)
--------------------------------------
#assumptions figure
par(mfrow = c(2, 2), las=1)
plot(richmodel1)

bptest(richmodel1)
shapiro.test(residuals(richmodel1))
#T.test on richness
t.test(Butterfly_richness ~ disturbance_category, alternative='two.sided',
 conf.level=.95 , var.equal=TRUE)
------------------------------------
#figure
par(mar=c(3,5,1,1),lwd=2)
boxplot(Butterfly_richness ~ disturbance_category, ylab = 'Richness', 
 names=c('Undisturbed','Disturbed'), notch=FALSE,cex.lab=1,cex.axis=1)
box(lwd=1)
my_cir<-'1.85 to 7.69'
my_pr<-0.00289
mylabel=bquote(italic(R)^2==.(format(my.p,digits=3)))
rp=vector('expression',2)
rp[1]=substitute(expression(italic(CI)==MYVALUE),list(MYVALUE=format(my_cir,dig=3)))[2]
rp[2]=substitute(expression(italic(p)==MYOTHERVALUE),list(MYOTHERVALUE=format(my_pr,digits=3)))[2]
legend('topright',cex=1,legend=rp,bty='n',y.intersp=1)

r1<-mean(disturbedsites$Butterfly_richness)
r2 <-mean(undistsites$Butterfly_richness)
sd(disturbedsites$Butterfly_richness)
sd(undistsites$Butterfly_richness)
srich<-r2-r1
mrich<-mean(Butterfly_richness)
mrich
srich/mrich*100
_____________________________________________________________________
#T.test on %in bloom and disturbance
bloomcutmodel<- lm(X.bloom_area ~ disturbance_category)
par(mfrow = c(2, 2), las=1)
plot(bloomcutmodel)
bptest(bloomcutmodel)
shapiro.test(residuals(bloomcutmodel))
summary(bloomcutmodel)
t.test(X.bloom_area ~ disturbance_category, alternative='two.sided',
 conf.level=.95, var.equal=TRUE)
____________________________________________________________________
#frances asked about flowering plants impact on SWI
#regression plot
mydata<-butterfly
require(ggplot2)
myplot<-ggplot(data=mydata, aes(x=X.bloom_area,
y=(Butterfly_shannon)))+geom_point()#+facet_grid(.~disturbance_category)
myplot
myplot<-myplot+
 stat_smooth(method = lm, se=FALSE)
myplot
update_labels(myplot, list(
 y = expression(SWI),
 x = expression(portion_blooming)))
#regression
cor.test(Butterfly_shannon , X.bloom_area)
modshabloo <- lm(Butterfly_shannon ~ X.bloom_area)
modshabloo
summary(modshabloo)