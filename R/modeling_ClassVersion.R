# Regression: modeling and model diagnostics
# NY municipalities expenditures data

# Load libraries
library(MASS)
library(corrplot)
library(car)
library(leaps)

rm(list=ls(all=TRUE)) # remove all previous objects from memory
options(warn=-1)  # forces R to ignore all warning messages

# Read in data, remove missing data
ny<-read.table("data/cs73.dat",header=T); dim(ny)  #  916  11
ny2<-na.omit(ny); dim(ny2) # 914  11
attach(ny2)

## review data set: expen is response
names(ny2)
head(ny2, n=5)


## Regression modeling: EDA informing a full model
## create transformations that may be needed
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
pint2<-pint**2
pint3<-pint**3
lpop2<-lpop**2
lpop3<-lpop**3
ldens2<-ldens**2
ldens3<-ldens**3
i2 = income^2
i3 = income^3
lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))


## Linear or nonlinear relationships with lexpen?
# wealth: log-transform
# lpop: two linear segments OR nonlinear (cubic)
# ldens: two linear segments OR nonlinear (cubic)
# pint: nonlinear (probably no need for log-transform; cubic)
# income: nonlinear (quadratic)
# lgrowth: linear


# WEALTH: log-transformation
plot(wealth, lexpen, xlab="Wealth", ylab="Log-Expenditure")  ## linear
#lines(smooth.spline(lwealth,lexpen), col="blue")
lines(lowess(wealth,lexpen), col="green")
# Check out log-transform
plot(lwealth, lexpen, xlab="Log-Wealth", ylab="Log-Expenditure")  ## linear
lines(lowess(lwealth,lexpen), col="green")


# DENS and POP: Transformations to simplify relationship
# Density and Population are compressed and seemingly non-linear relationship with log-expenditure
par(mfrow=c(2,1))
plot(dens,lexpen, ylab="Log-Expenditure", xlab="Density")
lines(lowess(dens,lexpen), col="blue")  #nonlinear, two linear segments
plot(pop,lexpen, ylab="Log-Expenditure", xlab="Density")
lines(lowess(pop,lexpen), col="blue")  #nonlinear, two linear segments

# Try log-transformation on both
plot(ldens,lexpen, ylab="Log-Expenditure", xlab="Log-Density")
lines(lowess(ldens,lexpen), col="blue")  #nonlinear, two linear segments
lines(c(4.5,4.5),c(0,6), col="grey", lwd=3)
plot(lpop,lexpen, xlab="Log-Population", ylab="Log-Expenditure")
lines(lowess(lpop,lexpen), col="blue") #nonlinear, perhaps cubic; two linear segments will do
lines(c(8.3,8.3),c(0,6), col="grey", lwd=3)


# PINT and INCOME: transformation may not be necessary
plot(pint,lexpen, xlab="% Intergovernmental Funds", ylab="Log-Expenditure")
lines(smooth.spline(pint,lexpen), col="blue")  #nonlinear?
abline(lm(lexpen~pint), col="green")
# Try log-transformation: still nonlinear, so probably best 
#  to not do log-transformation and just use polynomial directly on pint
plot(log(pint),lexpen)
lines(smooth.spline(log(pint),lexpen), col="blue")  #nonlinear?
abline(lm(lexpen~log(pint)), col="green")

plot(income,lexpen, ylab="Log-Expenditure", xlab="Income")  
lines(lowess(income,lexpen), col="blue") # non-linear, compressed; try log
abline(lm(lexpen~income), col="green")
# Check out log-transform: still polynomial, will not do log
plot(lincome,lexpen, ylab="Log-Expenditure", xlab="Log-Income")  
lines(lowess(lincome,lexpen), col="blue")  ## still non-linear
abline(lm(lexpen~lincome), col="green")


# GROWTH RATE: log-transformation to handle negative values
lgrowr = ifelse(growr>0, log(growr+1), -log(-growr+1))
plot(lgrowr,lexpen, xlab="Log-Growth function", ylab="Log-Expenditures")
lines(lowess(lgrowr,lexpen), col="blue")  ## linear


## Summary: linear or nonlinear relationships with lexpen?
# wealth: log-transform
# lpop: two linear segments OR nonlinear (cubic)
# ldens: two linear segments OR nonlinear (cubic)
# pint: nonlinear (probably no need for log-transform; cubic)
# income: nonlinear (probably no need for log-transform; quadratic)
# lgrowth: linear
###########################


#####
## Regression model on full data set (i.e., do not split data)
# wealth: log-transform
# lpop: cubic polynomial
# ldens: cubic polynomial
# pint: cubic polynomial, no log-transform
# income: quadratic, no log-transform
# lgrowth: linear
######

## Create transformations that may be needed
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
pint2<-pint**2
pint3<-pint**3
lpop2<-lpop**2
lpop3<-lpop**3
ldens2<-ldens**2
ldens3<-ldens**3
i2 = income^2
i3 = income^3
lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))


# Model selection: Section 6.1 of ISLR
# stepAIC or
# Section 6.5 of ISLR: regsubsets in the leaps package.
# Hopefully talk about cross-validation later in course.

fit11<-lm(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+
            ldens+ldens2+ldens3+income+i2+lgrowr)
summary(fit11)
# Query: what do next? -> remove income^2 (i2)
fit11b<-lm(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+
             ldens+ldens2+ldens3+income+lgrowr)
summary(fit11b)
# Try AIC model selection
stepAIC(fit11b, direction="both")  # get same final model as fit11b

# leaps: exhaustive best subsets search
ny2vars = data.frame(lexpen,lwealth,lpop,lpop2,lpop3,pint,pint2,pint3,ldens,ldens2,ldens3,income,lgrowr)
regfit =regsubsets(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+
                     ldens+ldens2+ldens3+income+lgrowr, data = ny2vars, nvmax=12)
summary(regfit)
par(mfrow=c(2,1))
plot(summary(regfit)$cp, xlab="Number of Variables", ylab="Cp")
plot(summary(regfit)$bic, xlab="Number of Variables", ylab="BIC")
which.min(summary(regfit)$cp); which.min(summary(regfit)$bic)
# Note that best BIC model has ldens and ldens^3, no quadratic term.
# Can that be justified?!



###############
# Model diagnostics: typically iterative process

# Model chosen to assess
fit11b<-lm(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+
             ldens+ldens2+ldens3+income+lgrowr)
summary(fit11b)

# Residual plot
plot(predict(fit11b), rstudent(fit11b), ylab="Studentized Residuals", xlab="Predicted")
identify(predict(fit11b), rstudent(fit11b), labels=row.names(ny2)) # 'escape to finish'
predict(fit11b)[rstudent(fit11b)==min(rstudent(fit11b))]
#887 
#5.941401 
ny2[887,]
#obs st co expen wealth  pop pint dens income   id growr
#887 893 36 55    57 170195 5371 29.8 4883  34082 7340 -54.1

# Normality of Residuals
sresid <- studres(fit11b)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
# Q-Q plot for studentized resid
qqPlot(fit11b, main="QQ Plot", ylab="Studentized Residuals")
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-p-1) as a guide; 
# Cook and Weisberg recommend 0.5 and 1 (R uses these guides in default diagnostic plots below)
cutoff <- 4/((nrow(ny2)-length(fit11b$coefficients)-2))
plot(fit11b, which=4, cook.levels=cutoff) # influence Plot
# Influence plot: studentized residuals vs. hat matrix diagonals (leverage) with bubbles a function of Cook's D
# Interactive, so can click to identify high leverage/influential/outlying points
influencePlot(fit11b, id.method="identify", 
              main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# VIF: don't want to put in polynomial terms since they are correlated!
fit=lm(lexpen~lwealth+lpop+pint+ldens+income+lgrowr)
vif(fit) # closer to 1 the better; 5-10 is moderate
#lwealth     lpop     pint    ldens   income   lgrowr 
#1.906337 6.413258 1.231470 7.854204 2.682954 1.022102 

# All encompansing R default regression model diagnostics
par(mfrow=c(2,2))
plot(fit11b)


# Predictions

# Model
fit11b<-lm(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+
             ldens+ldens2+ldens3+income+lgrowr, data=ny2)
summary(fit11b)

# Account for sigma^2
sdfit=sd(fit11b$resid)
new05 = data.frame(lwealth=log(85000), lpop=log(20442), lpop2=log(20442)^2, lpop3=log(20442)^3, pint=24.7, 
                   pint2=24.7^2, pint3=24.7^3, ldens= log(214), ldens2=log(214)^2, ldens3=log(214)^3, income=19500, 
                   lgrowr=log(35+1))
new25 = data.frame(lwealth=log(89000), lpop=log(31033), lpop2=log(31033)^2, lpop3=log(31033)^3, pint=26.0, 
                   pint2=26.0^2, pint3=26.0^3, ldens = log(214), ldens2=log(214)^2, ldens3=log(325)^3, income=20000, 
                   lgrowr=log(40+1))
warick05=predict.lm(fit11b,new05); exp(warick05+sdfit^2/2)                 
exp(predict(fit11b, new05, interval="prediction")+sdfit^2/2)
warick25=predict.lm(fit11b,new25); exp(warick25+sdfit^2/2)  
exp(predict(fit11b, new25, interval="prediction")+sdfit^2/2)
#fit      lwr      upr
#1 266.9911 135.0772 527.7298
#> warick25=predict.lm(fit11b,new25); exp(warick25+sdfit^2/2)  
#1 
#377.3172 
#> exp(predict(fit11b, new25, interval="prediction")+sdfit^2/2)
#fit      lwr      upr
#1 377.3172 185.3709 768.0185


# Try: remove the potential outlying and influential data point 887 with largish negative growth
ny2new = ny2[-887,]
detach(ny2); fitny2=fit11b
attach(ny2new)
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
pint2<-pint**2
pint3<-pint**3
lpop2<-lpop**2
lpop3<-lpop**3
ldens2<-ldens**2
ldens3<-ldens**3
i2 = income^2
i3 = income^3
lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))
fit11b<-lm(lexpen~lwealth+lpop+lpop2+lpop3+pint+pint2+pint3+ldens+
             ldens2+ldens3+income+lgrowr, data=ny2new)
summary(fit11b)
summary(fitny2)

# Predictions with outlier removed
sdfit=sd(fit11b$resid)
new05 = data.frame(lwealth=log(85000), lpop=log(20442), lpop2=log(20442)^2, lpop3=log(20442)^3, pint=24.7, 
                   pint2=24.7^2, pint3=24.7^3, ldens= log(214), ldens2=log(214)^2, ldens3=log(214)^3, income=19500, 
                   lgrowr=log(35+1))
new25 = data.frame(lwealth=log(89000), lpop=log(31033), lpop2=log(31033)^2, lpop3=log(31033)^3, pint=26.0, 
                   pint2=26.0^2, pint3=26.0^3, ldens = log(214), ldens2=log(214)^2, ldens3=log(325)^3, income=20000, 
                   lgrowr=log(40+1))
warick05=predict.lm(fit11b,new05); exp(warick05+sdfit^2/2)                 
exp(predict(fit11b, new05, interval="prediction")+sdfit^2/2)
warick25=predict.lm(fit11b,new25); exp(warick25+sdfit^2/2)  
exp(predict(fit11b, new25, interval="prediction")+sdfit^2/2)



# Consider interactions: Sections 3.3.2, 3.6.4 of ISLR
# Use the data subset on which there are linear relationships in all variables.

# log-transformations of all variables
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
lpint = log(pint)
lgrowr<-ifelse(growr>0, log(growr+1), -log(-growr+1))

set2<-(lpop>8.3 & ldens>4.5)
ny2vars = data.frame(lexpen,lwealth,lpop,lpint,lincome,lgrowr)
fit2 = lm(lexpen~.*., data = ny2vars, subset=set2)
fitfull = stepAIC(fit2)
summary(fitfull)
extractAIC(fitfull)  # want to minimize AIC
# remove non-significant interaction lwealth:lpop
fitbest = lm(lexpen ~ lwealth + lpop + lpint + lincome + lgrowr + 
               lwealth:lpint + lwealth:lgrowr + lpop:lincome + 
               lpop:lgrowr, data = ny2vars, subset = set2)
summary(fitbest)
extractAIC(fitbest)
anova(fitfull, fitbest)  # p = 0.16
# perhaps remove non-significant interactions:
#  lpop:lgrowr and then lpop:lincome
fitbest = lm(lexpen ~ lwealth + lpop + lpint  + lgrowr + 
               lwealth:lpint + lwealth:lgrowr , data = ny2vars, subset = set2)
summary(fitbest)
extractAIC(fitbest)


# Model diagnostics
# Residual plot
plot(predict(fitbest), rstudent(fitbest), ylab="Studentized Residuals", xlab="Predicted")
# Normality of Residuals
sresid <- studres(fitbest)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
# Q-Q plot for studentized resid
qqPlot(fitbest, main="QQ Plot", ylab="Studentized Residuals")
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fitbest$coefficients)-2))
plot(fitbest, which=4, cook.levels=cutoff) # influence Plot

# Aside: leaps not smart enough to leave main effects in when interaction in model
ff=regsubsets(lexpen~.*., data=ny2vars, subset=set2)
summary(ff)


# Categorical variables in regression
# For illustration purposes, we will use the whole data set (no subset split)
wealthcat = 1*(wealth<40000)+2*(wealth>40000)*(wealth<80000)+3*(wealth>80000)*(wealth<100000)+4*(wealth>100000)*(wealth<200000)+5*(wealth>200000)
# "Dummy variable" coding or "treatment" coding: 
# contrast to a reference level or baseline 
wealthcat=factor(wealthcat) # tell R this variable is categorical
# tabulate the categorical variable
table(wealthcat) 
# Parallel boxplots with box width a function of sample size
plot(wealthcat, expen, col="cyan", varwidth=T,
     ylab="Expenditures",xlab="Wealth Categories")
# Average log-expenditure by wealth category
tapply(lexpen, wealthcat, mean)
fitc = lm(formula = lexpen ~ wealthcat, data=ny2vars)
# Treatment coding: 
#  notice that the intercept is the (cell) mean log-expenditure for wealth category 1!
summary(fitc) 
# contrast matrix for 5-level categorical variable
contr.treatment(5)
# Can fit continuous variables as well
fitc = lm(formula = lexpen ~ wealthcat + lpop + lpint + lincome, data=ny2vars, subset=set2)
summary(fitc)


