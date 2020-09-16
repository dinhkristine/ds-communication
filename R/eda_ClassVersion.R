# EDA lecture: useful R functions

# Required packages for the EDA
library(MASS)
library(corrplot)
library(car)
library(vioplot)

# Read in data, remove missing data
ny<-read.table("cs73.dat",header=T); dim(ny)  #  916  11
ny2<-na.omit(ny); dim(ny2) # 914  11
attach(ny2)

## review data set: expen is response
names(ny2)
head(ny2, n=5)

# Graphics for univariate explorations:
# violin plot
vioplot(expen)  # combines box plot and kernel density estimate
# Code to get rid of that '1' on the x-axis and label the y-axis
plot(1, 1, xlim = c(0, 2), ylim = range(expen), type = 'n', xlab = '', ylab = '', xaxt = 'n')
vioplot(expen, at=1, add=T)  # combines box plot and kernel density estimate
axis(1, at = 1, labels = c('NY municipalities')) # set horizontal axis
axis(2, at = 1700, pos=-0.2, tck=0, labels="Expenditures")

# histogram smoothing
lexpen = log(expen)
# histogram with density plot overlay
hist(lexpen, prob=T, breaks = 20, xlab="Log-Expenditures", ylab="Density", main="")
# density smooth
lines(density(lexpen), col="blue")
rug(lexpen) # data rug
# normal approximation
curve(dnorm(x, mean=mean(lexpen), sd=sd(lexpen)), add=TRUE, col="green")

# Box-Cox transformation: just another exploration!
bc = boxcox(expen~1, data=ny2)
bc = boxcox(expen~., data=ny2)
# optimal lambda
bc$x[bc$y==max(bc$y)]


# Graphics for explorations of relationships
# Set up data frame with inputs to consider
ny2vars = data.frame(expen, wealth, pop, pint, dens, income, growr)

# On the scatterplot matrix exploratory tool
# Two ways to do the infamous scatter plot matrix: 
#  if present in paper, must interpret in text!
#  recommend placing in the appendix of a report, if at all
scatterplotMatrix(ny2vars) # in CARS package
pairs(~expen+wealth+pop+pint+dens+income+growr, ny2)

# On regression assumptions: 
#  predictors are not assumed normally distributed!
#  we are looking for linear relationships between response and each predictor
#  so even though all the variables are skewed, do not necessarily need log-transform
#  par(mfrow=c(x,y)) to put multiple graphics in a plot window!
lexpen<-log(expen)
lwealth<-log(wealth)
lpop<-log(pop)
ldens<-log(dens)
lincome<-log(income)
lpint<-log(pint)
par(mfrow=c(2,6))
# First histogram of non-transformed variables
hist(expen)
hist(wealth) 
hist(pop)
hist(pint)
hist(dens)
hist(income)
# Then histogram of log-transformed variables (except Growth Rate)
hist(lexpen) 
hist(lwealth)  
hist(lpop)
hist(log(pint))
hist(ldens)
hist(lincome)  


# Scatterplot smoothing: 
#  example: pint is skewed, but do we need to do a log-transform?
#    let us look at the relationship between log-Expenditure and pint
plot(pint, lexpen, xlab="% Intergovernmental Funds", ylab="Log-Expenditure")
lines(smooth.spline(pint,lexpen), col="blue")
abline(lm(lexpen~pint), col="green")
# Try log-transformation: still nonlinear, so probably best 
#  to not do log-transformation and just use polynomial directly on pint
plot(log(pint),lexpen)
lines(smooth.spline(log(pint),lexpen), col="blue")
abline(lm(lexpen~log(pint)), col="green")

# Fancier graphics: scatterplot smooth + violin plots
# requires manually identifying where to place the violin on the x- and y-axes
# the 'wex' option controls the expansion of the violin
plot(pint, lexpen, xlim=c(-10,max(pint)), ylim=c(2,max(lexpen)), 
     ylab="Log Expenditures", xlab="% Intergovernmental Funds")
vioplot(pint, col="tomato", horizontal=T, at=3, add=T, lty=2, rectCol="gray")
vioplot(lexpen, col="cyan", horizontal=F, at=-5, add=T, lty=2, wex=10)
# Two ways to place a scatterplot smooth: smoothing spline and LOWESS
lines(lowess(pint,lexpen), col="blue") 
#abline(lm(lexpen~pint), col="green")  # put regression line on the scatterplot
# present a regression line only over the scatterplot points
p = lm(lexpen~pint)
new=data.frame(pint=min(pint))
y0=predict(p, new); y1=predict(p, data.frame(pint=max(pint)))
segments(min(pint),y0,max(pint),y1,col="green")


# Side-by-side box plots for displaying categorical variables
# The NY Expenditures data set has only continuous inputs.  
# For illustration purposes, let's create a wealth category variable.
wealthcat = 1*(wealth<40000)+2*(wealth>40000)*(wealth<80000)+3*(wealth>80000)*(wealth<100000)+4*(wealth>100000)*(wealth<200000)+5*(wealth>200000)
wealthcat=factor(wealthcat) # tell R this variable is categorical
# tabulate the categorical variable
table(wealthcat) 
# Parallel boxplots with box width a function of sample size
plot(wealthcat, expen, col="cyan", varwidth=T,
     ylab="Expenditures",xlab="Wealth Categories")



