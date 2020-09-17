# Stat 794: EDA Lab
# Experimenting with EDA using the NY Expenditures data set
# Packages required: MASS, corrplot, car, vioplot, sm (for use with vioplot)

## Preliminaries
# Read in data, remove missing data
ny <- read.table("data/cs73.dat", header = T); dim(ny)  #  916  11
ny2<-na.omit(ny); dim(ny2) # 914  11
attach(ny2)

##############
# Lab Tasks: 
#   Treat this lab as an opportunity to build up EDA for DAR 1.
#   Please work through the following six tasks and be prepared to discuss during class.
#   You will not hand anything in for grading, points will be earned for this lab 
#   through participation in the class discussion.
#   Use eda_ClassVersion.R from online videos as a reference for code.
##############

## EDA on the response, expenditures

## Task 1: Study a descriptive numerical summary of expen
# What do you find?  (Briefly summarize)
summary(expen)
# Looking at the summary statistics, the range is very large. 
# The median is 227 and 75th percentile is 316. 
# This indicate that values above ~600 can be outliers

## Task 2: Study graphical summaries of expen
# a) boxplot (boxplot)
boxplot(expen)

# b) histogram (hist; consider using the 'breaks' option)
hist(expen, breaks = 100)

# c) violin plot (vioplot; see eda_ClassVersion.R for code)
vioplot::vioplot(expen)

# d) Q-Q plot (qqnorm; qqline)
qqnorm(expen)
qqline(expen, col = "red")

# e) Get in the habit of LABELLING AXES and removing titles!
# f) What do you find?  (Briefly summarize)
# the variable expen is right skew, the right tail is kind of too far from the median 


## Task 3: Consider a response transformation
# a) Perform a Box-Cox transformation on an intercept only model: boxcox(expen~1); 
#    argue for a log-transformation
MASS::boxcox(expen~1)
# a log transformation would be better to make expen normal 
# since using a log transformation would fix the normality problems for expen
# 95% CI cover zero
MASS::boxcox(expen~., data = ny2)

# b) Study a histogram with density plot overlay of log-expenditures 
#      (see eda_ClassVersion.R for code)
lexpen <- log(expen)
hist(lexpen, prob=T, breaks = 20)
lines(density(lexpen), col="red")
# normal approximation 
curve(dnorm(x, mean=mean(lexpen), sd = sd(lexpen)), add = TRUE, col = "green")

# c) Perform a violin plot and Q-Q plot of log-expenditures
vioplot::vioplot(lexpen)
qqnorm(lexpen)
qqline(lexpen, col = "red")

# d) What do you find?  (Briefly summarize)
# After log transformation, expen follow normal distribution now 
# there is no more skewness and
# transformation seems to solve the outliers problems 

# Response Transformation: let's try log-transform
lexpen = log(expen)


## Task 4: EDA for predictors, univariate analysis
# For this lab and EDA, let us consider only log-transformations for all variables.
# Note that in practice, you would scan through a variety of non-linear transformations.
#
# Use scatter plot smooths to study the relationship between log-expenditure and
# a) wealth and log-wealth (see eda_ClassVersion.R for code)
plot(wealth, lexpen, main = "log-expenditure vs wealth")
lines(smooth.spline(wealth,lexpen), col="blue")
abline(lm(lexpen~wealth), col="green")
# suggesting a log transformation to avoid clustering by spreading out the points, 
# with a log tranformation, it also resolve the skewness of data
# skewness data might effect the relationship of the variables 
#** definately try other transformation (quadritic, cubic, etc) to see better results 

plot(log(wealth), lexpen, main = "log-expenditure vs log-wealth")
lines(smooth.spline(log(wealth),lexpen), col="blue")
abline(lm(lexpen~log(wealth)), col="green")

# b) population and log-population with a cut-point
#   here is code to do (b)
plot(pop, lexpen)  # notice need for a transformation on pop
lpop = log(pop)
plot(lpop, lexpen)
lines(lowess(lpop,lexpen), col="blue") # using a LOWESS scatter plot smooth
## The pattern look like a quadratic but more like a V shape 
lines(c(8.3,8.3),c(0,6), col="grey", lwd=3)
## splitting the data, we can model separately, one model for each direction
## this will give us a linear relationship if we model separately 

# c) density and log-density with a cut-point (ammend code from b)
plot(dens, lexpen)  # also need transformation here
ldens = log(dens)
plot(ldens, lexpen)
lines(lowess(ldens,lexpen), col="blue") 
lines(c(4,4),c(4,5.5), col="grey", lwd=3)


## Task 5: Growth rate
# a) Study a histogram of growth rate--can we apply a log-transformation?
hist(growr)
# it seems like we need a log-transformation 
hist(log(growr)) 
# much better now but we have to consider when log(0) the result will be inf

# b) Consider a piecewise transformation:
# Use different transformations for growth rates > 0 and for growth rates < 0.
# As long as the joint transformation is monotonic and keeps the zero at 0, we are fine.
# One approach is the transformation t(x), x>0; -t(-x), x<0. 
# Then can apply log-transformation!  
# But have to be careful with x=0 since ln(x)=ln(0) is undefined.  Easy fix: add 1.01.
# Create the piecewise function
lgrowr<-ifelse(growr>0, log(growr+1.01), -log(-growr+1.01))
# c) Study a scatter plot smooth of log-expenditure against this new lgrowr variable 
plot(lgrowr, lexpen, main = "log-growrate vs wealth")
lines(smooth.spline(lgrowr,lexpen), col="blue")
abline(lm(lexpen~lgrowr), col="red")

# d) Interpret the four graphics from Task 4 (a)-(c) and this Task 5 (c) here in terms of the 
#    seeming success of the transformations to produce linear relationships with log-expenditures.

## FYI: In a data analysis report you typically can not present in the main text all 
## EDA graphics created; you will have to pick and choose the graphics that forward 
## your storyline. Nonetheless, a space saver is placing multiple graphics in a figure window.  
## Try coding a 2x2 graphic of scatter plot smooths from Tasks 4 and 5 
## (log-expenditure against log-wealth, log-population, log-density, and log-growth rate).

par(mfrow=c(2,2)) # specify 2x2 par 

# log=expenditure vs log-wealth
plot(log(wealth), lexpen, main = "log-expenditure vs log-wealth")
abline(lm(lexpen~log(wealth)), col="red")

# log=expenditure vs log-population
plot(log(pop), lexpen, main = "log-expenditure vs log-population")
abline(lm(lexpen~log(pop)), col="red")

# log=expenditure vs log-density
plot(log(dens), lexpen, main = "log-expenditure vs log-density")
abline(lm(lexpen~log(dens)), col="red")

# log=expenditure vs log-growth rate
plot(lgrowr, lexpen, main = "log-expenditure vs log-growth rate")
abline(lm(lexpen~lgrowr), col="red")

## log transformation for wealth and growth rate seems to have a successful 
## linear relationship with expenditure 
## however, after transforming population and density, 
## there still seems to have a curvature shape, suggesting different transformation like polynomial 
## log growth rate also seems to be very sparse, suggesting the linear relationship might be weak

## Task 6: Studying relationship between predictors, correlation
# Study and interpret a correlation plot.
# Set up data frame with only the inputs
ny2vars = data.frame(expen, wealth, pop, pint, dens, income, growr)
cny2 = cor(ny2vars)
corrplot::corrplot(cny2)
## wealth and expen seems to have high positive correlation 
## however, expense has really low correlation with other variables 
## density and population are highly positive correlated, this suggest multicolinearity issue 
## if both of these 2 variables are in the same model == unstable coefficient 

