# Stat 794: EDA Lab
# Experimenting with EDA using the NY Expenditures data set
# Packages required: MASS, corrplot, car, vioplot, sm (for use with vioplot)

## Preliminaries
# Read in data, remove missing data
ny<-read.table("cs73.dat",header=T); dim(ny)  #  916  11
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


## Task 2: Study graphical summaries of expen
# a) boxplot (boxplot)
# b) histogram (hist; consider using the 'breaks' option)
# c) violin plot (vioplot; see eda_ClassVersion.R for code)
# d) Q-Q plot (qqnorm; qqline)
# e) Get in the habit of LABELLING AXES and removing titles!
# f) What do you find?  (Briefly summarize)


## Task 3: Consider a response transformation
# a) Perform a Box-Cox transformation on an intercept only model: boxcox(expen~1); 
#    argue for a log-transformation
# b) Study a histogram with density plot overlay of log-expenditures 
#      (see eda_ClassVersion.R for code)
# c) Perform a violin plot and Q-Q plot of log-expenditures
# d) What do you find?  (Briefly summarize)
# Response Transformation: let's try log-transform
lexpen = log(expen)


## Task 4: EDA for predictors, univariate analysis
# For this lab and EDA, let us consider only log-transformations for all variables.
# Note that in practice, you would scan through a variety of non-linear transformations.
#
# Use scatter plot smooths to study the relationship between log-expenditure and
# a) wealth and log-wealth (see eda_ClassVersion.R for code)
# b) population and log-population with a cut-point
#   here is code to do (b)
plot(pop, lexpen)  # notice need for a transformation on pop
lpop = log(pop)
plot(lpop, lexpen)
lines(lowess(lpop,lexpen), col="blue") # using a LOWESS scatter plot smooth
lines(c(8.3,8.3),c(0,6), col="grey", lwd=3)
# c) density and log-density with a cut-point (ammend code from b)


## Task 5: Growth rate
# a) Study a histogram of growth rate--can we apply a log-transformation?
# b) Consider a piecewise transformation:
# Use different transformations for growth rates > 0 and for growth rates < 0.
# As long as the joint transformation is monotonic and keeps the zero at 0, we are fine.
# One approach is the transformation t(x), x>0; -t(-x), x<0. 
# Then can apply log-transformation!  
# But have to be careful with x=0 since ln(x)=ln(0) is undefined.  Easy fix: add 1.01.
# Create the piecewise function
lgrowr<-ifelse(growr>0, log(growr+1.01), -log(-growr+1.01))
# c) Study a scatter plot smooth of log-expenditure against this new lgrowr variable 
# d) Interpret the four graphics from Task 4 (a)-(c) and this Task 5 (c) here in terms of the 
#    seeming success of the transformations to produce linear relationships with log-expenditures.

## FYI: In a data analysis report you typically can not present in the main text all 
## EDA graphics created; you will have to pick and choose the graphics that forward 
## your storyline. Nonetheless, a space saver is placing multiple graphics in a figure window.  
## Try coding a 2x2 graphic of scatter plot smooths from Tasks 4 and 5 
## (log-expenditure against log-wealth, log-population, log-density, and log-growth rate).


## Task 6: Studying relationship between predictors, correlation
# Study and interpret a correlation plot.
# Set up data frame with only the inputs
ny2vars = data.frame(expen, wealth, pop, pint, dens, income, growr)
cny2 = cor(ny2vars)
corrplot(cny2)



