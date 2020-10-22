library(corrplot)
library(MASS)
#library(rJava); library(glmulti)
library(effsize)

##############
# Lab Tasks: 
#   Be prepared to discuss at end of class;
#   you will not hand anything in.
#   Use PK_EDALRModeleda_ClassVersion.R, PK_diagnostics_ClassVersion, and 
#     Prostate_diagnostics_ClassVersion.R as a reference for code.
##############


## Load in prostate data set and take initial looks
Prostate = read.table("prostate.txt", header=T)
dim(Prostate)
#[1] 380 8
sum(is.na(Prostate)) # missing data: 3 in race
Prostate<-na.omit(Prostate); dim(Prostate) # 377 8
Prostate$dcaps = as.factor(Prostate$dcaps)
Prostate$dpros = as.factor(Prostate$dpros)
attach(Prostate)


## Task 1: EDA
# A reminder: in this project, we will consider interactions, but not consider transformations.
#
# For today's lab, let us focus on the variables 
#   capsule (response), race (categorical predictor), and psa (continuous predictor)
# a) Conditional density plots graphically present how a binary response changes over a 
#     covariate. Such a graphic can be obtained with cdplot(factor(response)~covariate).
#     Present a 1x2 cdplot showing the relationship of capsule with each of race & psa.
# b) binary response, capsule: present a simple table of counts using 
#     table and/or prop.table.
# c) binary response, capsule against categorical covariates, here race: present a
#     contingency table of capsule by race. (Side-by-side bar charts are also an option, 
#     though I think contingency tables are sufficient.)
# d) binary response, capsule against continuous covariates, here psa: present
#     side-by-side box plots of psa by capsule categories; automatically done by the 
#     plot function with binary variable on x-axis and continuous variable on y-axis.
# e) standardized mean difference: present Cohen's d (cohen.d function) to
#     evaluate the relationship between capsule and each of race and psa.
##


## Task 2: Model building
# Logistic regression models are fit using the glm function using the logit link:
#   e.g., glm(capsule~psa+gleason, family=binomial(link=logit), data=Prostate)
# a) Stepwise model selection: include interactions, consider stepAIC for first pass
# b) Parsimonious model: perform backward selection via p-values,
#      identify a simpler model by being strict with interaction terms. 
#      Is it that much worse than best stepwise model?
#
# Note: This will NOT necessarily be your final model for DAR 2.  
#       We are just briefly illustrating the model building process here.
##


## Task 3: Model evaluation
# Consider a model of PSA, Gleason score, and Results of digital rectal exam.
#
# Prostate_diagnostics_ClassVersion.R presents sample code for a given model via explanatory variable patterns (EVPs).
# The components include:
# a) Residual plots: use the examine.logistic.reg function provided
# b) Outlier detection: evaluate EVPs for potential outlying data points
# c) HL test of overall fit: use the HLtest.R function provided; see PK_diagnostics_ClassVerion.R
#
# I included the code for parts (a) and (b) below, modified from the video lectures.
# For class discussion, think about the interpretation of the plots and 
# decisions with respect to the potentially "outlying" EVPs. 
##

# The functions for residual analysis we proposed using are as follows:
one.fourth.root=function(x){
  x^0.25
}
# make sure examine.logistic.reg.R is in your working directory or you have the right path specified
source("examine.logistic.reg.R")

# Consider a model of PSA, Gleason score, and Results of digital rectal exam
dat.glm <- glm(capsule ~ psa+gleason+dpros, family = binomial, data = Prostate)
dat.mf <- model.frame(dat.glm)
## Covariate pattern: too many EVPs!
w <- aggregate(formula = capsule ~ psa+gleason+dpros, data = Prostate, FUN = sum)
n <- aggregate(formula = capsule ~ psa+gleason+dpros, data = Prostate, FUN = length)
w.n <- data.frame(w, trials = n$capsule, prop = round(w$capsule/n$capsule,2))
dim(w.n)
#[1] 342 6

# Create EVPs by binning continuous covariates
g = 5 # number of categories
psa_interval = cut(psa, quantile(psa, 0:g/g), include.lowest = TRUE)  # Creates factor with levels 1,2,...,g
levels(psa_interval)

w <- aggregate(formula = capsule ~ psa_interval+gleason+dpros, data = Prostate, FUN = sum)
n <- aggregate(formula = capsule ~ psa_interval+gleason+dpros, data = Prostate, FUN = length)
w.n <- data.frame(w, trials = n$capsule, prop = round(w$capsule/n$capsule,2))
mod.prelim1 <- glm(formula = capsule/trials ~ psa_interval+gleason+dpros,
                   family = binomial(link = logit), data = w.n, weights = trials)
save1=examine.logistic.reg(mod.prelim1, identify.points=T, scale.n=one.fourth.root, scale.cookd=sqrt)
w.n.diag1=data.frame(w.n, pi.hat=round(save1$pi.hat, 2), std.res=round(save1$stand.resid, 2), 
                     cookd=round(save1$cookd, 2), h=round(save1$h, 2))
p=length(mod.prelim1$coef) # number of parameters in model (# coefficients)
ck.out=abs(w.n.diag1$std.res)>2 | w.n.diag1$cookd>4/nrow(w.n) | w.n.diag1$h > 3*p/nrow(w.n)
extract.EVPs=w.n.diag1[ck.out, ]
extract.EVPs

