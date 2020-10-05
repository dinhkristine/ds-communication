# Demo of tables for presentation of regression inferences

library(ISLR) # for College data set
# stargazer and xtable options for LaTeX table code from R output
library(stargazer)
library(xtable)

# We used the College data set for illustration purposes in the LaTeX/knitr lectures and lab
# In this illustration, we are regressing number of college applications against 6 college characteristics.
# We first create an indicator of Elite College status (see exercise 8 in Ch. 2 of ISLR text)
Elite=rep("No",nrow(College))
Elite[College$Top10perc >50]="Yes"
Elite=as.factor(Elite)
College=data.frame(College ,Elite)
numvars = length(College) # number of variables in the College data set
n = dim(College)[1]
# Fit a model on private/public status, elite status, acceptances, out-of-state students,
#  room and board, and graduation rate.
fm1 = lm(Apps~Private+Elite+Accept+Outstate+Room.Board+Grad.Rate, data = College)
summary(fm1)

# A default stargazer regression inference table analogous to that presented in the knitr lecture
# keep.stat = "n" keeps only sample size, n, as a summary stat in the table.
stargazer(fm1, title="Regression inferences using stargazer.", label="reginf", 
          align=TRUE, keep.stat="n", ci=TRUE, ci.level=0.95, 
          covariate.labels=c("Private", "Elite", "Number Accept", "Out-of-state tuition",
                                       "Room and board", "Grad rate"))

# Let us neaten up the table and add a few regression summaries.
# single.row determined if estimates and CIs should be presented in same row or not
stargazer(fm1, title="Regression inferences using stargazer with more options.", label="reginf", 
          align=TRUE,  ci=TRUE, ci.level=0.95, single.row=TRUE, omit.stat=c("LL", "ser", "f"),
          covariate.labels=c("Private", "Elite", "Number Accept", "Out-of-state tuition",
                             "Room and board", "Grad rate"),
          dep.var.labels="Number of Applications", digits=2)


# An xtable version of the regression inference table, again analogous to that from the knitr lecture.
# Note that we add a caption and label for the LaTeX code.
fm.table = xtable(fm1, digits=4, 
                  caption="Inferences from regressing number of applications on whether the college is private or public,
                  whether the college is elite or not, acceptance rate, out of state tuition, room and board, and 
                  graduation rate.",
                  label="reginf")
align(fm.table) <- "|l|rrrr|"  # place vertical lines on left and right of table, and after first column of var names 
print(fm.table)

# Let's create a table with CIs
# First create table elements
# use formatC and signif to get the desired number of digits after the decimal
betahat = formatC(signif(fm1$coeff,digits=6), digits=2, format="f", flag="#")
SE = formatC(signif(summary(fm1)$coeff[,2],digits=6), digits=2, format="f", flag="#") 
cibounds = formatC(signif(confint(fm1),digits=6), digits=2, format="f", flag="#") 
pval = formatC(signif(summary(fm1)$coeff[,4],digits=4), digits=4, format="f", flag="#")
# Create table matrix
x = cbind(betahat, SE, matrix(paste("(", cibounds[,1], ",", cibounds[,2], ")")))
# Create column names
colnames(x) = cbind("Coefficient", "SE", "95% CI")
# Use rownames(x) to change variable names
rownames(x) = cbind("Intercept", "Private", "Elite", "Number Accept", "Out-of-state tuition",
                "Room and board", "Grad rate")
# Create LaTeX code using xtable
inftable = xtable(x, digits=2, 
                  caption="Inferences from final model fit using xtable.", label="modelinf")
align(inftable) = "|l|rrr|"
print(inftable)


# Add p-values to the xtable
x = cbind(betahat, SE, pval, matrix(paste("(", cibounds[,1], ",", cibounds[,2], ")")))
colnames(x) = cbind("Coefficient", "SE", "p-value", "95% CI")
rownames(x) = cbind("Intercept", "Private", "Elite", "Number Accept", "Out-of-state tuition",
                    "Room and board", "Grad rate")
inftable = xtable(x, digits=2, 
                  caption="Inferences from final model fit using xtable.", label="modelinf")
align(inftable) = "|l|rrrr|"
print(inftable)


