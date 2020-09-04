
library(ISLR)
library(stargazer)
library(xtable)

rm(list=ls(all=TRUE)) # remove all previous objects from memory

# Set up data for the illustration
# For illustration purposes we will use the College data set from the ISLR text
# Create an indicator of Elite College status (see exercise 8 in Ch. 2 of ISLR text)
Elite=rep("No",nrow(College))
Elite[College$Top10perc >50]="Yes"
Elite=as.factor(Elite)
College=data.frame(College ,Elite)
numvars = length(College) # number of variables in the College data set
n = dim(College)[1]

# Fit a model
fm1 = lm(Apps~Private+Elite+Accept+Outstate+Room.Board+Grad.Rate, data = College)
# predcit a new school not in the data set
new1 = data.frame(Private="No", Elite="No", Accept=5000, Outstate=8000, Room.Board=6000, Grad.Rate=0.6)
newpred1 = signif(predict(fm1, new1, interval="prediction"), 2)
new2 = data.frame(Private="Yes", Elite="Yes", Accept=1000, Outstate=16000, Room.Board=4000, Grad.Rate=0.90)
newpred2 = signif(predict(fm1, new2, interval="prediction"), 2)

fm.table <- xtable(fm1, digits=2, 
                   caption = "Summary of Regression",
                   label="reginf")

align(fm.table) <- "|l|rrrr|"  

print(fm.table)

y = data.frame(College$Apps,College$Private,College$Elite,College$Accept,College$Outstate,College$Room.Board,College$Grad.Rate)
pairs(y)

stargazer(College, 
          title="Summary statistics of all variables for the ISLR College data set.", 
          label="descrips", 
          summary.stat = c("mean", "median", "sd", "min", "p25", "p75", "max"), 
          covariate.labels = c("Private Flag"
                               , "Application Count"
                               , "Acceptance Count"
                               , "Enrollment Count"
                               , "Top 10 Percent in High School"
                               , "Top 25 Percent in High School"
                               , "Full-time Undergrad"
                               , "Part-time Undergrad"
                               , "Out-of-state tuition"
                               , "Room and board Costs"
                               , "Estimated Book Costs"
                               , "Estimated Personal Costs"
                               , "Percent of Faculty with PhD"
                               , "Percent of Faculty with terminal Degree"
                               , "Student/Faculty Ratio"
                               , "Percent of alumni donated"
                               , "Instructional expenditure per student"
                               , "Graduation rate"), 
          float.env = "sidewaystable", 
          table.placement = "H")


# create the table and store in 'x'
univ = rbind("University 1", "University 2")
elite = rbind("No", "Yes")
gradrate = rbind(new1[,6], new2[,6])
preds = rbind(newpred1[,1], newpred2[,1])
lwr = rbind(newpred1[,2], newpred2[,2])
upr = rbind(newpred1[,3], newpred2[,3])

x <- data.frame(univ
                , elite
                , gradrate
                , preds
                , lwr
                , upr
                , outstate = rbind(new1[,"Outstate"], new2[,"Outstate"]))

fm.table <- xtable(x, 
                   digits = 2,
                   caption = "Prediction Table", 
                   label = "pred_table", 
                   table.placement="H")

align(fm.table) <- "|l|rrrrrrr|"

print(fm.table)

## diagnostic

par(mfrow=c(2,2))
plot(fm1)

