#### Packages ####
library(tidyverse)
library(magrittr)
library(ggcorrplot)
library(MASS)
library(leaps)
library(car)
library(stargazer)


#### Parameters ####

my_col <- "#5a8fa1"


#### Functions ####

ScatterPlotFunction <- function(df, xvar, xlab, smooth_method = "loess"){
  ggplot(df, aes_string(x = xvar, y = "lexpen")) + 
    geom_point(size = 2, alpha = 0.4) + 
    geom_smooth(method = smooth_method, se = FALSE, color = "red", size = 1) + 
    theme_bw() + 
    labs(y = "Log-Expenditures", 
         x = xlab)
}


#### Load data ####

ny <- read.table("data/cs73.dat", header = T)

ny2 <- na.omit(ny)


#### Summary Statistic Tables ####

stargazer(ny2 %>% dplyr::select(-obs, -st, -co, -id))


#### Add transformed variables ####

ny2 %<>% 
  mutate(lexpen = log(expen), 
         lwealth = log(wealth), 
         lpop = log(pop), 
         lpint = log(pint), 
         ldens = log(dens), 
         lincome = log(income), 
         lgrowr = case_when(
           growr > 0 ~ log(growr + 1.01), 
           TRUE ~ -log(-growr + 1.01)))


#### Explore Target variables #### 

## generate histogram of expenditure to determine y-var assumption met 
## the plot look very right skew indicating we should transform the y variable
ggplot(ny2, aes(expen)) + 
  geom_histogram(aes(y = stat(density)), bins = 30, col = "dark gray", fill = my_col) +
  geom_density(size = 1, color = "red") + 
  theme_bw() + 
  labs(x = "Expenditures", 
       y = "Density")

## after transforming the y variable to using log tranformation 
## expen is now seems to be normal distributed 
ggplot(ny2, aes(lexpen)) + 
  geom_histogram(aes(y = stat(density)), bins = 30, col = "dark gray", fill = my_col) +
  geom_density(size = 1, color = "red") + 
  theme_bw() + 
  labs(x = "Log-Expenditures", 
       y = "Density")

## perform qq plot
ggplot(ny2, aes(sample = lexpen)) + 
  stat_qq(size = 2, alpha = .4) + 
  stat_qq_line(col = "red", size = 1) + 
  theme_bw() + 
  labs(y = "Log-Expenditures", 
       x = "Theoretical")


#### Explore Independent Variables ####

## scatter plots of predictors and target 
ScatterPlotFunction(ny2, "lwealth", "Log-Wealth")
ScatterPlotFunction(ny2, "lpop", "Log-Population")
ScatterPlotFunction(ny2, "lpint", "% Log Intergovernmental Funds")
ScatterPlotFunction(ny2, "ldens", "Log-Density")
ScatterPlotFunction(ny2, "lincome", "Log-Income")
ScatterPlotFunction(ny2, "lgrowr", "Log-Grow Rate")


#### Subset data ####
## subsetting the data based on regions of log-population and log-density 
## where the relationship with log-expenditure is linear
set2 <- ny2 %>% 
  filter(lpop > 8.3 & ldens > 4.5)

## use scatterplot to explore the relationship between 
## x and y again with new set 

ScatterPlotFunction(set2, "lwealth", "Log-Wealth", "lm")
ScatterPlotFunction(set2, "lpop", "Log-Population", "lm")
ScatterPlotFunction(set2, "lpint", "% Log Intergovernmental Funds", "lm")
ScatterPlotFunction(set2, "ldens", "Log-Density", "lm")
ScatterPlotFunction(set2, "lincome", "Log-Income", "lm")
ScatterPlotFunction(set2, "lgrowr", "Log-Grow Rate", "lm")

## correlation plot ##
corr_table <- set2 %>% 
  dplyr::select(lwealth, lpop, lpint, ldens, lincome, lgrowr) %>% 
  cor()

## lpop and ldens have high positive correlation = multicolinary problem 
## lincome and lwealth has high possitive correlation also 
ggcorrplot(corr_table, hc.order = TRUE, 
           outline.col = "white",
           colors = c(my_col, "white", "red"), type = "upper")


#### Model Development ####

## since lpop and ldens are highly correlated, we cannot put them in the same model 

fit1 <- lm(lexpen ~ lwealth + lpop + lpint + ldens + lincome + lgrowr, 
           data = set2)

## after doing stepwise selection 
## the final model include the same 6 variables
fit1_step <- stepAIC(fit1, direction = "both", trace = FALSE)

## looking at the summary of regression we can see that 
## the coefficient for density is not in the right direction 
## scatter plot shows positive relationship between ldens and lexpen 
## suspecting the model has high VIF on ldens and lpop
summary(fit_step)

## try removing lpop  
fit2 <- lm(lexpen ~ lwealth + lpint + ldens + lincome + lgrowr, data = set2)

summary(fit2)

fit2_step <- stepAIC(fit2, direction = "both", trace = FALSE)

## try removing ldens 
fit3 <- lm(lexpen ~ lwealth + lpop + lpint + lincome + lgrowr, data = set2)

summary(fit3)

fit3_step <- stepAIC(fit3, direction = "both", trace = FALSE)

## coefficient seems right now 
## stepwise for both models also give the same models
## picking model 3 to be the final model since it has 
## lower MSE and higher Adjusted R-squared

#### Interaction ####

fit4 <- lm(lexpen ~ .*., 
          data = set2 %>% dplyr::select(lexpen, lwealth, lpop, lpint, lincome, lgrowr))

fit4_step <- stepAIC(fit4)

## coefficient seems quite off for pint  
summary(fit4_step)


#### final model ####
## pick final model to have no interaction terms 
fit_final <- fit3_step


#### Model Diagnostic ####

## Residuals
## add prediction values and rstudent residuals into set2 
set2 %<>% 
  mutate(predict = predict(fit_final), 
         rstudent = rstudent(fit_final))

## plot studenttized residuals vs predicted values 
ggplot(set2, aes(x = predict, y = rstudent)) + 
  geom_point(size = 2, alpha = .6) + 
  theme_bw() + 
  labs(x = "Predicted", 
       y = "Studentized Residuals")

## identify the outlier in residual vs predicted plot 
## extremely large wealth and dens 
## extremely low grow rate 
set2 %>% 
  filter(rstudent == min(rstudent))


## Normality of Residuals
set2 %<>% 
  mutate(dnorm_rstudent = dnorm(rstudent))

ggplot(set2, aes(x = rstudent)) + 
  geom_histogram(aes(y = stat(density)), bins = 30, col = "dark gray", fill = my_col) + 
  geom_line(aes(y = dnorm_rstudent)) +
  theme_bw() + 
  labs(title = "Distribution of Studentized Residuals", 
       x = "Studentized Residuals", 
       y = "Count")

## Q-Q plot for studentized resid
qqPlot(fit_final, main="QQ Plot", ylab="Studentized Residuals")

## Influential Observations
## Cook's D plot
## identify D values > 4/(n-p-1) as a guide; 
## Cook and Weisberg recommend 0.5 and 1 (R uses these guides in default diagnostic plots below)
cutoff <- 4/((nrow(set2) - length(fit_final$coefficients) - 2))

diag <- broom::augment(fit_final) %>% mutate(Index = 1:nrow(.))

diag %<>% 
  mutate(high_cooksd = case_when(
    .cooksd > cutoff ~ 1, TRUE ~ 0), 
    col_stdresid = case_when(
      .std.resid > 0 ~ 1, 
      .std.resid < 0 ~ 0), 
    high_hat = case_when(
      .hat > .1 ~ 1, 
      TRUE ~ 0))

## cook's distant ggplot
ggplot(diag, aes(x = Index, y = .cooksd)) + 
  geom_bar(stat = "identity") +
  labs(y = "Cook's Distance") + 
  theme_minimal() +  
  geom_label(data = diag %>% filter(.cooksd > cutoff + .03), 
             aes(label = Index), label.size = NA, size = 3)

## Influence plot: studentized residuals vs. hat matrix diagonals (leverage) with bubbles a function of Cook's D
## Interactive, so can click to identify high leverage/influential/outlying points
influencePlot(fit_final, id.method="identify", 
              main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

## VIF
## vif score seem very close to 1 
vif(fit_final) # closer to 1 the better; 5-10 is moderate

# All encompansing R default regression model diagnostics
par(mfrow=c(2,2))
plot(fit_final)


#### Predictions

## Account for sigma^2
sd_fit <- sd(fit_final$resid)

## Read Warick and monroe data 
wm <- readxl::read_xlsx("data/warwick-and-monroe.xlsx")

## create log transformation variables for wm
wm %<>% 
  mutate(lwealth = log(wealth), 
         lpop = log(pop), 
         lpint = log(pint), 
         ldens = log(dens), 
         lincome = log(income), 
         lgrowr = case_when(
           growr > 0 ~ log(growr + 1.01), 
           TRUE ~ -log(-growr + 1.01)))


## generate prediction for wm 
wm$lexpen_pred <- predict(fit_final, newdata = wm)

wm %<>%
  mutate(lexpen_pred = predict(fit_final, newdata = wm)) 

wm %<>% 
  cbind(exp(predict(fit_final, wm, interval="prediction")+sdfit^2/2))













