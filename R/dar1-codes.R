#### Packages ####
library(tidyverse)
library(magrittr)
library(ggcorrplot)
library(MASS)
library(leaps)

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

## not helpful at all 
step.model <- stepAIC(fit1, direction = "both", trace = FALSE)

## we will create a function to model all posible combination and compare 
## 2 metric of validation are R-square adjusted (higher = good) and MSE (lower = good) 


LinearFitFunction <- function(xvars){
  
  # select varaibles
  df  <- fire %>%  select(large_fire, xvars)
  
  all_mse <- c()
  all_rsq_adj <- c()
  all_iteration <- list()
  index <- 1
  
  
  
  
  return(all_iteration)
}

SafelyBinaryFit <- safely(BinaryFit)


#' Function to loop all the models through each variable 
#'
#' @param rp 
#' @param nrounds 
#' @param list_of_xvars 
#' @param number_of_xvars 
#'
#' @return
#' @export
#'
#' @examples
LoopAllVars <- function(rp, nrounds, list_of_xvars, number_of_xvars){
  # group variables
  vars <- combn(list_of_xvars, number_of_xvars)
  
  # how many group do we have?
  no_vars <- dim(vars)[[2]]
  
  # initiate print list and index 
  print_list <- list()
  k <- 1
  
  for (i in 1:no_vars) {
    iteration_log <- SafelyBinaryFit(rp, xvars = vars[,i], nrounds)
    print_list[[k]] <- iteration_log
    k <- k + 1
  }
  # map all separate log to bind them together into a dataframe 
  iteration_log <- map(print_list, "result") %>% rbind_list()
  
  return(iteration_log)
}








