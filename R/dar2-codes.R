#### Load packages ####
library(MASS)
library(magrittr)
library(tidyverse)
library(ggcorrplot)
library(car)
library(broom)


#### Functions ####

#' Explore variables function, returning plots of variables count and average proportion
#'
#' @param df dataframe
#' @param xvar independent variable 
#' @param count return histogram of count,default is TRUE
#' @param x_axis x axis name 
#'
#' @return
#' @export
#'
#' @examples
ExploreVariable <- function(df, xvar, count = TRUE, x_axis){
  
  group_df <- df %>% 
    group_by({{xvar}}) %>% 
    summarise(avg_prop_capsule = mean(capsule),
              count = n(), 
              .groups = "drop")
  
  ratio <- max(group_df$count) / max(group_df$avg_prop_capsule)
  
  if(count == TRUE){
    p <- ggplot(group_df, aes(x = factor({{xvar}}), group = 1)) +
      geom_bar(aes(y = count), stat = "identity", fill = "#3e8ddd", col = "white") +
      geom_point(aes(y = avg_prop_capsule * ratio), size = 2, color = "black") + 
      geom_line(aes(y = avg_prop_capsule * ratio), size = 1, color = "black") +
      scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Average Proportion of Penetrated"))
    
    y_lab <- "Number of Penetrated"
    
  } else {
    p <- ggplot(group_df, aes(x = {{xvar}}, group = 1)) +
      geom_point(aes(y = avg_prop_capsule), size = 2, color = "black") + 
      geom_line(aes(y = avg_prop_capsule), size = 1, color = "black") 
    
    y_lab <- "Average Proportion of Penetrated"
  }
  p +
    labs(y = y_lab, x = x_axis) +
    theme_bw()
}

#' Simple binomial regression to run bootstrap for variable selection 
#'
#' @param rp random partition 
#' @param xvars independent variable/s
#' @param nrounds number of times running the model
#'
#' @return
#' @export
#'
#' @examples
BinaryFit <- function(rp, xvars, nrounds){
  
  # select varaibles
  df  <- prostate %>% dplyr::select(capsule, all_of(xvars))
  
  all_auc <- c()
  all_aic <- c()
  all_iteration <- list()
  index <- 1
  
  for(each_rp in rp){
    for (i in 1:nrounds){
      # split train and test randomly based on proportion
      n <- floor(each_rp * nrow(df))
      
      train_ind <- sample(seq_len(nrow(df)), size = n)
      
      train <- df[train_ind, ]
      
      test <- df[-train_ind, ]
      
      # build model 
      fit <- glm(capsule ~ ., data = train, family = "binomial")
      
      # predict 
      preds <- predict(fit, newdata = test, type = "response")
      
      test$preds <- preds
      
      # Validate AUC  
      test_roc <- pROC::roc(response = test$capsule, predictor = test$preds)
      
      auc <- pROC::auc(test_roc)[[1]]
      
      all_auc <- c(all_auc, auc)
      all_aic <- c(all_aic, fit$aic)
    }
    # iteration log 
    all_iteration[[index]] <- data.frame(iteration_date = Sys.time(), 
                                         model_type = "glm",
                                         distibution = "binomial",
                                         random_partition = each_rp, 
                                         nrow_train = nrow(train), 
                                         nrow_test = nrow(test),
                                         nrounds = nrounds, 
                                         n_features = length(xvars),
                                         features = paste(xvars, collapse = ","), 
                                         AIC = mean(all_aic),
                                         auc = mean(all_auc))
    index <- index + 1
  }
  all_iteration %<>% bind_rows()
  
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
  iteration_log <- map(print_list, "result") %>% bind_rows()
  
  return(iteration_log)
}


#### Load data ####

## Load in prostate data set 
prostate <- read.table("data/prostate.txt", header = T)


#### Clean data & Data Pre-processing ####

## check for NA 
sum(is.na(prostate)) # missing data: 3 in race

## omit NA 
prostate %<>%
  na.omit()

## change categorical variables to class factor  
prostate %<>% 
  modify_at(c("dcaps", "dpros", "race"), as.factor)

## add bins for psa 
prostate %<>%
  mutate(psa_bins = cut(psa, breaks = seq(0, 140, 20)))


#### Data Exploratory Analysis ####

## Target Variable - capsule - histogram  
ggplot(prostate, aes(factor(capsule))) + 
  geom_histogram(stat = "count", fill = "#3e8ddd", col = "white") + 
  theme_bw() + 
  labs(x = "Capsule", 
       y = "Count")

## Predictors 
ExploreVariable(prostate, age, count = FALSE, x_axis = "Age")
ExploreVariable(prostate, race, x_axis = "Race")
ExploreVariable(prostate, dpros, x_axis = "Results of Digital Rectal Exam")
ExploreVariable(prostate, dcaps, x_axis = "Detection of Capsular Involvement")
ExploreVariable(prostate, psa_bins, count = FALSE, 
                x_axis = "Prostatic Specific Antigen Value (mg/ml)")
ExploreVariable(prostate, gleason, x_axis = "Total Gleason Score")

# after the variable exploratory, we can see that 
# - age don't have a relationship with capsule 
# - there is little to no relationship with capsule, and unbalance factors
# - dpros has a strong effect of getting penetration by different levels
# - dcaps also has strong effect of getting penetration
# - psa has a strong positive relationship with capsule 
# - gleason also has a strong positive relationship with capsule 
# we can continue the analysis using 4 variables: dpros, dcaps, psa, and gleason


#### Check Correlation for Continuous variables ####
corr_table <- prostate %>%
  select(age, psa, gleason) %>% 
  cor()

ggcorrplot(corr_table, hc.order = TRUE, 
           outline.col = "white",
           colors = c("blue", "white", "red"))

# no high correlation between continuous predictors
# all variables can be in the same model  


#### Model Development ####

## parameter for bootstrapping

xvars <- c("dpros", "psa", "gleason", "dcaps")

rp_par <- c(0.6, 0.7, 0.8, 0.9)     # random partition parameters 

nrounds <- 10                       # number of rounds per model

## Bootstrapping

# initiate print list 
print_list <- list()

# loop
for (n_features in 1:4){
  results <- LoopAllVars(rp = rp_par,
                         nrounds = nrounds, 
                         list_of_xvars = xvars, 
                         number_of_xvars = n_features)
  print_list[[n_features]] <- results
}

# bind iteration log into a dataframe
iteration_log <- print_list %>% bind_rows()

# after running the iteration logs, models with variables dpros, psa, and gleason 
# has the high AUC and low AIC, using train set of 60% prop and test set of 40%


#### Select Model after bootstrap #### 

## Split data to train/test set using random partition with proportion .7/.3 
set.seed(1234)

n <- floor(.6 * nrow(prostate))

train_ind <- sample(seq_len(nrow(prostate)), size = n)

train <- prostate[train_ind, ]

test <- prostate[-train_ind, ]


## start with initial fit using 4 variables 
fit1 <- glm(capsule ~ dpros + psa + gleason,
            family = binomial(link = logit), 
            data = train)


## Checking for Interaction term 

fit2 <- glm(capsule ~ .*.,
            family  = "binomial",
            data    = train %>% dplyr::select(dpros, psa, gleason, capsule))

test$preds2 <- predict(fit2, newdata = test, type = "response")

test_roc <- pROC::roc(response = test$capsule, predictor = test$preds2)
test_auc <- as.data.frame(pROC::auc(test_roc))
aic <- fit2$aic

summary(fit2)

stepAIC(fit2, direction = c("both"))

# although auc for this model is high and AIC is low, 
# each coefficient in the model is not statistically significant 
# so we're not considering this model to be the final model 
# also using model selection function stepAIC, 
# interaction terms are not needed in the model

#### Final Model ####

fit_final <- fit1

## generate predictions using test set 
test$preds <- predict(fit_final, newdata = test, type = "response")

## create percentile to generate lift chart 
test$percentile <- ntile(test$preds, 10)

lift_plot_df <- test %>% 
  group_by(percentile) %>% 
  summarise("Empirical" = mean(capsule), 
            "Indicated" = mean(preds)) %>%  
  gather("key", "value", -percentile)

## Lift chart to visualize the quality of the model 
ggplot(lift_plot_df, aes(x = factor(percentile), y = value, color = key, group = key)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  theme_minimal() + 
  labs(x = "Percentile", 
       y = "Average Proportion Penetrated") + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("black", "#3e8ddd"))

## compute AIC and AUC of final model
aic <- fit_final$aic
test_roc <- pROC::roc(response = test$capsule, predictor = test$preds)
test_auc <- as.data.frame(pROC::auc(test_roc))

tidy <- broom::tidy(fit_final) %>% 
  mutate(OR = exp(fit_final$coefficients)) %>%
  cbind(exp(confint(fit_final))) %>% 
  modify_if(is.numeric, round, 4) 

## wald test 
aod::wald.test(b = coef(fit_final), Sigma = vcov(fit_final), Terms = 4)


## Generate confusion matrix and accuracy  
test %<>% 
  mutate(preds = case_when(preds < 0.5 ~ 0.5, TRUE ~ 1.1))

confusion_matrix <- ftable(test$capsule, test$preds)

accuracy <- sum(diag(confusion_matrix))/nrow(test)*100

accuracy


#### Diagnostics ####

#### Outliers test ####
## Bonferonni p-value for most extreme obs
outlierTest(fit_final) 


## Influential Observations
influencePlot(fit_final, 
              id.method="identify", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

diag <- augment(fit_final) %>% 
  mutate(Index = 1:nrow(.))

cutoff <- 4/((nrow(train) - length(fit_final$coefficients) - 2))

diag %<>% 
  mutate(high_cooksd = case_when(
    .cooksd > cutoff ~ 1, TRUE ~ 0), 
    col_stdresid = case_when(
      .std.resid > 0 ~ 1, 
      .std.resid < 0 ~ 0), 
    high_hat = case_when(
      .hat > .07 ~ 1, 
      TRUE ~ 0))

## cook's distant plot
ggplot(diag, aes(x = Index, y = .cooksd)) + 
  geom_bar(stat = "identity", fill = "#3e8ddd", col = "white") +
  labs(y = "Cook's Distance") + 
  theme_minimal() +  
  geom_label(data = diag %>% filter(.cooksd > cutoff + .005), 
             aes(label = .rownames), label.size = NA, size = 3)


## standard residual plot
ggplot(diag, aes(x = Index, y = .std.resid, color = factor(col_stdresid))) + 
  geom_point(alpha = 0.7) +
  labs(y = "Standard Residuals") + 
  theme_bw() + 
  scale_color_manual(values = c("black", "#3e8ddd")) + 
  theme(legend.position = "none") + 
  geom_label(data = diag %>% filter(.std.resid > 2 | .std.resid < -2), 
             aes(label = .rownames), label.size = NA, size = 3, hjust=0.45, vjust=-.15)

## diagonal hat matrix plot
ggplot(diag, aes(x = Index, y = .hat, color = factor(high_hat))) + 
  geom_point() +
  labs(y = "Diagonal Hat Matrix") + 
  theme_bw() + 
  scale_color_manual(values = c("black", "#3e8ddd")) + 
  theme(legend.position = "none") + 
  geom_label(data = diag %>% filter(.hat > .07), 
             aes(label = .rownames), label.size = NA, size = 3, hjust=0.45, vjust=-.15)

## student residual distribution 
sresid <- studres(fit_final)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit, col = "red")
# looks normal

#### Collinearity  ####
vif(fit_final) # variance inflation factors
# all VIF are low = no multicolinearity 


#### Find influential point in data ####

inf_points <- (diag %>% 
                 filter(.std.resid > 2 | .std.resid < -2 | .cooksd > cutoff | .hat > .07))$.rownames %>% 
  as.numeric()

prostate[inf_points, ] %>% 
  as.data.frame() %>% 
  dplyr::select(capsule, dpros, psa, gleason) %>% 
  mutate(rownames = inf_points) %>% 
  arrange(rownames)







