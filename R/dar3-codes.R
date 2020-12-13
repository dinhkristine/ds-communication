#### Packages ####

library(magrittr)
library(tidymodels)
library(lubridate)
library(corrplot)
library(MASS)
library(broom)
library(car)
library(tidyverse)


#### Parameters ####

avocado_color <- "#D8D881"


#### Functions ####

ExploreVariable <- function(df, xvar, count = TRUE, x_axis){
  
  group_df <- df %>% 
    group_by({{xvar}}) %>% 
    summarise(average_price = mean(average_price),
              count = n(), 
              .groups = "drop")
  
  ratio <- max(group_df$count) / max(group_df$average_price)
  
  if(count == TRUE){
    p <- ggplot(group_df, aes(x = factor({{xvar}}), group = 1)) +
      geom_bar(aes(y = count), stat = "identity", fill = avocado_color, col = "white") +
      geom_point(aes(y = average_price * ratio), size = 2, color = "black") + 
      geom_line(aes(y = average_price * ratio), size = 1, color = "black") +
      scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Average Avocado Price in Dollars"), 
                         label = scales::comma)
    
    y_lab <- "Count"
    
  } else {
    p <- ggplot(group_df, aes(x = {{xvar}}, group = 1)) +
      geom_point(aes(y = average_price), size = 2, color = "black") + 
      geom_line(aes(y = average_price), size = 1, color = "black") 
    
    y_lab <- "Average Avocado Price in Dollars"
  }
  p +
    labs(y = y_lab, x = x_axis) +
    theme_bw()
}


ValidationTable <- function(fit, model_type){
  mod <- fit
  fit_summary <- tibble(Model = model_type, 
                        "Number of Features" = length((coef(mod) %>% names())[-1]), 
                        MSE = mean(mod$residuals^2), 
                        Adj.R.squared = summary(mod)$adj.r.squared, 
                        F.statistics = summary(mod)$fstatistic[[1]], 
                        AIC = AIC(mod))
  return(fit_summary)
}

#### Data #### 

avocado <- read_csv("data/avocado-updated-2020.csv")


# descriptive statistics of the data 

summary(avocado %>% 
          select_if(is.numeric))


# add deciles to continuous response 

avocado %<>% 
  mutate(
    total_volume_bins = as_factor(cut(total_volume, breaks = 10, 
                                      include.lowest = TRUE, labels = FALSE)), 
    "4046_bins" = as_factor(cut(`4046`, breaks = 10, 
                                include.lowest = TRUE, labels = FALSE)), 
    "4225_bins" = as_factor(cut(`4225`, breaks = 10, 
                                include.lowest = TRUE, labels = FALSE)), 
    "4770_bins" = as_factor(cut(`4770`, breaks = 10, 
                                include.lowest = TRUE, labels = FALSE)), 
    total_bags_bins = as_factor(cut(total_bags, breaks = 10, 
                                    include.lowest = TRUE, labels = FALSE)), 
    small_bags_bins = as_factor(cut(small_bags, breaks = 10, 
                                    include.lowest = TRUE, labels = FALSE)), 
    large_bags_bins = as_factor(cut(large_bags, breaks = 10, 
                                    include.lowest = TRUE, labels = FALSE)), 
    xlarge_bags_bins = as_factor(cut(xlarge_bags, breaks = 10, 
                                     include.lowest = TRUE, labels = FALSE)))

# add month

avocado %<>% 
  mutate(month = factor(month(date)))


# feature engineer 

price_by_location <- avocado %>% 
  group_by(geography) %>% 
  summarise(average_price = mean(average_price), 
            .groups = "drop") %>% 
  mutate(average_price_bins = cut(average_price, breaks = 4, 
                                  include.lowest = TRUE, labels = FALSE)) %>% 
  dplyr::select(geography, geography_bins = average_price_bins) %>% 
  distinct()

avocado %<>% 
  left_join(price_by_location, by = "geography") %>% 
  modify_at("geography_bins", as_factor)


# log tranformation of total_volume

avocado %<>% 
  mutate(log_total_volume = log(total_volume)) %>% 
  mutate(log_total_volume_bins = as_factor(cut(log_total_volume, breaks = 10,
                                               include.lowest = TRUE, labels = FALSE)))


#### Explore #### 

# target variable: average_avocado price 

ggplot(avocado, aes(average_price)) + 
  geom_histogram(fill = avocado_color, bins = 30, col = "white") + 
  theme_bw() + 
  labs(x = "Average Avocado Price in Dollars", y = "Count") + 
  scale_y_continuous(label = scales::comma)


# continuous predictors

ExploreVariable(avocado, total_volume_bins, count = FALSE, 
                x_axis = "Total Number of Avocados Sold")
ExploreVariable(avocado, log_total_volume_bins, count = FALSE, 
                x_axis = "Log of Total Number of Avocados Sold")

ExploreVariable(avocado, `4046_bins`, count = FALSE, 
                x_axis = "Total Number of Avocados with PLU 4046 Sold")
ExploreVariable(avocado, `4225_bins`, count = FALSE, 
                x_axis = "Total Number of Avocados with PLU 4225 Sold")
ExploreVariable(avocado, `4770_bins`, count = FALSE, 
                x_axis = "Total Number of Avocados with PLU 4770 Sold")
ExploreVariable(avocado, total_bags_bins, count = FALSE, 
                x_axis = "Total Number of Bags Sold")
ExploreVariable(avocado, small_bags_bins, count = FALSE, 
                x_axis = "Total Number of Small Bags Sold")
ExploreVariable(avocado, large_bags_bins, count = FALSE, 
                x_axis = "Total Number of Large Bags Sold")
ExploreVariable(avocado, xlarge_bags_bins, count = FALSE, 
                x_axis = "Total Number of Extra Large Bags Sold")

# categorical predictors 

ExploreVariable(avocado, type, count = TRUE, 
                x_axis = "Avocado Type")
ExploreVariable(avocado, year, count = TRUE, 
                x_axis = "Year of Observation")
ExploreVariable(avocado, month, count = TRUE, 
                x_axis = "Month of Observation")
ExploreVariable(avocado, geography, count = FALSE, 
                x_axis = "Geography of The Avocado") + 
  coord_flip()
ExploreVariable(avocado, geography_bins, count = TRUE, 
                x_axis = "Geography of the Avocado Group")


# correlation 

corr_table <- avocado %>% 
  dplyr::select(total_volume, `4046`, `4225`, `4770`, total_bags, small_bags, 
                large_bags, xlarge_bags, average_price) %>% 
  cor()

corr_table %>% 
  {.[order(abs(.[, 1]), decreasing = TRUE), 
     order(abs(.[, 1]), decreasing = TRUE)]} %>%
  corrplot(method = "number", type = "upper") 


#### Model Development ####

# select significant variables 

avocado %<>% 
  dplyr::select(average_price, 
                log_total_volume, 
                month, 
                type, 
                geography_bins)


# split data into train and test set 

set.seed(123)

avocado_split <- initial_split(avocado, strata = average_price)

avocado_train <- training(avocado_split)

avocado_test <- testing(avocado_split)


# Initial model with all predictors

init_fit <- lm(average_price ~ ., 
               data = avocado_train)


# variable selection using stepAIC 

step_fit <- stepAIC(init_fit, direction = "both", trace = FALSE)


# interaction 

int_fit <- lm(average_price ~ .*., 
              data = avocado_train)


# variable selection for interaction model 

int_step_fit <- stepAIC(int_fit, direction = "both", trace = FALSE)


# generate iteration log 

init_fit_summary <- ValidationTable(init_fit, "Initial Model")
step_fit_summary <- ValidationTable(step_fit, "Stepwise Model")
int_fit_summary <- ValidationTable(int_fit, "Model with Interaction Terms")
int_step_fit_summary <- ValidationTable(int_step_fit, "Stepwise Model with Interaction Terms")

bind_rows(init_fit_summary, 
          step_fit_summary, 
          int_fit_summary, 
          int_step_fit_summary) %>% 
  modify_if(is.numeric, round, 3)


# final model

final_fit <- init_fit 

final_fit %>% 
  tidy() %>% 
  modify_if(is.numeric, round, 3)


#### Model Diagnostic ####

# add rownames 

avocado_train %<>%
  rownames_to_column()

# Residuals

avocado_train %<>% 
  mutate(predict = predict(final_fit), 
         rstudent = rstudent(final_fit))


## Influential Observations
## Cook's D plot
## identify D values > 4/(n-p-1) as a guide; 
## Cook and Weisberg recommend 0.5 and 1 (R uses these guides in default diagnostic plots below)

cutoff <- 4/((nrow(avocado_train) - length(final_fit$coefficients) - 2))

diag <- augment(final_fit) %>% 
  mutate(Index = 1:nrow(.))

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

ggplot(diag, aes(x = as.numeric(Index), y = .cooksd)) + 
  geom_bar(stat = "identity", col = avocado_color) +
  labs(y = "Cook's Distance") + 
  theme_minimal() + 
  geom_label(data = diag %>% filter(.cooksd > cutoff + .001), 
             aes(label = Index), label.size = NA, size = 3) + 
  scale_x_continuous(label = scales::comma)


## Normality of Residuals

ggplot(diag, aes(x = .std.resid)) + 
  geom_histogram(bins = 30, col = "white", fill = avocado_color) + 
  theme_bw() + 
  labs(x = "Studentized Residuals", 
       y = "Count") + 
  scale_y_continuous(label = scales::comma)


# studentize residual plot 

ggplot(diag, aes(x = Index, y = .std.resid)) + 
  geom_point(alpha = 0.2, col = avocado_color) +
  labs(y = "Standard Residuals", x = "Index") + 
  theme_bw() +  
  theme(legend.position = "none") + 
  geom_label(data = diag %>% filter(.std.resid > 5 | .std.resid < -5), 
             aes(label = Index), label.size = NA, size = 3, hjust=-.25, vjust=.3) + 
  scale_x_continuous(label = scales::comma)


# outlier and influential points 

diag %>% 
  filter(.std.resid > 5 | .std.resid < -5 | .cooksd > cutoff + .001) %>% 
  dplyr::select(average_price, log_total_volume, month, type, geography_bins)


## VIF
## vif score seem very close to 1 
vif(final_fit) # closer to 1 the better; 5-10 is moderate


#### Predictions

# Account for sigma^2

sd_fit <- sd(final_fit$resid)


# predict 

avocado_test %<>%
  mutate(average_price_preds = predict(final_fit, newdata = .))


# errors 

avocado_test %<>%
  mutate(average_price_error = average_price - average_price_preds)

mse <- mean(avocado_test$average_price_error)^2


# create lift chart 

avocado_test %<>%
  mutate(average_price_decile = ntile(average_price_preds, n = 10))

decile_price <- avocado_test %>% 
  group_by(average_price_decile) %>% 
  summarise(Actual = mean(average_price), 
            Predicted = mean(average_price_preds), 
            .groups = "drop") %>% 
  gather(key, price, -average_price_decile)

ggplot(decile_price, 
       aes(x = factor(average_price_decile), y = price, 
           group = key, color = key)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) +
  theme_bw() + 
  scale_color_manual(values = c("black", avocado_color)) + 
  labs(x = "Decile", 
       y = "Average Avocado Price in Dollars") + 
  theme(legend.title = element_blank(), legend.position = c(0.15,.8))








