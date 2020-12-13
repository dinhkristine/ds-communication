#### Packages ####

library(tidyverse)
library(magrittr)
library(tidymodels)
library(lubridate)
library(corrplot)


#### Parameters ####

avocado_color <- "#D8D881"


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


#### Data #### 

avocado <- read_csv("data/avocado-updated-2020.csv")


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
  mutate(month = month(date))



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


















