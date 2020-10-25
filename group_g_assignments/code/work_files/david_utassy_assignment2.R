#cleaning memory
rm(list=ls())



#setting the working directory to current files path (to avoid absolute paths for file handling)
# use this in Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#in other editors
#setwd(getSrcDirectory()[1]) #in other editors

#calling libbrariies--------------
library(tidyverse)
library(moments)

########## 
#### Getting data + creating useful tables
##########
pizza_data <- read_csv('../data/clean/assignment1_v2_2020-10-24_11_59.csv')

#Creating a table, where we group countryside cities into one group (non_Budapest)
bp_or_not_table <- pizza_data
bp_or_not_table$city[bp_or_not_table$city != 'Budapest'] <- "not_Budapest"

bp_or_not_table_beverage <- pizza_data %>% 
  filter( !is.na( beverage_price ))
bp_or_not_table_beverage$city[bp_or_not_table_beverage$city != 'Budapest'] <- "not_Budapest"

##Create table for only Budapest 
budapest_data <- pizza_data %>% 
  filter( city == 'Budapest' ) 

##Create table for only countryside 
non_budapest_data <- pizza_data %>% 
  filter( city != 'Budapest' ) 

##Create table for only Budapest with beverage
budapest_data_beverage <- pizza_data %>% 
  filter( city == 'Budapest' ) %>% 
  filter( !is.na( beverage_price ))

##Create table for only countryside with beverage
non_budapest_data_beverage <- pizza_data %>% 
  filter( city != 'Budapest' ) %>% 
  filter( !is.na( beverage_price ))

########## 
#### Analyzing only 32 cm margherita pizza prices
##########

ggplot( bp_or_not_table , aes( x = margherita_pizza_price , fill = city ) ) +
  geom_histogram( aes(y = ..density..) , alpha = 0.5, binwidth = 100) +
  geom_density( aes(y = ..density..) , alpha = 0.5 , bw = 100) +
  labs(x='Margherita Pizza Prices',y='Density',fill='Cities')

ggplot(bp_or_not_table, aes(y = margherita_pizza_price, x = city)) +
  geom_boxplot(color = "blue", size = 0.5, width = 0.1, alpha = 0.5) +
  labs(x='Cities',y='Margherita Pizza Prices') + 
  stat_boxplot(geom = "errorbar", width = 0.05,  size = 0.5) + 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")


##########
#### Hypothesis testing (Two sample t-test)
# Questiion: Is the average price of marheritha is the same in Budapest vs. in the rest of Hungary (other big cities: Debrecen, Szeged, Pécs, Miskolc)
#   H0: avg. price of in Budapest - avg. price outside of Budapest == 0
#   H-Alternative:  avg. price of in Budapest - avg. price outside of Budapest != 0

#Test for equality of average prices.
# We can reject N0 if p < 0.05
t.test(margherita_pizza_price ~ city, bp_or_not_table)

# t = 4.0079, df = 82.502, p-value = 0.0001337
# alternative hypothesis: true difference in means is not equal to 0 -> avg. price of in Budapest vs. avg. price outside of Budapest differs signifiicantly
# we CAN reject the Null hypothesis. The probability of doing a false positive error is about 0.013%
# 95 percent confidence interval: 134.9711 400.9516
#sample estimates:
#  mean in group Budapest mean in group not_Budapest 
#1825.739                   1557.778 

# Check the summary statistics of data tables
# Check the summary stat
budapest_sum <- budapest_data %>% summarise(
  mean     = mean(margherita_pizza_price),
  median   = median(margherita_pizza_price),
  std      = sd(margherita_pizza_price),
  iq_range = IQR(margherita_pizza_price), 
  min      = min(margherita_pizza_price),
  max      = max(margherita_pizza_price),
  skew     = skewness(margherita_pizza_price),
  numObs   = sum( !is.na( margherita_pizza_price ) ) )

non_budapest_sum <- non_budapest_data %>% summarise(
  mean     = mean(margherita_pizza_price),
  median   = median(margherita_pizza_price),
  std      = sd(margherita_pizza_price),
  iq_range = IQR(margherita_pizza_price), 
  min      = min(margherita_pizza_price),
  max      = max(margherita_pizza_price),
  skew     = skewness(margherita_pizza_price),
  numObs   = sum( !is.na( margherita_pizza_price ) ) )


marg_pizze_price_summary <- budapest_sum %>% add_row( non_budapest_sum )
marg_pizze_price_summary

rm( all_sum , budapest_sum , non_budapest_sum )

########## 
#### Analyzing 32 cm margherita pizza prices + beverage
##########

ggplot( bp_or_not_table_beverage , aes( x = margherita_pizza_price + beverage_price , fill = city ) ) +
  geom_histogram( aes(y = ..density..) , alpha = 0.5, binwidth = 200) +
  geom_density( aes(y = ..density..) , alpha = 0.5 , bw = 200) +
  labs(x='Margherita Pizza + Beverage Prices',y='Density',fill='Cities')

ggplot(bp_or_not_table_beverage, aes(y =  margherita_pizza_price + beverage_price, x = city)) +
  geom_boxplot(color = "blue", size = 0.5, width = 0.1, alpha = 0.5) +
  labs(x='Cities',y='Margherita Pizza + Beverage Prices') + 
  stat_boxplot(geom = "errorbar", width = 0.05,  size = 0.5) + 
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")



##########
#### Hypothesis testing (Two sample t-test)
# Questiion: Is the average price of marheritha is the same in Budapest vs. in the rest of Hungary (other big cities: Debrecen, Szeged, Pécs, Miskolc)
#   H0: avg. price of in Budapest - avg. price outside of Budapest == 0
#   H-Alternative:  avg. price of in Budapest - avg. price outside of Budapest != 0

#Test for equality of average prices.
# We can reject N0 if p < 0.05
t.test(margherita_pizza_price+beverage_price ~ city, bp_or_not_table)

# Check the summary statistics of data tables
# Check the summary stat
budapest_sum <- budapest_data_beverage %>% summarise(
  mean     = mean(margherita_pizza_price+beverage_price),
  median   = median(margherita_pizza_price+beverage_price),
  std      = sd(margherita_pizza_price+beverage_price),
  iq_range = IQR(margherita_pizza_price+beverage_price), 
  min      = min(margherita_pizza_price+beverage_price),
  max      = max(margherita_pizza_price+beverage_price),
  skew     = skewness(margherita_pizza_price+beverage_price),
  numObs   = sum( !is.na( margherita_pizza_price+beverage_price ) ) )

non_budapest_sum <- non_budapest_data_beverage %>% summarise(
  mean     = mean(margherita_pizza_price+beverage_price),
  median   = median(margherita_pizza_price+beverage_price),
  std      = sd(margherita_pizza_price+beverage_price),
  iq_range = IQR(margherita_pizza_price+beverage_price), 
  min      = min(margherita_pizza_price+beverage_price),
  max      = max(margherita_pizza_price+beverage_price),
  skew     = skewness(margherita_pizza_price+beverage_price),
  numObs   = sum( !is.na( margherita_pizza_price+beverage_price ) ) )


marg_pizze_bev_price_summary <- budapest_sum %>% add_row( non_budapest_sum )
marg_pizze_bev_price_summary

rm(budapest_sum , non_budapest_sum )



