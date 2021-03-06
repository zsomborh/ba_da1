#cleaning memory
rm(list=ls())

#setting the working directory to current files path (to avoid absolute paths for file handling)
# use this in Rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#in other editors
#setwd(getSrcDirectory()[1]) #in other editors

#calling tidyverse--------------
library(tidyverse)

#define right function for further use 
RIGHT = function(x,n){
  substring(x,nchar(x)-n+1)
}
########## 
#### 1st we join the two dataframes and do a small amount of data cleaning 
##########


#we start with the restaurant dataframe
df_restaurant <- read_csv('../data/raw/all_restaurants_v3_w_dists.csv')
df_restaurant$Restaurant<-gsub('\n','',df_restaurant$Restaurant)
df_restaurant$X1 <- NULL
df_restaurant <- filter(df_restaurant,!duplicated(df_restaurant))

#then we go for product database
df_product <- read_csv('../data/raw/all_products.csv')
df_product$X1 <- NULL
df_product$Restaurant<-gsub('\n','',df_product$Restaurant)
df_product$Restaurant<-gsub('\r','',df_product$Restaurant)
df_product <- filter(df_product,!duplicated(df_product))

#creating merged database out of the two
final_df <- merge(df_product, df_restaurant)
final_df$Price<-gsub("[^0-9\\.]",'',final_df$Price)


########## 
#### 2nd filtering for Margherita pizzas 
##########
logical_vect <- grepl('Marg', final_df[['Product_Name']])
logical_vect2 <- grepl('marg', final_df[['Product_Name']])

pizza_df <- filter(final_df, logical_vect|logical_vect2)


# Manually found some observations that should be dropped while looking through the text (e.g.: menu discounts)
# Since we only care about 32cm margherita pizzas, I removed every other observation with special characteristics such as: 
#   szenhidratcsokkentett, vegan, glutenmentes, teljes kiorlesu, cube_shaped etc...
#   we also dropped Princess Bakery & Bistro since their pricing/was not compatible with the exercise

to_remove <- c('Margitka pizza (32cm)',
               "1 db Margherita pizza (32cm), 1 db Calzone Classico pizza",
               "2 db Margherita pizza (32cm), 2 adag Tiramisu",
               "1 adag Spaghetti Bolognese, 1 db Margherita pizza (32cm)",
               "2 db Margherita pizza (32cm), 2 db Coca-Cola sz�nsavas �d�toital",
               "1 db Margherita pizza (32cm), 1 db Calzone Classico pizza, 2 adag Sajttorta",
               'Margherita pizza men� poharas j�gkr�mmel',
               "Margherita pizza (32cm) + Carte D'or j�gkr�m (465ml) + San Benedetto �sv�nyv�z (0,5l)",
               "Margitszigeti sz�nsavmentes krist�lyv�z (0,5l)",
               "Margitszigeti sz�nsavas krist�lyv�z (0,5l)",
               "Margarita pizza San Francisco st�lus� t�szt�val, sajttal t�lt�tt sz�llel (31cm)",
               "Margarita pizza San Francisco st�lus� t�szt�val, sajttal t�lt�tt sz�llel (38cm)",
               "Margarita - nagy m�ret",
               "Margarita - k�zepes m�ret",
               'H�rman p�rban akci� (Cannibale pizza, Margherita pizza, Prosciutto pizza)',
               '2 db Margherita pizza (32cm)',
               'Margarita twister (6db)',
               'Margherita prima pizza (32cm)'
)

pizza_df<- filter(pizza_df,!pizza_df$Restaurant == 'Princess Bakery & Bistro')
logical_vect3 <- pizza_df$Product_Name %in% to_remove
pizza_df <- filter(pizza_df,!logical_vect3)


#make size of margherita pizzas a new column so that we can filter them out in our population
pizza_df$size <- gsub("[^0-9]",'',pizza_df$Product_Name)
pizza_df$size <- RIGHT(pizza_df$size,2)
pizza_df <- filter(pizza_df,pizza_df$size == 32)

pizza_df$Product_Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(pizza_df$Product_Name))

#Since we are only interested in 32cm Margherita pizzas for given restaurant, we needed to eliminate some more
to_remove2 <- c('Margherita calzone pizza',
                'Campus margherita pr�mium pizza varg�ny�val f�szerezve',
                'Margherita prima pizza', 'Margherita con prosciutto di Parma pizza',
                'LightCarb Veg�n Pepe margarit�ja pizza norm�l t�szt�val',
                'LightCarb Veg�n Pepe margarit�ja pizza v�kony t�szt�val',
                'LightCarb Pepe margarit�ja pizza v�kony t�szt�val',
                'Pepe margarit�ja pizza v�kony t�szt�val',
                'LightCarb Pepe margarit�ja pizza norm�l t�szt�val',
                'Veg�n Pepe margarit�ja pizza v�kony t�szt�val',
                'Veg�n Pepe margarit�ja pizza norm�l t�szt�val', 'Margherita kocka pizza',
                'Margherita veg�n pizza', 'Margherita di bufala pizza', 
                'Extra Margherita pizza',
                'Margar�ta sz�nhidr�t cs�kkentett pizza', 'Margar�ta glut�nmentes pizza',
                'Margar�ta korong', 'Buffala Margaritta pizza', 'Glut�nmentes veg�n Margar�ta pizza',
                'Glut�nmentes Margar�ta pizza', 'Sz�nhidr�tcs�kkentett Margar�ta pizza', 'Margar�ta veg�n pizza',
                'Ol�v�s margarita pizza', 'Teljes ki�rl�s� Ol�v�s margherita pizza', 'Teljes ki�rl�s� Margherita pizza',
                'Margarita calzone pizza', 'Margherita di bufala pizza','Margerita con cotto pizza')

logical_vect4 <- pizza_df$Product_Name %in% to_remove2
pizza_df <- filter(pizza_df,!logical_vect4)
pizza_df <- filter(pizza_df,!duplicated(pizza_df))

#We remove size and product name and rename price column so that we can do an outer join with beverages
colnames(pizza_df)[colnames(pizza_df) == 'Price'] <- 'margherita_pizza_price'
pizza_df$Product_Name <- NULL
pizza_df$size <- NULL

#Removing duplicate values, if any... 
pizza_df <- filter(pizza_df,!duplicated(pizza_df))

########## 
#### 3nd filtering for Coca cola (0,5l) 
##########

logical_vect <- grepl('Coca-Cola', final_df[['Product_Name']])
logical_vect2 <- grepl('Pepsi', final_df[['Product_Name']])

beverage_df <- filter(final_df, logical_vect|logical_vect2)
#manual removal of elements that don't fit criteria - coca cola beverages
to_remove <- c('2 db Margherita pizza (32cm), 2 db Coca-Cola sz�nsavas �d�toital',
               'Tihany pizza (32cm), 0,33l, Coca-Cola',
               '2 db 26cm-es pizza aj�nd�k 2 db Coca-Cola (0,25l) �d�tovel',
               '2 db 32cm-es pizza aj�nd�k 2 db Coca-Cola (0,33l)',
               'Okt�beri 2 db-os pizza men� + 1,25L Coca-Cola')

logical_vect3 <- beverage_df$Product_Name %in% to_remove
beverage_df <- filter(beverage_df,!logical_vect3)

#We create a separate column for sizes and filter for 0.5l products
beverage_df$size <- gsub("[^0-9\\.]",'',beverage_df$Product_Name)
beverage_df <- filter(beverage_df,beverage_df$size == '0.5' | beverage_df$size == '05')


beverage_df$Product_Name <- NULL
beverage_df$size <- NULL
beverage_df <- filter(beverage_df,!duplicated(beverage_df))

colnames(beverage_df)[colnames(beverage_df) == 'Price'] <- 'beverage_price'

########## 
#### 4th Create Final DF
##########
final_df <- merge(pizza_df, beverage_df, all = TRUE)

#removing duplicates and obsevations where beverages is available but pizza is not. 
logical_vect <- is.na(final_df$margherita_pizza_price)
final_df <- filter(final_df,!logical_vect)

#replacing NAs with empty space - we didn't use it in the assingment in the end.
#   final_df$Feature1 <- replace(final_df$Feature1,is.na(final_df$Feature1),'')
#   final_df$Feature2 <- replace(final_df$Feature2,is.na(final_df$Feature2),'')
#   final_df$Feature3 <- replace(final_df$Feature3,is.na(final_df$Feature3),'')
#   final_df$Feature4 <- replace(final_df$Feature4,is.na(final_df$Feature4),'')
#   final_df$Feature5 <- replace(final_df$Feature5,is.na(final_df$Feature5),'')
#   final_df$Tags <- with(final_df, paste0(Feature1,Feature2,Feature3,Feature4,Feature5))

#removing cols that we didn't use for the assignment
final_df$Feature1<-NULL
final_df$Feature2<-NULL
final_df$Feature3<-NULL
final_df$Feature4<-NULL
final_df$Feature5<-NULL
final_df$ProperAddress<-NULL
final_df$Location<-NULL

#standardising colnames with lowercase font + underscore as separator
new_col_names <- c('restaurant','user_rating', 'no_ratings','address', 'city',
                   'restaurant_latitude', 'restaurant_longitude', 'center_latitude',
                   'center_longitude', 'distance_city_center', 'distance_ceu', 'margherita_pizza_price',
                   'beverage_price')
colnames(final_df) <- new_col_names

#-------------------------------------------------------- not important
#writing date and time into output file name to avoid deleting previous outputs
st=format(Sys.time(), "%Y-%m-%d_%H_%M")
write_csv(final_df, paste("../data/clean/assignment1_v2_",st, ".csv", sep = ""))

