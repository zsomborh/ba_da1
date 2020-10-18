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
df_restaurant <- read_csv('./data_analysis1/all_restaurants_v3_w_dists.csv')
df_restaurant$Restaurant<-gsub('\n','',df_restaurant$Restaurant)
df_restaurant$X1 <- NULL
df_restaurant <- filter(df_restaurant,!duplicated(df_restaurant))

#then we go for product database
df_product <- read_csv('./data_analysis1/all_products.csv')
df_product$X1 <- NULL
df_product$Restaurant<-gsub('\n','',df_product$Restaurant)
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
# We couldn't filter for features such as:
  #szenhidratcsokkentett, vegan, glutenmentes, teljes kiorlesu,
  
to_remove <- c('Margitka pizza (32cm)',
               "1 db Margherita pizza (32cm), 1 db Calzone Classico pizza",
               "2 db Margherita pizza (32cm), 2 adag Tiramisu",
               "1 adag Spaghetti Bolognese, 1 db Margherita pizza (32cm)",
               "2 db Margherita pizza (32cm), 2 db Coca-Cola szénsavas üdítõital",
               "1 db Margherita pizza (32cm), 1 db Calzone Classico pizza, 2 adag Sajttorta",
               'Margherita pizza menü poharas jégkrémmel',
               "Margherita pizza (32cm) + Carte D'or jégkrém (465ml) + San Benedetto ásványvíz (0,5l)",
               "Margitszigeti szénsavmentes kristályvíz (0,5l)",
               "Margitszigeti szénsavas kristályvíz (0,5l)",
               "Margarita pizza San Francisco stílusú tésztával, sajttal töltött széllel (31cm)",
               "Margarita pizza San Francisco stílusú tésztával, sajttal töltött széllel (38cm)",
               "Margarita - nagy méret",
               "Margarita - közepes méret",
               'Hárman párban akció (Cannibale pizza, Margherita pizza, Prosciutto pizza)',
               '2 db Margherita pizza (32cm)',
               'Margarita twister (6db)')

logical_vect3 <- pizza_df$Product_Name %in% to_remove
pizza_df <- filter(pizza_df,!logical_vect3)
pizza_df <- filter(pizza_df,!duplicated(pizza_df))

#make size of margherita pizzas a new column
pizza_df$size <- gsub("[^0-9]",'',pizza_df$Product_Name)
pizza_df$size <- RIGHT(pizza_df$size,2)
pizza_df$Product_Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(pizza_df$Product_Name))

########## 
#### 3nd filtering for Coca cola (0,5l) 
##########

logical_vect <- grepl('Coca-Cola', final_df[['Product_Name']])
cola_df <- filter(final_df, logical_vect)
to_remove <- c('2 db Margherita pizza (32cm), 2 db Coca-Cola szénsavas üdítõital',
               'Tihany pizza (32cm), 0,33l, Coca-Cola',
               '2 db 26cm-es pizza ajándék 2 db Coca-Cola (0,25l) üdítõvel',
               '2 db 32cm-es pizza ajándék 2 db Coca-Cola (0,33l)',
               'Októberi 2 db-os pizza menü + 1,25L Coca-Cola')

logical_vect2 <- cola_df$Product_Name %in% to_remove
cola_df <- filter(cola_df,!logical_vect2)
cola_df$size <- gsub("[^0-9\\.]",'',cola_df$Product_Name)
cola_df$Product_Name <- gsub("\\s*\\w*\\.*$", "", cola_df$Product_Name)
cola_df$Product_Name <- gsub("\\s*\\w*\\.*$", "", cola_df$Product_Name)

#cola_df <- filter(cola_df,cola_df$size == '0.5' | cola_df$size == '05')

########## 
#### 4th Create Final DF
##########

final_df <- rbind(pizza_df,cola_df)

final_df$Feature1 <- replace(final_df$Feature1,is.na(final_df$Feature1),'')
final_df$Feature2 <- replace(final_df$Feature2,is.na(final_df$Feature2),'')
final_df$Feature3 <- replace(final_df$Feature3,is.na(final_df$Feature3),'')
final_df$Feature4 <- replace(final_df$Feature4,is.na(final_df$Feature4),'')
final_df$Feature5 <- replace(final_df$Feature5,is.na(final_df$Feature5),'')

final_df$Tags <- with(final_df, paste0(Feature1,Feature2,Feature3,Feature4,Feature5))

final_df$Feature1<-NULL
final_df$Feature2<-NULL
final_df$Feature3<-NULL
final_df$Feature4<-NULL
final_df$Feature5<-NULL
final_df$ProperAddress<-NULL


#-------------------------------------------------------- not important
write.csv(final_df,'./data_analysis1/assignment.csv')
