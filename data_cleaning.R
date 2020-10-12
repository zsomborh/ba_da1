#calling tidyverse--------------
library(tidyverse)

########## 
#### 1st we join the two dataframes and do a small amount of data cleaning 
##########
#we start with the restaurant dataframe
df_restaurant <- read_csv('all_restaurants.csv')
df_restaurant$Restaurant<-gsub('\n','',df_restaurant$Restaurant)
df_restaurant$X1 <- NULL
df_restaurant <- filter(df_restaurant,!duplicated(df_restaurant))

#than we go for product database
df_product <- read_csv('all_products.csv')
df_product$X1 <- NULL
df_product$Restaurant<-gsub('\n','',df_product$Restaurant)
head(df)

#creating merged database out of the two
final_df <- merge(df_product, df_restaurant)
final_df$Price<-gsub("[^0-9\\.]",'',final_df$Price)
final_df$Price


########## 
#### 2nd filtering for Margherita pizzas 
##########
logical_vect <- grepl('Marg', final_df[['Product_Name']])
logical_vect2 <- grepl('marg', final_df[['Product_Name']])

Marg_df <- filter(final_df, logical_vect)
marg_df <- filter(final_df,logical_vect2)
pizza_df <- rbind(Marg_df,marg_df)

# Manually found some observations that should be dropped
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
               '2 db Margherita pizza (32cm)')

logical_vect3 <- pizza_df$Product_Name %in% to_remove
pizza_df <- filter(pizza_df,!logical_vect3)
head(pizza_df)

#filter out smaller margherita pizzas only

#-- code to come here 


#-------------------------------------------------------- not important
#write.csv(df_product,'./df_product_for_excel.csv')
#write.csv(df_restaurant,'./df_restaurant_for_excel.csv')
#write.csv(final_df,'./final_product_for_excel.csv')
#szenhidratcsokkentett, vegan, glutenmentes, teljes kiorlesu, akciok