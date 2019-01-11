install.packages("yelp")
install.packages("rtweet")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("datatable")
install.packages("stats")
install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("remotes")
install.packages("psych")
remotes::install_github("richierocks/yelp")
library("dplyr")
library("ggpubr")
library("rtweet")
library("psych")

#Assigning categories to each restaurant

trying <- FoodOnly %>% 
  mutate(type = ifelse(CATEGORIES == "Fast Food", "fast food",
                                ifelse(CATEGORIES == "Bars", "bars", 
                                       ifelse(CATEGORIES == "Diner", "diner",
                                       ifelse(CATEGORIES == "Cafe", "cafe")))))



# creating df from specific attributes 
stars <- FoodOnly %>% 
  filter(STARS >= 4)

reviews <- FoodOnly %>% 
  filter(REVIEW_COUNT > 47)

not_open <- FoodOnly %>% 
  filter(IS_OPEN == 0)

price_range <- FoodOnly %>% 
  filter(PRICE_RANGE >= 3)

has_wifi <- FoodOnly %>% 
  filter(str_detect(WIFI, "free"))

accepts_ccard <- FoodOnly %>% 
  filter(str_detect(ACCEPTS_CREDITCARD, "TRUE"))

wheelchair_access <- FoodOnly %>% 
  filter(str_detect(WHEELCHAIR_ACCESS, "TRUE"))

diners <- FoodOnly %>% 
  filter(str_detect(CATEGORIES, "Diners"))

bars <- FoodOnly %>% 
  filter(str_detect(CATEGORIES, "Bars"))

fastfood <- FoodOnly %>% 
  filter(str_detect(CATEGORIES, "Fast Food"))

parcelshaswifi <- PittFoodParcels %>% 
  filter(str_detect(WIFI, "free"))

parcelsNOwifi <- PittFoodParcels %>% 
  filter(str_detect(WIFI, "no"))

parcelsBARS <- PittFoodParcels %>% 
  filter(str_detect(CATEGORIES, "Bars"))

shill3 <- shill %>% 
  filter(IS_OPEN == 1)


#writing attributes as csv
write_as_csv(has_wifi, "has_wifi", prepend_ids = TRUE, na = "",
             fileEncoding = "UTF-8")


write_as_csv(stars, "stars", prepend_ids = TRUE, na = "",
             fileEncoding = "UTF-8")

write_as_csv(not_open, "not_open", prepend_ids = TRUE, na = "",
             fileEncoding = "UTF-8")


write_as_csv(price_range, "price_range", prepend_ids = TRUE, na = "",
             fileEncoding = "UTF-8")




########################## CORRELATIONS ##########################
# http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r 

x<- df[9] #review_count
y <- df[8] #stars
cor(x, y, use="everything", method = "pearson")
  #0.1440574


x <- df[9] #review_count
y <- df[10] #is_open
cor(x, y, use="everything", method = "pearson")
  #0.1574527

x <- df[8] #stars
y <- df[10] #is_open
cor(x, y, use="everything", method = "pearson")
   #0.04662034


x <- df[10] #is_open
y <- df[12] #price range
cor(x, y, use="complete.obs", method = "pearson")
     #-0.08407548

x <- df[9] #review_count
y <- df[12] #price range
cor(x, y, use="complete.obs", method = "pearson")
      #0.2053889

x <- df[8] #stars
y <- df[12] #price range
cor(x, y, use="complete.obs", method = "pearson")
      # 0.02715162
cov(x, y, use = "complete.obs", method = "pearson")


ggscatter(df, x = "REVIEW_COUNT", y="PRICE_RANGE", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Reviews", ylab = "Price Range")



x <- has_wifi[9] #review count
y <- has_wifi[12] #price range
cor(x, y, use="complete.obs", method = "pearson")
      #0.2901126


x <- price_range[8] #stars
y <- price_range[9] #review count
cor(x, y, use="complete.obs", method = "pearson")
      #0.2541819

x <- price_range[9] #review count
y <- price_range[10] #is_open
cor(x, y, use="complete.obs", method = "pearson")
      #0.2704901


x <- stars[9] #review count
y <- stars[12] #price range
cor(x, y, use="complete.obs", method = "pearson")
    #0.2443791

x <- stars[10] #
y <- stars[9] #price range
cor(x, y, use="complete.obs", method = "pearson")


x <- fastfood[9] #price_range 
y <- fastfood[12] #review_count
cor(x, y, use="complete.obs", method = "pearson")
      #0.4577991

sside <- SouthSideFlats_ParcelJoin_CSV
x <- sside[25] #
y <- sside[26] #
cor(x, y, use="complete.obs", method = "pearson")
      #0.3586496

x <- sside[27] #
y <- sside[28] #
cor(x, y, use="complete.obs", method = "pearson")

shill <- SquirrellHill_Restaurants_Parcel_Join
x <- shill[27] #
y <- shill[28] #
cor(x, y, use="complete.obs", method = "pearson")


PittFoodParcels<- Pitt_Food_Parcel_Join
x <- PittFoodParcels[24] #
y <- PittFoodParcels[26] #
cor(x, y, use="complete.obs", method = "pearson")

CBD <- CBD_parceljion
CBD$WIFI [CBD$WIFI == "free"] <- 1
CBD$WIFI [CBD$WIFI == "no"] <- 0
CBD$WIFI [CBD$WIFI == "paid"] <- 1
CBD$WIFI <- as.numeric(CBD$WIFI)
x <- CBD[24] #
y <- CBD[30] #
cor(x, y, use="complete.obs", method = "pearson")

x <- PittFoodParcels[24] #
y <- PittFoodParcels[30] #
cor(x, y, use="complete.obs", method = "pearson")

PittFoodParcels$WIFI [PittFoodParcels$WIFI == "free"] <- 1
PittFoodParcels$WIFI [PittFoodParcels$WIFI == "no"] <- 0
PittFoodParcels$WIFI [PittFoodParcels$WIFI == "paid"] <- 1
PittFoodParcels$WIFI <- as.numeric(PittFoodParcels$WIFI) 
PittFoodParcels <- Pitt_Food_Parcel_Join

shill$WIFI [shill$WIFI == "free"] <- 1
shill$WIFI [shill$WIFI == "no"] <- 0
shill$WIFI [shill$WIFI == "paid"] <- 1
shill$WIFI <- as.numeric(shill$WIFI) 

x <- shill[25] #review_count
y <- shill[30] #wifi
cor(x, y, use="complete.obs", method = "pearson")
      #-0.3693391

x <- shill[26] #
y <- shill[30] #
cor(x, y, use="complete.obs", method = "pearson")


x <- shill[24] #stars
y <- shill[30] #wifi
cor(x, y, use="complete.obs", method = "pearson")
#-0.04367569


x <- shill3[24] #
y <- shill3[30] #
cor(x, y, use="complete.obs", method = "pearson")


######### COUNTS ###############

shill %>% 
  count(IS_OPEN)
#21 closed, 62 open

sside%>% 
  count(IS_OPEN)
#43 closed, 87 open

shill %>% 
  count(WIFI)
#free 20, no 43, NA 20

sside%>% 
  count(WIFI)
#free 38, no 52, NA 40

ssidetype <- sside%>% 
  count(CATEGORY1)

type <- FoodOnly %>% 
  count(CATEGORY1)

type2 <- PittFoodParcels %>% 
  count(CATEGORY1)

type3 <- PittFoodParcels %>% 
  count(CATEGORY2)

bars1 <- bars %>% 
  count(ZIPCODE)

wifi2 <- has_wifi%>% 
  count(ZIPCODE)


stars2 <- stars%>% 
  count(ZIPCODE)

foodbyzip <- FoodOnly%>% 
  count(ZIPCODE)

shill2  <- shill %>%
  count(STARS)

CBD1 <- CBD %>% 
  count(CATEGORY1)

CBD2 <- CBD %>% 
  count(CATEGORY2)

############# describe by ################
describeBy(FoodOnly, FoodOnly$WIFI)

describeBy(PittFoodParcels, PittFoodParcels$WIFI)
#free group, 383 places, average 3.69 stars, average review count 108.33
#no group, 524 places, average 3.69 stars, average review count 96.6
#paid group, 10 places, average 3.4 stars, average review count 59.10
#unknown group ....

describeBy(PittFoodParcels, PittFoodParcels$PRICE_RANG)
#group 1, 537 places, review count  47, stars 3.59 
#group 2, 573 places, review count 107.29,  stars 3.61
#group 3, 58 places, review count 159.21, stars 3.69 
#group 4, 11 places,  review count 83.64, stars 3.73
     

summary(PittFoodParcels)


####### GRAPHS WHAT ##############


ggplot(top10, aes(CATEGORY1, fill = CATEGORY1 ) ) +
  geom_bar()

ggplot(top10, aes(x= CATEGORY1, y= n)) + 
  geom_bar(stat="identity", fill= "light blue")


ggplot(top10,  aes(x = factor(""), fill = CATEGORY1) ) +
  geom_bar()

top10 <- type2 %>% 
  filter(n >= 23)




