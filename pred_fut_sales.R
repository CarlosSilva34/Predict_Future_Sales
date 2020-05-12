library(tidyverse)
library(lubridate)
library(scales)
library(caret)
library("Matrix")
library(xgboost)


setwd("C:/Users/Utilizador/Desktop/kaggle/Predict_Future_Sales")
set.seed(2020)


# Load data 

sales_Data <- read.csv('sales_train.csv')
testData <- read.csv('test.csv')
itemsData <- read.csv('items.csv')
shopsData <- read.csv('shops.csv')
itemsCatData <- read.csv('item_categories.csv')


# Preparing the data

sales_Data$date <- as.Date(sales_Data$date, "%d.%m.%Y")

sales_Data <- sales_Data %>% 
  mutate(month = month(date))

sales_Data <- sales_Data %>% 
  mutate(day = day(date))

Sys.setlocale("LC_TIME", "C")
sales_Data <- sales_Data %>% 
  mutate(weekdays = weekdays(date))

sales_Data <- sales_Data %>% 
  mutate(shop_id = as.character(shop_id))

sales_Data <- sales_Data %>% 
  mutate(item_id = as.character(item_id))

sales_Data <- sales_Data %>%
  mutate( revenue = ifelse((item_cnt_day < 0)|(item_price < 0), 0, item_price*item_cnt_day))

testData <- testData %>% 
  mutate(ID = as.character(ID))

testData <- testData %>% 
  mutate(shop_id = as.character(shop_id))

testData <- testData %>% 
  mutate(item_id = as.character(item_id))

itemsData <- itemsData %>% 
  mutate(item_id = as.character(item_id))

itemsData <- itemsData %>% 
  mutate(item_category_id = as.character(item_category_id))

itemsData <- itemsData %>% 
  mutate(item_name = as.character(item_name))

itemsCatData <- itemsCatData %>%
  mutate(item_category_id = as.character(item_category_id))

itemsCatData <- itemsCatData %>%
  mutate(item_category_name = as.character(item_category_name))

shopsData <- shopsData %>%
  mutate(shop_id = as.character(shop_id))

shopsData <- shopsData %>%
  mutate(shop_name  = as.character(shop_name))

# merge data

salesData <- testData %>%
    mutate(tmp_id = 1) %>%
    left_join(data.frame(tmp_id = 1,
                              date_block_num = seq(0, 34, by = 1)), by = "tmp_id") %>%
    left_join(sales_Data, by = c("shop_id", "item_id", "date_block_num")) %>%
    arrange(shop_id, item_id, date) %>%
    left_join(shopsData, by = "shop_id") %>%
    left_join(itemsData, by = "item_id")

rm(sales_Data)


# replace negative value and NA

salesData <- salesData %>%
        mutate( item_cnt_day = ifelse(item_cnt_day < 0, 0, item_cnt_day),
                
                item_price = ifelse(is.na(item_price), 0, item_price))


# Exploring the Data

# Item sales by shop

sales_by_shop <- salesData %>%
  mutate(shop_id = as.factor(shop_id)) %>%
  select(shop_id, item_cnt_day) %>%
  group_by(shop_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = TRUE))

ggplot(data =  sales_by_shop, 
       mapping = aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = shop_id)) +
  geom_histogram(stat = "identity", color = "orange") +
  scale_y_continuous(breaks= seq(0, 160000, by=20000), labels = comma) +
  xlab("Shop ID") + ylab("Item Sales Count")+
  ggtitle(label = "Item Sales by Shop") 


# Item sales by category

item_sales_by_category <- salesData %>%
  mutate(item_category_id = as.factor(item_category_id)) %>%
  select(item_category_id, item_cnt_day) %>%
  group_by(item_category_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = TRUE))

ggplot(data =  item_sales_by_category, 
       mapping = aes(x = reorder(item_category_id,item_cnt_day), 
                     y = item_cnt_day,
                     fill = item_category_id)) +
  geom_histogram(stat = "identity", color = "orange") +
  scale_y_continuous(breaks= seq(0, 220000, by=20000), labels = comma) +
  xlab("Item Category") + ylab("Item Sales Count") +
  ggtitle("Item sales by Category")
        

# Tne most available items by category

most_available_items_by_category <- salesData %>%
  mutate(item_category_id = as.factor(item_category_id)) %>%
  select(item_category_id, item_id) %>%
  group_by(item_category_id) %>%
  summarise(item_id =  n_distinct(item_id))

ggplot(data = most_available_items_by_category,
       mapping = aes(x = reorder(item_category_id,item_id),
                     y = item_id,
                     fill = item_category_id)) +
  geom_histogram(stat = "identity", color = "orange") +
  xlab(" Category ID")+ ylab(" Items Available") +
  scale_y_continuous(breaks= seq(0, 1000, by=100), labels = comma) +
  ggtitle("Tne Most Available Items by Category") 


# The most sold item in the each shop 

most_sold_items_in_shop  <-  salesData %>%
  mutate(item_id = as.factor(item_id)) %>%
  group_by(shop_id, item_id) %>%
  summarise(most_sold_item = sum(item_cnt_day, na.rm=TRUE)) %>%
  filter(most_sold_item == max(most_sold_item)) %>%
  arrange(desc(most_sold_item))

ggplot(data = most_sold_items_in_shop,
       mapping = aes(x = reorder(shop_id, most_sold_item),
                     y = most_sold_item,
                     fill = item_id)) +
  geom_histogram(stat = "identity", color = "orange") +
  scale_y_continuous(breaks= seq(0, 20000, by=4000), labels = comma) +
  xlab("Shop ID") + ylab("Item Sales Count") +
  ggtitle("The Most Sold Items in each Shop ") 
        
        
# Total sales by item category

total_sales_by_category <- salesData %>%
  mutate(item_category_id = as.factor(item_category_id)) %>%
  group_by(item_category_id) %>%
  summarise(sales_category = sum(revenue, na.rm = TRUE)) %>%
  arrange(desc(sales_category))

ggplot(data = na.omit(total_sales_by_category), 
       aes(x = reorder(item_category_id, sales_category),
           y = sales_category,
           fill = item_category_id)) +
  geom_histogram(stat = "identity", color = "orange") +
  scale_y_continuous(breaks= seq(0, 415000000, by=50000000), labels = comma) +
  xlab("Category ID") + ylab("Sales Count")+
  ggtitle("Total Sales by Item Category") 
        

# The most sold item in eack category

most_sold_item_per_category <- salesData %>%
  mutate(item_id = as.factor(item_id)) %>%
  group_by(item_category_id, item_id) %>%
  summarise(totalSales = sum(revenue, na.rm = TRUE)) %>%
  filter(totalSales == max(totalSales)) %>%
  arrange(desc(totalSales))

ggplot(data = na.omit(most_sold_item_per_category),
       aes(x = reorder(item_category_id, totalSales), 
           y = totalSales,
           fill = item_id)) +
  geom_histogram(stat = "identity", color = "orange") +
  scale_y_continuous(breaks= seq(0, 200000000, by=20000000), labels = comma) +
  labs(title = "The Most Sold Item in eack Category",x = "Category ID", y = "Sales Count", fill = "Item ID") 


# Day and month vs total sales 

month_day_total_sales =  salesData %>%
  mutate(month = as.factor(month)) %>%
  mutate(day = as.factor(day)) %>%
  group_by(month, day) %>%
  summarise(totalSales =  sum(revenue, na.rm = TRUE))

ggplot(data = na.omit(month_day_total_sales), 
       aes(x = day, 
           y = totalSales, 
           group =  month, 
           color =  month)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(breaks= seq(0, 34000000, by=4000000), labels = comma) +
  labs(title = "Total Sales by Day and Month", x = "Days", y = "Sales Count", fill = "Months")


ggplot(data = na.omit(month_day_total_sales), 
       aes(x = day, 
           y = totalSales, 
           fill =  factor(day))) +
  geom_histogram(stat = "identity", color = "blue") +
  scale_y_continuous(breaks= seq(0, 30000000, by=9000000), labels = comma) +
  labs(title = "Total Sales by Day and Month", x = "Days", y = "Sales Count", fill = "Days") +
  facet_wrap(~month, ncol = 2)


# Total sales by year

yearSales <- salesData %>%
  mutate(year = as.factor(year(date))) %>%
  group_by(year) %>%
  summarise(yearSale = sum(revenue, na.rm = TRUE))

ggplot(data = na.omit(yearSales), aes(x =  year, y = yearSale, fill =  year))+
  geom_histogram(stat = "identity", color = "blue")+
  scale_y_continuous(breaks= seq(0, 700000000, by=100000000), labels = comma) +
  labs(title = "Total Sales by Year", x = "Year", y = "Sales Count", fill = "Year")+
  geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = yearSale)) 


# Total sales by year and month 

ymSales = salesData %>%
  mutate(year = as.factor(year(date))) %>%
  group_by(year, month) %>%
  summarise(ymSale = sum(revenue, na.rm = TRUE)) %>%
  arrange(year)
ymSales$ymSale = round(ymSales$ymSale, 2)

ggplot(na.omit(ymSales), aes(x =  month, y = ymSale, fill =  year))+
  geom_histogram(stat = "identity", position = "dodge", color = "blue") +
  scale_x_continuous(breaks= seq(0, 12, by=1)) +
  scale_y_continuous(breaks= seq(0, 126000000, by=25000000), labels = comma) +
  labs(title = "Total Sales by Year and Month", x = "Month", y =  "Sales Count", fill = "Year")
        

# Number of items sold by day 

dailySale = salesData %>%
  group_by(date) %>%
  summarise(itemSold =  sum(item_cnt_day, na.rm = TRUE))

ggplot(na.omit(dailySale), aes(x =  date, y = itemSold, color =  itemSold)) +
  geom_line() +
  geom_point()+
  scale_y_continuous(breaks= seq(0, 15000, by=1000), labels = comma) +
  labs(title = "Items Sold by Day", x =  "Date", y = "Items Count")  


# Number of items sold on weekdays 

weekdays_item_Sale = salesData %>%
  group_by(weekdays) %>%
  summarise(itemSold = sum(item_cnt_day, na.rm = TRUE)) %>%
  arrange(desc(itemSold))

ggplot(na.omit(weekdays_item_Sale), aes(x =reorder(weekdays, itemSold), y =  itemSold, fill = weekdays))+
  geom_bar(stat = "identity", color = "blue") +
  scale_y_continuous(breaks= seq(0, 750000, by=100000), labels = comma) +
  labs(title = "Items Sold on Weekdays", x = "Weekdays", y =  "Items count", fill = "Weekdays") +
  geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = itemSold))


# Total sales on weekdays

weekdaysSales = salesData %>%
  group_by(weekdays) %>%
  summarise(totalSale = sum(revenue, na.rm = TRUE)) %>%
  arrange(desc(totalSale))
weekdaysSales$totalSale = round(weekdaysSales$totalSale, 2)

ggplot(na.omit(weekdaysSales), aes(x =reorder(weekdays, totalSale), y =  totalSale, fill = weekdays))+
  geom_bar(stat = "identity", color ="blue") +
  scale_y_continuous(breaks= seq(0, 650000000, by=50000000), labels = comma) +
  labs(title = "Total Sales on Weekdays", x = "Weekdays", y =  "Sales Count", fill = "Weekdays") +
  geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = totalSale)) 

 
# Feature engineering

new_df<- function(df, period){
  
  
  # item_price summarized by shop_id and item_id 
  item_price1 <- df %>%
    filter(!is.na(date)) %>%
    filter(date_block_num < period) %>%
    arrange(shop_id, item_id, date) %>%
    group_by(shop_id, item_id) %>%
    summarise(
      item_price_shop_min = min(item_price, na.rm = TRUE),
      item_price_shop_mean = mean(item_price, na.rm = TRUE),
      item_price_shop_median = median(item_price, na.rm = TRUE),
      item_price_shop_max = max(item_price, na.rm = TRUE),
      item_price_shop_sd = sd(item_price, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # item_price summarized by item_id 
  item_price2 <- df %>%
    filter(!is.na(date)) %>%
    filter(date_block_num < period) %>%
    group_by(item_id) %>%
    summarise(
      item_price_min = min(item_price, na.rm = TRUE),
      item_price_mean = mean(item_price, na.rm = TRUE),
      item_price_median = median(item_price, na.rm = TRUE),
      item_price_max = max(item_price, na.rm = TRUE),
      item_price_sd = sd(item_price, na.rm = TRUE),
      month1 = min(date_block_num, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # sales and item price before 1~6 months 
  n_sales1_6 <- df %>%
    group_by(shop_id, item_id, date_block_num) %>%
    summarise(
      l_sales1 = sum(item_cnt_day, na.rm = TRUE),
      l_price1 = max(item_price, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(shop_id, item_id, date_block_num) %>%
    mutate(
      l_sales2 = lag(l_sales1, 1),
      l_sales3 = lag(l_sales1, 2),
      l_sales4 = lag(l_sales1, 3),
      l_sales5 = lag(l_sales1, 4),
      l_price2 = lag(l_price1, 1),
      l_price3 = lag(l_price1, 2),
      l_price4 = lag(l_price1, 3),
      l_price5 = lag(l_price1, 4),
      l_sales1_3_mean = (l_sales1 + l_sales2 + l_sales3)/3
    ) %>%
    filter(date_block_num == period - 1)
  
  # total num sales summarized by shop_id / item_id, count num of month with no sales
  tot_num_sales <- df %>%
    filter(date_block_num < period) %>%
    mutate(
      is_zero_sales = ifelse(is.na(item_cnt_day), 1 ,0),
      item_cnt_day = ifelse(is.na(item_cnt_day), 0 ,item_cnt_day)
    ) %>%
    group_by(shop_id, item_id, date_block_num) %>%
    summarise(
      total_sales = sum(item_cnt_day, na.rm = TRUE),
      is_zero_sales = max(is_zero_sales, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(shop_id, item_id) %>%
    summarise(
      total_sales_min = min(total_sales, na.rm = TRUE),
      total_sales_mean = mean(total_sales, na.rm = TRUE),
      total_sales_median = median(total_sales, na.rm = TRUE),
      total_sales_max = max(total_sales, na.rm = TRUE),
      total_sales_sd = sd(total_sales, na.rm = TRUE),
      zero_sales = sum(is_zero_sales, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # revenue summarized by shop_id
  rev_by_shop <- df %>%
    filter(date_block_num < period) %>%
    mutate(
      revenue = ifelse(is.na(revenue), 0 ,revenue)
    ) %>%
    group_by(shop_id, date_block_num) %>%
    summarise(
      total_rev_sales = sum(revenue, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(shop_id) %>%
    summarise(
      total_rev_sales_min = min(total_rev_sales, na.rm = TRUE),
      total_rev_sales_mean = mean(total_rev_sales, na.rm = TRUE),
      total_rev_sales_median = median(total_rev_sales, na.rm = TRUE),
      total_rev_sales_max = max(total_rev_sales, na.rm = TRUE),
      total_rev_sales_sd = sd(total_rev_sales, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # items by shop
  item_by_shop <- df %>%
    filter(!is.na(date)) %>%
    filter(date_block_num < period) %>%
    group_by(shop_id) %>%
    summarise(
      n_item = n_distinct(item_id)
    ) %>%
    ungroup()
  
  # maximum sales by item categories
  sales_by_itemcat <- df %>%
    filter(!is.na(date)) %>%
    group_by(shop_id, item_id, date_block_num) %>%
    summarise(
      total_sales = sum(item_cnt_day, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(shop_id, item_id, date_block_num) %>%
    distinct(shop_id, item_id, .keep_all = TRUE) %>%
    filter(date_block_num < period) %>%
    left_join(itemsData, by = "item_id") %>%
    group_by(shop_id, item_category_id) %>%
    summarise(
      p_total_sales_max = max(total_sales, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # total sales selected
  tot_sales_select <- df %>%
    filter(date_block_num == period) %>%
    group_by(shop_id, item_id) %>%
    summarise(
      total_sales = sum(item_cnt_day, na.rm = TRUE),
      price_mean = mean(item_price, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      total_sales = ifelse(is.na(total_sales), 0, total_sales),
      total_sales_select = ifelse(total_sales > 20, 20, total_sales)
    )
  
  # merge data
  final <- tot_sales_select %>%
    left_join(item_price1, by = c("shop_id", "item_id")) %>%
    left_join(item_price2, by = c("item_id")) %>%
    left_join(n_sales1_6, by = c("shop_id", "item_id")) %>%
    left_join(tot_num_sales, by = c("shop_id", "item_id")) %>%
    left_join(rev_by_shop, by = c("shop_id")) %>%
    left_join(item_by_shop, by = c("shop_id")) %>%
    left_join(itemsData, by = "item_id") %>%
    left_join(sales_by_itemcat, by = c("shop_id", "item_category_id")) %>%
    left_join(itemsCatData, by = "item_category_id") %>%
    left_join(shopsData, by = "shop_id") %>%
    mutate(
      # replace na
      l_price1 = ifelse(l_price1 != 0, l_price1,
                        ifelse(is.na(item_price_shop_median), item_price_median, item_price_shop_median)),
      l_price2 = ifelse(l_price2 != 0, l_price2,
                        ifelse(is.na(item_price_shop_median), item_price_median, item_price_shop_median)),
      l_price3 = ifelse(l_price3 != 0, l_price3,
                        ifelse(is.na(item_price_shop_median), item_price_median, item_price_shop_median)),
      l_price4 = ifelse(l_price4 != 0, l_price4,
                        ifelse(is.na(item_price_shop_median), item_price_median, item_price_shop_median)),
      l_price5 = ifelse(l_price5 != 0, l_price5,
                        ifelse(is.na(item_price_shop_median), item_price_median, item_price_shop_median)),
      item_price_shop_min = ifelse(is.na(item_price_shop_min), item_price_min, item_price_shop_min),
      item_price_shop_mean = ifelse(is.na(item_price_shop_mean), item_price_mean, item_price_shop_mean),
      item_price_shop_median = ifelse(is.na(item_price_shop_median), item_price_median, item_price_shop_median),
      item_price_shop_max = ifelse(is.na(item_price_shop_max), item_price_max, item_price_shop_max),
      item_price_shop_sd = ifelse(is.na(item_price_shop_sd), item_price_sd, item_price_shop_sd),
      
      # diff of item price
      d_price1 = l_price1 - item_price_shop_median,
      d_price2 = l_price1 - item_price_median,
      
      # total sales duration
      dur_total_sales = ifelse(is.na(month1), 0, period - month1),
      
      # zero sales period
      zero_sales = ifelse(zero_sales > dur_total_sales, dur_total_sales, zero_sales),
      
      # diff of sales
      d_sales_mean = l_sales1 - total_sales_mean,
      d_sales_max = l_sales1 - total_sales_max,
      d_sales1 = l_sales1 - l_sales2,
      d_sales2 = l_sales1 - l_sales3,
      d_sales3 = l_sales1 - l_sales4,
      d_sales4 = l_sales1 - l_sales5,
      
      # flg of release month 
      r_month = ifelse(dur_total_sales == 0, 1, 0),
      
      # new items num of sales
      l_sales1 = ifelse(r_month == 1, p_total_sales_max, l_sales1),
      
      # month
      month = date_block_num %% 12
    ) %>%
    # replace na
    mutate_at(vars(starts_with("l_"), starts_with("item_price_"), starts_with("d_")), list(~ifelse(is.na(.), 0, .)))
  
  # dummy colomuns
  dum_colums <- dummyVars(~., data = final %>% select(
    item_category_id, item_category_name, shop_name))
  final <- final %>%
    bind_cols(predict(dum_colums, final) %>% as.data.frame())
  
  return(final)
  
}

salesData1 <- new_df(salesData, 32)
salesData2 <- new_df(salesData, 33)
salesData_pred <- new_df(salesData, 34)

# Preparing data
x_salesData1 <- salesData1 %>%
  select(
    starts_with("item_price_"), -month1, starts_with("l_"), starts_with("total_sales_"),
    -total_sales_select, starts_with("total_rev_sales_"), n_item,
    starts_with("d_"), dur_total_sales, zero_sales, starts_with("d_sales_"), r_month, month,
    starts_with("item_category_id"),-item_category_id, -item_category_id0, -l_sales2, -l_sales3, -item_price_shop_sd,
    -item_price_shop_min, -item_price_shop_mean, -item_price_shop_median, -item_price_mean,
    -item_price_median, -item_price_max, -l_price1, -l_price2, -l_price3,-l_price4, -l_price5
  )

y_salesData1 <- salesData1 %>%
  select(total_sales_select)

x_salesData2 <- salesData2 %>%
  select(
    starts_with("item_price_"), -month1, starts_with("l_"), starts_with("total_sales_"),
    -total_sales_select, starts_with("total_rev_sales_"), n_item,
    starts_with("d_"), dur_total_sales, zero_sales, starts_with("d_sales_"), r_month, month,
    starts_with("item_category_id"), -item_category_id, -item_category_id0, -l_sales2, -l_sales3, -item_price_shop_sd,
    -item_price_shop_min, -item_price_shop_mean, -item_price_shop_median, -item_price_mean,
    -item_price_median, -item_price_max, -l_price1, -l_price2, -l_price3,-l_price4, -l_price5
  )

y_salesData2 <- salesData2 %>%
  select(total_sales_select)

# Modeling

x_data <- x_salesData1 %>% as.matrix()
Y_data <- y_salesData1 %>% as.matrix()
  
set.seed(2020)
param<-list(
    max_depth = 4, 
    eta = 0.02, 
    gamma = 0, 
    colsample_bytree = 0.65, 
    subsample = 0.6, 
    min_child_weight = 3
)

  
# nrounds with cross-validation
xgbcv <- xgb.cv( param = param, data = x_data, label = Y_data, nrounds = 1000, 
                 nfold = 10, showsd = F, stratified = T, print_every_n = 250, 
                 early_stopping_rounds = 100, maximize = F)

set.seed(2020)
model_xgb <- xgboost(param = param, data = x_data, label = Y_data,
                       nrounds = xgbcv$best_iteration, importance = TRUE)
  

# check the most important features
mat <- xgb.importance(names(x_data), model = model_xgb)
ggplot(mat[1:40,])+
  geom_bar(aes(x=reorder(Feature, Gain), y=Gain), stat='identity', fill='blue')+
  xlab(label = "Features")+
  coord_flip() +
  ggtitle("Feature Importance")


# RMSE 1
pred_salesData1 <- salesData1 %>%
  bind_cols(pred = predict(model_xgb, newdata = x_salesData1 %>% as.matrix(), type = "response")) %>%
  mutate(error = total_sales_select - pred)

pred_salesData1 %>%
  summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )

# RMSE 2
pred_salesData2 <- salesData2 %>%
  bind_cols(pred = predict(model_xgb, newdata = x_salesData2 %>% as.matrix(), type = "response")) %>%
  mutate(error = total_sales_select - pred)

pred_salesData2 %>%
  summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )

# Final prediction

x_salesData_pred <- salesData_pred %>%
  select(
    starts_with("item_price_"), -month1, starts_with("l_"), starts_with("total_sales_"),
    -total_sales_select, starts_with("total_rev_sales_"), n_item,
    starts_with("d_"), dur_total_sales, zero_sales, starts_with("d_sales_"), r_month, month,
    starts_with("item_category_id"), -item_category_id, -item_category_id0, -l_sales2, -l_sales3, -item_price_shop_sd,
    -item_price_shop_min, -item_price_shop_mean, -item_price_shop_median, -item_price_mean,
    -item_price_median, -item_price_max, -l_price1, -l_price2, -l_price3,-l_price4, -l_price5
  )

finalPredictions <- salesData_pred %>%
  bind_cols(item_cnt_month = predict(model_xgb, newdata = x_salesData_pred %>% as.matrix(), type = "response")) %>%
  left_join(testData, by = c("shop_id", "item_id")) %>%
  select(ID, item_cnt_month) %>%
  mutate(item_cnt_month = ifelse(item_cnt_month > 20, 20, ifelse(item_cnt_month < 0, 0, item_cnt_month)))

summary(finalPredictions)

write.csv(finalPredictions, file="finalpredictions.csv", row.names = F)
