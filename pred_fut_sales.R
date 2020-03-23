library("tidyverse")
library(lubridate)
library(scales)
library("caret")

setwd("C:/Users/Utilizador/Desktop/kaggle/Predict_Future_Sales")
set.seed(2020)


# Load data
salesData <- read.csv('sales_train.csv')
testData <- read.csv('test.csv')
itemsData <- read.csv('items.csv')
itemsCatData <- read.csv('item_categories.csv')
shopsData <- read.csv('shops.csv')

# Overview

glimpse(salesData)
glimpse(itemsData)
glimpse(itemsCatData)
glimpse(shopsData)

summary(salesData$item_cnt_day)

# Preparing Data
# include the item category details in the sales data
salesData <- merge(salesData, itemsData[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
salesData$date <- as.Date(salesData$date, "%d.%m.%Y")

salesData <- salesData %>% 
        mutate(year = as.factor(year(date)))

salesData <- salesData %>% 
        mutate(month = as.factor(month(date)))

salesData <- salesData %>% 
        mutate(day = as.factor(day(date)))

# Sys.setlocale("LC_TIME", "C")
salesData <- salesData %>% 
        mutate(weekdays = as.factor(weekdays(date)))

salesData <- salesData %>% 
        mutate(item_id = as.factor(item_id))

salesData <- salesData %>% 
        mutate(shop_id = as.factor(shop_id))

salesData <- salesData %>% 
        mutate(item_category_id = as.factor(item_category_id))

# EDA

summary(salesData$item_cnt_day)
boxplot(salesData$item_cnt_day)


# sales by shop
sales_by_shop = salesData %>%
        select(shop_id, item_cnt_day) %>%
        group_by(shop_id) %>%
        summarise(item_cnt_day =  sum(item_cnt_day))

ggplot(data =  sales_by_shop, 
       mapping = aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = shop_id)) +
        geom_histogram(stat = "identity", color = "blue") +
        scale_y_continuous(breaks= seq(0, 320000, by=20000), labels = comma) +
        xlab("Shop ID") + ylab("Sales Count")+
        ggtitle(label = "Sales by Shop") 

# sales by item category
sales_by_category = salesData %>%
        select(item_category_id, item_cnt_day) %>%
        group_by(item_category_id) %>%
        summarise(item_cnt_day =  sum(item_cnt_day))

ggplot(data =  sales_by_category, 
       mapping = aes(x = reorder(item_category_id,item_cnt_day), 
                     y = item_cnt_day,
                     fill = item_category_id)) +
        geom_histogram(stat = "identity", color = "blue") +
        scale_y_continuous(breaks= seq(0, 650000, by=20000), labels = comma) +
        xlab("Item Category") + ylab("Sales Count") +
        ggtitle("Sales by Item Category")
        


# items by shop

items_by_shop = salesData %>%
        select(shop_id, item_id) %>%
        group_by(shop_id) %>%
        summarise(item_id = n_distinct(item_id))

ggplot(data = items_by_shop,
       mapping = aes(x = reorder(shop_id,item_id),
                     y = item_id,
                     fill = shop_id))+
        geom_histogram(stat = "identity", color = "blue") +
        scale_y_continuous(breaks= seq(0, 16000, by=500), labels = comma) +
        xlab(" Shop ID")+ ylab("Items ID")+
        ggtitle(" Items by Shop") 
        

# Tne most available items by category

items_by_category = salesData %>%
        select(item_category_id, item_id) %>%
        group_by(item_category_id) %>%
        summarise(item_id =  n_distinct(item_id))

ggplot(data = items_by_category,
       mapping = aes(x = reorder(item_category_id,item_id),
                     y = item_id,
                     fill = item_category_id))+
        geom_histogram(stat = "identity", color = "blue") +
        xlab(" Category ID")+ ylab(" Items in Category")+
        scale_y_continuous(breaks= seq(0, 16000, by=500), labels = comma) +
        coord_flip() +
        ggtitle("Most Items by Category") 


# The most most sold item in the each shop 

most_sold_items_in_shop  =  salesData %>%
        group_by(shop_id, item_id) %>%
        summarise(most_sold_item = sum(item_cnt_day)) %>%
        filter(most_sold_item == max(most_sold_item)) %>%
        arrange(desc(most_sold_item))

ggplot(data = most_sold_items_in_shop,
       mapping = aes(x = reorder(shop_id, most_sold_item),
                     y = most_sold_item,
                     fill = item_id)) +
        geom_histogram(stat = "identity", color = "blue") +
        xlab("Shop ID") + ylab("Sales Count") +
        ggtitle("Most Sold Items in each Shop ") 
        
        
# Item category is highest sales grossing in all shops

sales_grossing_category = salesData %>%
        group_by(item_category_id) %>%
        summarise(gross_category = sum(item_cnt_day * item_price)) %>%
        arrange(desc(gross_category))

ggplot(sales_grossing_category, 
       aes(x = reorder(item_category_id, gross_category),
           y = gross_category,
           fill = item_category_id)) +
        geom_histogram(stat = "identity", color = "blue") +
        scale_y_continuous(breaks= seq(0, 415000000, by=50000000), labels = comma) +
        xlab("Category ID") + ylab("Gross Category")+
        coord_flip()+
        ggtitle("Sales Grossing by Item category") 
        
# item categories by shop 
item_category_in_shops = salesData %>%
        group_by(shop_id) %>%
        summarise(item_category =  paste(sort(unique(item_category_id)), collapse = ", ")) 

head(item_category_in_shops)

# The most sold item in eack category

most_sold_item_per_category = salesData %>%
        group_by(item_category_id, item_id) %>%
        summarise(totalSales = sum(item_price * item_cnt_day)) %>%
        filter(totalSales == max(totalSales)) %>%
        arrange(desc(totalSales))


ggplot(most_sold_item_per_category,
       aes(x = reorder(item_category_id, totalSales), 
           y = totalSales,
           fill = item_id)) +
        geom_histogram(stat = "identity", color = "blue") +
        scale_y_continuous(breaks= seq(0, 220000000, by=50000000), labels = comma) +
        labs(title = "Items sold per category",x = "Category ID", y = "Sales", fill = "Item ID") +
        coord_flip() 


# day and month vs total sales 
month_day_total_sales =  salesData %>%
        group_by(month, day) %>%
        summarise(totalSales =  sum(item_price * item_cnt_day))

ggplot(month_day_total_sales, 
       aes(x = day, 
           y = totalSales, 
           group =  month, 
           color =  factor(month))) +
        geom_line() + 
        geom_point() +
        scale_y_continuous(breaks= seq(0, 45000000, by=9000000), labels = comma) +
        labs(title = "Total Sales month-day", x = "Days", y = "Total sales", fill = "Months")


ggplot(month_day_total_sales, 
       aes(x = day, 
           y = totalSales, 
           fill =  factor(day))) +
        geom_histogram(stat = "identity", color = "blue") +
        scale_y_continuous(breaks= seq(0, 45000000, by=9000000), labels = comma) +
        labs(title = "Total Sales month-day", x = "Days", y = "Total sales", fill = "Days") +
        facet_wrap(~month, ncol = 2)


# total sales by year

yearSales <- salesData %>%
        group_by(year) %>%
        summarise(yearSale = sum(item_price * item_cnt_day))

ggplot(yearSales, aes(x =  year, y = yearSale, fill =  year))+
        geom_histogram(stat = "identity", color = "blue")+
        scale_y_continuous(breaks= seq(0, 1400000000, by=200000000), labels = comma) +
        labs(title = "Sales by Year", x = "Year", y = "Total Sale", fill = "Year")+
        geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = yearSale)) 


# Total sales by year and month 
ymSales = salesData %>%
        group_by(year, month) %>%
        summarise(ymSale = sum(item_price*item_cnt_day)) %>%
        arrange(year)

ymSales$ymSale = round(ymSales$ymSale, 2)
ggplot(ymSales, aes(x =  month, y = ymSale, fill =  year))+
        geom_histogram(stat = "identity", position = "dodge", color = "blue") +
        labs(title = "Year / Month sales", x = "Month", y =  "Total sales", fill = "Year")
        
# percent of items sold each month


# number of items sold by day 
dailySale = salesData %>%
        group_by(date) %>%
        summarise(itemSold =  sum(item_cnt_day))

ggplot(dailySale, aes(x =  date, y = itemSold, color =  itemSold)) +
        geom_line() +
        geom_point()+
        scale_y_continuous(breaks= seq(0, 15000, by=1000), labels = comma) +
        labs(title = "Item sold by day", x =  "Date", y = "Items sold")  

# number of items sold on weekdays 

weekdays_item_Sale = salesData %>%
        group_by(weekdays) %>%
        summarise(itemSold = sum(item_cnt_day)) %>%
        arrange(desc(itemSold))

ggplot(weekdays_item_Sale, aes(x =reorder(weekdays, itemSold), y =  itemSold, fill = weekdays))+
        geom_bar(stat = "identity", color = "blue") +
        scale_y_continuous(breaks= seq(0, 750000, by=100000), labels = comma) +
        labs(title = "Items sold on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
        geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = itemSold)) 



# sales on weekdays

weekdaysSales = salesData %>%
        group_by(weekdays) %>%
        summarise(totalSale = sum(item_cnt_day * item_price)) %>%
        arrange(desc(totalSale))
weekdaysSales$totalSale = round(weekdaysSales$totalSale, 2)

ggplot(weekdaysSales, aes(x =reorder(weekdays, totalSale), y =  totalSale, fill = weekdays))+
        geom_bar(stat = "identity", color ="blue") +
        scale_y_continuous(breaks= seq(0, 650000000, by=50000000), labels = comma) +
        labs(title = "Sales on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
        geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = totalSale)) 
        