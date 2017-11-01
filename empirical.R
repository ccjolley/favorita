###############################################################################
# This is the code to generate "empirical" features that are derived from
# the training data -- I'll want to be cautious about how I use this in
# cross-validation.
###############################################################################
library(dplyr)

###############################################################################
# Augment training data
###############################################################################
augment <- function(train) {
  by_date <- train %>%
    filter(!test) %>%
    group_by(date) %>%
    summarize(date_mean=mean(log_sales,na.rm=TRUE),
              date_median=median(log_sales,na.rm=TRUE),
              date_sd=sd(log_sales,na.rm=TRUE)) %>%
    mutate(date_sd=ifelse(is.na(date_sd),0,date_sd),
           date=as.character(date))

  by_item <- train %>%
    filter(!test) %>%
    group_by(item_nbr) %>%
    summarize(item_mean=mean(log_sales,na.rm=TRUE),
              item_median=median(log_sales,na.rm=TRUE),
              item_sd=sd(log_sales,na.rm=TRUE)) %>%
    mutate(item_sd=ifelse(is.na(item_sd),0,item_sd))

  by_store <- train %>%
    filter(!test) %>%
    group_by(store_nbr) %>%
    summarize(store_mean=mean(log_sales,na.rm=TRUE),
              store_median=median(log_sales,na.rm=TRUE),
              store_sd=sd(log_sales,na.rm=TRUE)) %>%
    mutate(store_sd=ifelse(is.na(store_sd),0,store_sd))
  
  by_itemstore <- train %>%
    filter(!test) %>%
    group_by(item_nbr,store_nbr) %>%
    summarize(itemstore_mean=mean(log_sales,na.rm=TRUE),
              itemstore_median=median(log_sales,na.rm=TRUE),
              itemstore_sd=sd(log_sales,na.rm=TRUE)) %>%
    mutate(itemstore_sd=ifelse(is.na(itemstore_sd),0,itemstore_sd))
  
  train_mean <- mean(train$log_sales)
  train_median <- median(train$log_sales)
  train_sd <- sd(train$log_sales)  
  
  train %>% 
    mutate(date=as.character(date)) %>%
    left_join(by_date,by='date') %>%
    left_join(by_item,by='item_nbr') %>%
    left_join(by_store,by='store_nbr') %>%
    left_join(by_itemstore,by=c('item_nbr','store_nbr')) %>%
    mutate(date_mean = ifelse(is.na(date_mean),train_mean,date_mean),
           date_median = ifelse(is.na(date_median),train_median,date_median),
           date_sd = ifelse(is.na(date_sd),train_sd,date_sd),
           item_mean = ifelse(is.na(item_mean),train_mean,item_mean),
           item_median = ifelse(is.na(item_median),train_median,item_median),
           item_sd = ifelse(is.na(item_sd),train_sd,item_sd),
           store_mean = ifelse(is.na(store_mean),train_mean,store_mean),
           store_median = ifelse(is.na(store_median),train_median,store_median),
           store_sd = ifelse(is.na(store_sd),train_sd,store_sd),
           itemstore_mean = ifelse(is.na(itemstore_mean),train_mean,itemstore_mean),
           itemstore_median = ifelse(is.na(itemstore_median),train_median,itemstore_median),
           itemstore_sd = ifelse(is.na(itemstore_sd),train_sd,itemstore_sd))
}


