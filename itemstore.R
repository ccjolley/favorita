library(dplyr)

# Simple solution: For each item, future prediction is the mean for that
# item and store. 
# If an item_store combo is not available, take the mean for that item.
# If an item is not available, take the mean for similar items.

train <- read.csv('train.csv') %>%
  mutate(promo_na=is.na(onpromotion),
         onpromotion=(!is.na(onpromotion) & onpromotion==TRUE),
         log_sales = ifelse(unit_sales >= 0,log(unit_sales+1),0))

by_itemstore <- train %>%
  group_by(item_nbr,store_nbr) %>%
  summarize(itemstore_mean=mean(log_sales,na.rm=TRUE))

by_item <- train %>%
  group_by(item_nbr) %>%
  summarize(item_mean=mean(log_sales,na.rm=TRUE))

items <- read.csv('items.csv') %>% 
  left_join(by_item,by='item_nbr')

by_class <- items %>%
  group_by(class) %>%
  summarize(class_mean=mean(item_mean,na.rm=TRUE))

by_family <- items %>%
  group_by(family) %>%
  summarize(family_mean=mean(item_mean,na.rm=TRUE))

items <- items %>%
  left_join(by_class,by='class') %>%
  left_join(by_family,by='family')

test <- read.csv('test.csv')

j <- test %>%
  left_join(by_itemstore,by=c('store_nbr','item_nbr')) %>%
  left_join(items,by='item_nbr') %>% 
  mutate(best_guess = ifelse(!is.na(itemstore_mean),itemstore_mean,
                             ifelse(!is.na(item_mean),item_mean,
                                    ifelse(!is.na(class_mean),class_mean,
                                           family_mean)))) %>%
  mutate(unit_sales = exp(best_guess)-1)
  
j %>% 
  select(id,unit_sales) %>%
  write.csv('submit/sub_itemstore_mean.csv',row.names=FALSE,quote=FALSE)


