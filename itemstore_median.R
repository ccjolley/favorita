library(dplyr)

# Simple solution: For each item, future prediction is the median for that
# item and store. 
# If an item_store combo is not available, take the median for that item.
# If an item is not available, take the median for similar items.

train <- read.csv('train.csv') %>%
  mutate(promo_na=is.na(onpromotion),
         onpromotion=(!is.na(onpromotion) & onpromotion==TRUE),
         log_sales = ifelse(unit_sales >= 0,log(unit_sales+1),0))

by_itemstore <- train %>%
  group_by(item_nbr,store_nbr) %>%
  summarize(itemstore_median=median(log_sales,na.rm=TRUE))

by_item <- train %>%
  group_by(item_nbr) %>%
  summarize(item_median=median(log_sales,na.rm=TRUE))

items <- read.csv('items.csv') %>% 
  left_join(by_item,by='item_nbr')

by_class <- items %>%
  group_by(class) %>%
  summarize(class_median=median(item_median,na.rm=TRUE))

by_family <- items %>%
  group_by(family) %>%
  summarize(family_median=median(item_median,na.rm=TRUE))

items <- items %>%
  left_join(by_class,by='class') %>%
  left_join(by_family,by='family')

test <- read.csv('test.csv')

j <- test %>%
  left_join(by_itemstore,by=c('store_nbr','item_nbr')) %>%
  left_join(items,by='item_nbr') %>% 
  mutate(best_guess = ifelse(!is.na(itemstore_median),itemstore_median,
                             ifelse(!is.na(item_median),item_median,
                                    ifelse(!is.na(class_median),class_median,
                                           family_median)))) %>%
  mutate(unit_sales = exp(best_guess)-1)

j %>% 
  select(id,unit_sales) %>%
  write.csv('submit/sub_itemstore_median.csv',row.names=FALSE,quote=FALSE)