# simplest thing I can think of -- linear model

library(dplyr)
library(magrittr)

# Empirical data only

###############################################################################
# Load training data, enrich with empirical averages
###############################################################################
train <- read.csv('train.csv') 

train <- train %>%
  mutate(promo_na=is.na(onpromotion),
         onpromotion=(!is.na(onpromotion) & onpromotion==TRUE),
         log_sales = ifelse(unit_sales >= 0,log(unit_sales+1),0))

by_item <- train %>%
  group_by(item_nbr) %>%
  summarize(item_mean=mean(log_sales,na.rm=TRUE))

by_store <- train %>%
  group_by(store_nbr) %>%
  summarize(store_mean=mean(log_sales,na.rm=TRUE))

train_mean <- mean(train$log_sales)
train_median <- median(train$log_sales)
train_sd <- sd(train$log_sales)  

j <- train %>% 
  mutate(date=as.character(date)) %>%
  left_join(by_item,by='item_nbr') %>%
  left_join(by_store,by='store_nbr') %>%
  select(-id,-date,-item_nbr,-store_nbr,-unit_sales)

my_lm <- lm(log_sales ~ .,data=j)
summary(my_lm)

# This is actually sort of lame -- not much there.

###############################################################################
# Load test data, join averages from training data
# Because test data contains some new items, may need to guess the appropriate
# item_mean based on class and family.
###############################################################################

test <- read.csv('test.csv')

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

j <- test %>%
  left_join(items,by='item_nbr') %>% 
  mutate(item_mean = ifelse(!is.na(item_mean),item_mean,
                                    ifelse(!is.na(class_mean),class_mean,
                                           family_mean)))

# CHECK: what's the relationship between dates in the train and test datasets?
# what about stores?
