# simplest thing I can think of -- linear model

library(dplyr)
library(magrittr)

source('empirical.R')

# Empirical data only, 10x cross-validation

train <- read.csv('train.csv') 

items <- read.csv('items.csv') # need perishables for correct scoring
train <- left_join(train,items %>% select(item_nbr,perishable),by='item_nbr')
train <- train %>%
  mutate(promo_na=is.na(onpromotion),
         onpromotion=(!is.na(onpromotion) & onpromotion==TRUE),
         log_sales = ifelse(unit_sales >= 0,log(unit_sales+1),0))

lm_xval <- function(train,fold) {
  sapply(1:fold,function(i) {
    train$test <- (train$id %% fold == i-1)
    train_i <- augment(train)
    test_i <- train_i %>% filter(test==TRUE) %>% 
      select(-id,-date,-store_nbr,-unit_sales,-item_nbr,-test)
    train_i <- train_i %>% filter(test==FALSE) %>% 
      select(-id,-date,-store_nbr,-unit_sales,-item_nbr,-test)
    my_lm <- lm(log_sales ~ .,data=train_i)
    w <- ifelse(test_i$perishable==1,1.25,1)
    score <- sqrt(sum(w*((predict(my_lm,test_i) - test_i$log_sales)^2)) / sum(w))
    paste0(i,' ',score) %>% print
    score
  }) %>% mean
}

n <- 1e5
short <- train %>% 
  filter(id %% round(nrow(train)/n) == 0)
short$id <- 1:nrow(short)

lm_xval(short,10)
# average of 0.8746726 when I run on my laptop with a subset of 10k rows
# when I run without the itemstore columns, drops to 0.824913
  
# With the itemstore columns, I get the following on an AWS r4.2xlarge
# 1e5 rows: 0.8020752
  
