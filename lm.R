# simplest thing I can think of -- linear model

library(magrittr)
library(dplyr)

source('empirical.R')

# Empirical data only, 10x cross-validation

train <- read.csv('train_subset.csv') 
items <- read.csv('items.csv')
# need perishables for correct scoring
train <- left_join(train,items %>% select(item_nbr,perishable),by='item_nbr')
train <- train %>%
  mutate(promo_na=is.na(onpromotion),
         onpromotion=(!is.na(onpromotion) & onpromotion==TRUE))

lm_xval <- function(train,fold) {
  partition <- rep(1:fold,round(nrow(train)/fold)+1) %>% 
    head(nrow(train)) %>% sample
  sapply(1:fold,function(i) {
    train_i <- train[partition != i,] 
    test_i <- train[partition == i,] 
    both <- augment(train_i,test_i)
    train_i <- both %>% filter(test==0) %>% 
      select(-id,-date,-store_nbr,-unit_sales,-item_nbr,-test)
    test_i <- both %>% filter(test==1) %>% 
      select(-id,-date,-store_nbr,-unit_sales,-item_nbr,-test)
    my_lm <- lm(log_sales ~ .,data=train_i)
    w <- ifelse(test_i$perishable==1,1.25,1)
    score <- sqrt(sum(w*((predict(my_lm,test_i) - test_i$log_sales)^2)) / sum(w))
    paste0(i,' ',score) %>% print
    score
  }) %>% mean
}

lm_xval(train,10)
# average of 0.8746726 when I run on my laptop with a subset of 10k rows
# when I run without the itemstore columns, drops to 0.824913
  
  
