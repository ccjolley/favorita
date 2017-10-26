library(dplyr)
library(ff)
library(ffbase2)
library(randomForest)

setwd("C:/Users/Craig/Desktop/Live projects/kaggle/favorita")
source('joins.R')

fold <- 10

# this approach sets me up for cross-validation and doesn't require storing
# a big vector of random numbers
ti <- Sys.time()
test_i <- train4 %>% filter(id %% fold == 0)
train_i <- train4 %>% filter(id %% fold != 0)  
Sys.time() - ti

# What I want to do is train a model on small chunks, then average
# results obtained from evaluation on several of these mini-models,
# so that I'm still using all the data.

# Before going down that road, I need to know how training of a knn.reg
# scales with the number of instances I'm looking for. 
bench <- function(chunk_size,result) {
  nchunks <- round(nrow(train4) / chunk_size)
  ti <- Sys.time()
  train_chunk <- train4 %>% filter(id %% nchunks == 0) %>% data.frame
  chunk_t <- Sys.time() - ti
  train_chunk <- train_chunk %>%
    mutate(onpromotion=as.character(onpromotion),
           onpromotion=ifelse(is.na(onpromotion),'no_info',onpromotion),
           onpromotion=as.factor(onpromotion)) %>%
    select(-id,-date,-store_nbr,-item_nbr)
  if (result == 'time') {
    ti <- Sys.time()
    my_rf <- randomForest(unit_sales ~ .,data=train_chunk)
    train_t <- Sys.time() - ti
    paste0(chunk_size,' ',chunk_t,' ',train_t) %>% print
    return(chunk_t+train_t)
  }
  if (result == 'rmse') {
    n <- nrow(train_chunk)
    holdout <- c(rep(TRUE,0.1*n),rep(FALSE,0.9*n)) %>% sample
    my_train <- train_chunk[!holdout,]
    my_test <- train_chunk[holdout,]
    my_rf <- randomForest(unit_sales ~ .,data=my_train)
    (predict(my_rf,my_test) - my_test$unit_sales)^2 %>% mean %>% sqrt %>% return
  }
}

bench(1000)
a <- c(1000,2000,5000,1e4,2e4,5e4,1e5,2e5,5e5,1e6)
times <- sapply(a,function(i) bench(i,result='time'))

# Based on a trial run, the time for RF training seems to scale with chunk size 
# as N^2, but the number of chunks needed scales as 1/N -- this means that 
# it's faster to train many small models than one big one. What I don't know
# is where I run into diminishing returns in terms of training model size and 
# number of little models. 
# According to a line fit in Excel, RF training took about 2e-6*N^2 seconds

# First, how does accuracy improve with more training data?

bench(100,result='rmse') # 10.2208
a <- (100*1.3^(1:20)) %>% round
rmse <- sapply(a,function(i) bunch(i,result='rmse'))
