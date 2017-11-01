library(dplyr)
library(magrittr)
library(randomForest)

setwd("C:/Users/Craig/Desktop/Live projects/kaggle/favorita")
source('joins.R')

fold <- 1000

# this approach sets me up for cross-validation and doesn't require storing
# a big vector of random numbers
test_i <- train5 %>% filter(id %% fold == 0)
train_i <- train5 %>% filter(id %% fold != 0)  

# What I want to do is train a model on small chunks, then average
# results obtained from evaluation on several of these mini-models,
# so that I'm still using all the data.

# Before going down that road, I need to know how training of a knn.reg
# scales with the number of instances I'm looking for. 

# Test on the same holdout data (10% of total) regardless of size of the 
# training data used, so that comparisons are fair.

w <- ifelse(test_i$onpromotion=='True',1.25,1.0)
bench <- function(chunk_size) {
  nchunks <- round(nrow(train5) / chunk_size)
  train_chunk <- train5 %>% filter(id %% nchunks == 0) %>%
    select(-id)
  ti <- Sys.time()
  my_rf <- randomForest(unit_sales ~ .,data=train_chunk)
  train_t <- Sys.time() - ti
  ti <- Sys.time()
  score <- (w*(log(predict(my_rf,test_i)+1) - log(test_i$unit_sales+1))^2) %>% 
    mean %>% sqrt
  score_t <- Sys.time() - ti
  paste0(chunk_size,' ',score,' ',train_t,' ',score_t) %>% print
  return(score)
}

system.time(bench(100))
scores <- sapply(a,bench)

# Scoring time seems to depend fairly weakly on chunk size N, as ~log(N)
# Training time scales as N^2, so that training on 1e5 will take ~3.3h,
# on 1e6 will take ~268h, and the full dataset will take 7,000 years.

# Also, no appreciable decrease in score over the numbers I looked at; 
# it seems to hover around 1.0.

(w*(log(mean(test_i$unit_sales)+1) - log(test_i$unit_sales+1))^2) %>% 
  mean %>% sqrt

<<<<<<< HEAD
# Based on a trial run, the time for RF training seems to scale with chunk size 
# as N^2, but the number of chunks needed scales as 1/N -- this means that 
# it's faster to train many small models than one big one. What I don't know
# is where I run into diminishing returns in terms of training model size and 
# number of little models. 

# According to a line fit in Excel, RF training took about 2e-6*N^2 seconds
=======
# randomForest on small samples just barely beats this. Lame-o.
>>>>>>> 8f77241d6901db955ee11d5adb341db8537550ee

(w*(log(mean(test_i$unit_sales)+1) - log(test_i$unit_sales+1))^2) %>% 
  median %>% sqrt

# Using the median actually beats random forest.
