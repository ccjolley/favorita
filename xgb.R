library(dplyr)
library(xgboost)
library(Matrix)
library(data.table)
library(lubridate)

system.time(train <- fread('train.csv',header=TRUE,sep=',')) # 33% of memory on r4.2xlarge
train <- train %>% 
  mutate(log_sales=ifelse(unit_sales > 0,log(unit_sales+1),0),
         date=ymd(date)) %>%
  select(-unit_sales)
source('joins.R')

# Haven't succeeded at doing this in memory in one shot; try it this way
n <- 10
step <- round(nrow(train)/n + 1)
train_j <- Matrix(nrow=0,ncol=0)
for (i in 1:n) {
  start <- (i-1)*step + 1
  end <- min(i*step,nrow(train))
  paste0('chunk ',i,', rows ',start,'-',end) %>% print
  chunk <- train[start:end,] %>%
    join_data %>%
    select(-date,-log_sales) %>%
    mutate(onpromotion=factor(onpromotion,levels=c('TRUE','FALSE','NA'))) %>% 
    df2sparse
  if (ncol(train_j) == 0) {
    train_j <- Matrix(nrow=0,ncol=ncol(chunk),sparse=TRUE)
  }
  train_j <- rbind(train_j,chunk)
}

# Save memory by keeping only the columns I still need in train
train <- train %>% select(date,log_sales)

###############################################################################
# Now for the training!
# simulate the real prediction situation by splitting out the last two weeks
###############################################################################
cutoff <- ymd('2017-07-31')
dtrain <- xgb.DMatrix(data = train_j[train$date <= cutoff,],
                      label=train[train$date <= cutoff,'log_sales'])
dtest <- xgb.DMatrix(data = train_j[train$date > cutoff,],
                     label=train[train$date > cutoff,'log_sales'])

watchlist <- list(train=dtrain, test=dtest)
xgb1 <- xgb.train(data = dtrain, 
                  watchlist=watchlist,
                  objective = "reg:linear",
                  nthread=8, # for AWS r4.2xlarge
                  nrounds=200)
# Seems to be overfitting after about 10 rounds