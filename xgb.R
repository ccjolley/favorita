library(dplyr)
library(xgboost)
library(Matrix)
library(data.table)
library(lubridate)
library(ggplot2)

train <- fread('train.csv',header=TRUE,sep=',') # 33% of memory on r4.2xlarge
train <- train %>% 
  mutate(log_sales=ifelse(unit_sales > 0,log(unit_sales+1),0),
         date=ymd(date)) %>%
  select(-unit_sales)
source('joins.R')

n <- 25
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
rm(n,step,i,start,end,chunk)

# Save memory by keeping only the columns I still need in train
train <- train %>% select(date,log_sales)

###############################################################################
# Small datasets for faster training
# My real test dataset has 3370464 rows; if I go with a 70/30 split then I'll
# want ~11M rows, with ~8M in the training set
###############################################################################
n <- 3370464
end_test <- xgb.DMatrix(data = tail(train_j,n),
                        label = train$log_sales %>% tail(n))
end_train <- xgb.DMatrix(data= tail(train_j,n*10/3) %>% head(n*7/3),
                         label = train$log_sales %>% tail(n*10/3) %>% head(n*7/3))
xgb.DMatrix.save(end_train,'end-train.data')
xgb.DMatrix.save(end_test,'end-test.data')

# Start here!
end_train <- xgb.DMatrix('end-train.data')
end_test <- xgb.DMatrix('end-test.data')

watchlist <- list(train=end_train, test=end_test)
xgb1 <- xgb.train(params=list(max_depth=7),
                  data = end_train, 
                  watchlist=watchlist,
                  objective = "reg:linear",
                  nthread=16, # for AWS r4.2xlarge
                  nrounds=100)

min(xgb1$evaluation_log$test_rmse)

# Default parameters: min test-rmse = 0.790962
# max_depth=7, 0.789887
# max_depth=8,0.790657
# max_depth=10, 0.791362


ggplot(xgb1$evaluation_log,aes(x=iter,y=test_rmse)) +
  geom_point(size=2,color='cornflowerblue') +
  geom_line(size=1,color='cornflowerblue') +
  theme_classic()

# Let's try submitting with this model and see if the leaderboard result 
# looks reasonable...

test <- fread('test.csv',header=T,sep=',')
test_sp <- test %>%
  join_data %>%
  select(-date) %>%
  mutate(onpromotion=factor(onpromotion,levels=c('TRUE','FALSE','NA')),
         year=factor(year,levels=c('2013','2014','2015','2016','2017')),
         month=factor(month,levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul',
                                     'Aug','Sep','Oct','Nov','Dec'))) %>% 
  df2sparse

pred <- predict(xgb1, test_sp)
output <- data.frame(id=test$id,unit_sales=exp(pred)-1)
write.csv(output,'submit/xgb2.csv',row.names=FALSE,quote=FALSE)

output <- output %>% mutate(unit_sales=round(unit_sales))
write.csv(output,'submit/xgb3.csv',row.names=FALSE,quote=FALSE)

###############################################################################
# Test run of training to find optimal # of iterations for default xgboost 
# parameters.
# Simulate the real prediction situation by splitting out the last two weeks
###############################################################################
cutoff <- ymd('2017-07-31')
dtrain <- xgb.DMatrix(data = train_j[train$date <= cutoff,],
                      label=train[train$date <= cutoff,'log_sales'])
dtest <- xgb.DMatrix(data = train_j[train$date > cutoff,],
                     label=train[train$date > cutoff,'log_sales'])
xgb.DMatrix.save(dtrain,'bench-train.data')
xgb.DMatrix.save(dtest,'bench-test.data')

# Start here!
dtrain <- xgb.DMatrix('bench-train.data')
dtest <- xgb.DMatrix('bench-test.data')

watchlist <- list(train=dtrain, test=dtest)
xgb1 <- xgb.train(data = dtrain, 
                  watchlist=watchlist,
                  objective = "reg:linear",
                  nthread=8, # for AWS r4.2xlarge
                  nrounds=4)

# This isn't a memory hog, especially when I load dtrain and dtest in a clean R 
# session using xgb.DMatrix(). Memory usage seems pegged at about 35.2% on an
# r4.2xlarge instance, so this is manageable.

# It seems like it hangs if I try to save source code changes while it's 
# running, oddly.

# test-rmse stopped decreasing after 182 rounds; stop here for the real
# training.

###############################################################################
# Production training, generate output
###############################################################################
dprod <- xgb.DMatrix(data = train_j,
                     label=train$log_sales)
xgb.DMatrix.save(dprod,'prod-train.data')

# Start here!
dprod <- xgb.DMatrix('prod-train.data')

watchlist=list(train=dprod)
xgb_prod <- xgb.train(data = dprod, 
                      watchlist=watchlist,
                      verbose = 1,
                      objective = "reg:linear",
                      nthread=8, # for AWS r4.4xlarge
                      nrounds=182)

# TODO: Can I do a custom objective function?

test <- fread('test.csv',header=T,sep=',')
test_sp <- test %>%
  join_data %>%
  select(-date) %>%
  mutate(onpromotion=factor(onpromotion,levels=c('TRUE','FALSE','NA')),
         year=factor(year,levels=c('2013','2014','2015','2016','2017')),
         month=factor(month,levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul',
                                     'Aug','Sep','Oct','Nov','Dec'))) %>% 
  df2sparse

pred <- predict(xgb_prod, test_sp)
output <- data.frame(id=test$id,unit_sales=exp(pred)-1)
write.csv(output,'submit/xgb1.csv',row.names=FALSE,quote=FALSE)

## Sanity checks
xgb_prod$evaluation_log$train_rmse %>% tail
# Leaderboard result was 1.217 -- did I just overtrain brutally?

importance_matrix <- xgb.importance(model = xgb_prod)
print(importance_matrix) %>% head
xgb.plot.importance(importance_matrix = importance_matrix)
test_sp@Dimnames[[2]][33-1]

