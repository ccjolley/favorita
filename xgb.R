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

# Not sure I can do this in memory, but here it goes...
train_j <- train %>%
  join_data %>%
  select(-date,-log_sales) %>%
  df2sparse

# Looks like it made it through the joins but blew out on model.matrix
n <- 100
res <- Matrix(nrow=0,ncol=0)
step <- round(nrow(train)/100 + 1)
for (i in 1:n) {
  start <- (i-1)*step + 1
  end <- min(i*step,nrow(train))
  chunk <- train[]
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