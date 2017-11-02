library(dplyr)
library(xgboost)
library(Matrix)

train <- read.csv('train_subset.csv')
source('joins.R')

train_j <- train %>%
  mutate(log_sales=ifelse(unit_sales > 0,log(unit_sales+1),0)) %>%
  select(-unit_sales) %>%
  join_data %>%
  mutate(date=ymd(date))

###############################################################################
# Now for the training!
# simulate the real prediction situation by splitting out the last two weeks
###############################################################################
cutoff <- ymd('2017-07-31')
dtrain <- xgb.DMatrix(data = train_j %>%
                        filter(date <= cutoff) %>% 
                        select(-date,-log_sales) %>% 
                        df2sparse,
                      label=train_j[train_j$date <= cutoff,'log_sales'])
dtest <- xgb.DMatrix(data = train_j %>%
                        filter(date > cutoff) %>% 
                        select(-date,-log_sales) %>% 
                        df2sparse,
                      label=train_j[train_j$date > cutoff,'log_sales'])

watchlist <- list(train=dtrain, test=dtest)
xgb1 <- xgb.train(data = dtrain, 
                watchlist=watchlist,
                objective = "reg:linear",
                nrounds=20)
# Seems to be overfitting after about 10 rounds