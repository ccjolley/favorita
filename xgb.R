library(dplyr)
library(xgboost)
library(Matrix)

train <- read.csv('train_subset.csv')
source('joins.R')

train4 <- train4 %>%
  mutate(log_sales=ifelse(unit_sales > 0,log(unit_sales+1),0)) %>%
  select(-unit_sales)

###############################################################################
# Convert a data frame with factor variables into a sparse matrix suitable
# for xgboost.
# TODO: Probably move this to a general utility section eventually, but I'll 
# want a more general way to create NA factors.
###############################################################################
df2sparse <- function(d) {
  d <- d %>%
    mutate(onpromotion=ifelse(is.na(onpromotion),'NA',onpromotion),
           onpromotion=as.factor(onpromotion))
  model.matrix(~ .+0, data=d, 
               contrasts.arg = lapply(d[,sapply(d, is.factor)], 
                                      contrasts, contrasts=FALSE)) %>%
    Matrix(sparse=TRUE)
}

###############################################################################
# Now for the training!
# simulate the real prediction situation by splitting out the last two weeks
###############################################################################
train4$date <- ymd(train4$date)
my_test <- train4[train4$date > ymd('2017-07-31'),] %>% 
  select(-id,-date,-store_nbr,-item_nbr,-log_sales) %>% 
  df2sparse
my_test_label <- train4[train4$date > ymd('2017-07-31'),'log_sales']
my_train <- train4[train4$date <= ymd('2017-07-31'),] %>% 
  select(-id,-date,-store_nbr,-item_nbr,-log_sales) %>% 
  df2sparse
my_train_label <- train4[train4$date <= ymd('2017-07-31'),'log_sales']

dtrain <- xgb.DMatrix(data = my_train, label = my_train_label)
dtest <- xgb.DMatrix(data = my_test, label = my_test_label)

watchlist <- list(train=dtrain, test=dtest)
xgb1 <- xgb.train(data = dtrain, 
                watchlist=watchlist,
                objective = "reg:linear",
                nrounds=20)
# Seems to be overfitting after about 10 rounds