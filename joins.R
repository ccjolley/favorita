# This file contains *non-empirical* feature enrichments that are just based
# on joins with other datasets. If I'm deriving new features from the training
# data, I'll need to factor them out into a function so that I can insert them
# into cross validation loops correctly.

library(dplyr)
library(lubridate)

system.time(train <- read.csv('train.csv'))
# Uses 85.3% of memory on an r4.large instance; might be a little tight!
# Only about 11% on r4.xlarge

###############################################################################
# Add date-related features
###############################################################################
all_dates <- data.frame(date=seq(ymd('2013-01-01'),
                                 ymd('2017-08-15'),
                                 by='1 day')) %>%
  mutate(year=as.factor(year(date)),
         month=month(date,label=TRUE),
         wday=wday(date,label=TRUE))

# TODO: How to handle April 16th 2016 earthquake? Need to look at total sales.
# TODO: How quickly do things taper off after paydays? (15th and end of month)

## Add holidays
# TODO: Expand this out to regional and local holidays by interacting with 
# store locations
holidays <- read.csv('holidays_events.csv') %>%
  mutate(date=ymd(date)) %>%
  filter(locale=='National') %>%
  filter(transferred=='False') %>% 
  select(date,type)
all_dates <- left_join(all_dates,holidays,by='date') %>%
  mutate(type=as.character(type),
         type=ifelse(is.na(type),'None',type)) %>%
  group_by(date) %>% # need this to deal with multiple events on same date
  summarize(year=first(year),month=first(month),
            wday=first(wday),type=first(type)) %>%
  mutate(type=as.factor(type))


## Add oil prices
oil <- read.csv('oil.csv') %>%
  mutate(date=ymd(date))
all_dates <- left_join(all_dates,oil,by='date')

all_dates$imputed <- sapply(1:nrow(all_dates), function(i) {
  start <- max(1,i-3)
  end <- min(i+3,nrow(all_dates))
  all_dates[start:end,'dcoilwtico'] %>% colMeans(na.rm=TRUE)
})
all_dates <- all_dates %>% 
  mutate(dcoilwtico=ifelse(is.na(dcoilwtico),imputed,dcoilwtico)) %>%
  select(-imputed)

## Join date features to training data
train$date <- as.character(train$date)
all_dates$date <- as.character(all_dates$date)
system.time(train2 <- left_join(train,all_dates,by='date'))
# takes 11.5s on r4.xlarge
rm(train)

###############################################################################
# Add store-related features
# Ignore explicit geography information for now; I'll want that if I start
# looking at regional holidays.
###############################################################################
stores <- read.csv('stores.csv') %>%
  mutate(cluster=as.factor(cluster)) %>%
  select(store_nbr,type,cluster)
train3 <- left_join(train2,stores,by='store_nbr')
rm(train2)

# I think the best way to handle regional holidays would be to keep the 
# locality identifiers for both stores and holidays, create a new 
# column that flags whether there's a local or regional holiday, then merge
# that into the 'type' column (relpacing some 'None' entries).

###############################################################################
# Add item-related features
#
# TODO: Since item classes are too numerous to make a good factor variable
# (and families might be), I'll need to go back to the training data to find
# an empirical way to group classes together into a smaller number of classes
# or assign them numeric features like their intra-class mean or standard 
# deviation.
###############################################################################
items <- read.csv('items.csv')

# How many classes and families are there?
items$class %>% table # too many for a reasonable factor variable
items$family %>% table  %>% sort(decreasing=TRUE) # better
# TODO: do things work better if I consolidate into a smaller number of families?

items <- items %>%
  select(-class) 

system.time(train4 <- left_join(train3,items,by='item_nbr')) 
rm(train3)

###############################################################################
# Drop columns that won't be useful for training
###############################################################################
train5 <- train4 %>%
  select(-date,-store_nbr,-item_nbr) %>%
  mutate(unit_sales = ifelse(unit_sales < 0,0,unit_sales)) 
# positive unit_sales required because of ln in score function
rm(train4)
