# This file contains *non-empirical* feature enrichments that are just based
# on joins with other datasets. If I'm deriving new features from the training
# data, I'll need to factor them out into a function so that I can insert them
# into cross validation loops correctly.

library(dplyr)
library(lubridate)

###############################################################################
# Load auxiliary datasets
###############################################################################

### Date features
all_dates <- data.frame(date=seq(ymd('2013-01-01'),
                                 ymd('2017-08-31'),
                                 by='1 day')) %>%
  mutate(year=as.factor(lubridate::year(date)),
         month=lubridate::month(date,label=TRUE),
         wday=lubridate::wday(date,label=TRUE))


## Holidays; join to date features
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

# TODO: How to handle April 16th 2016 earthquake? Need to look at total sales.
# TODO: How quickly do things taper off after paydays? (15th and end of month)
# TODO: Expand this out to regional and local holidays by interacting with 
# store locations

## Oil prices; join to date features
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

all_dates$date <- as.character(all_dates$date) # makes joins easier

## Store features
stores <- read.csv('stores.csv') %>%
  mutate(cluster=as.factor(cluster)) %>%
  select(store_nbr,type,cluster)
# TODO: ignoring explicit geography for now; will need that if I want
# regional holidays
# I think the best way to handle regional holidays would be to keep the 
# locality identifiers for both stores and holidays, create a new 
# column that flags whether there's a local or regional holiday, then merge
# that into the 'type' column (relpacing some 'None' entries).

## Item features
items <- read.csv('items.csv') %>%
  select(-class) # too many for a reasonable factor variable
# TODO: Maybe make a factor variable out of the most heavily-populated classes

###############################################################################
# Join basic features to these and drop the variables that won't be useful for
# training.
# Because I'm not actually calculating anything based on the dataset being 
# processed, there's no reason not to break a dataset into small chunks for 
# this process to save memory.
###############################################################################
join_data <- function(d) {
  d$date <- as.character(d$date)
  d %>%
    left_join(all_dates,by='date') %>%
    left_join(stores,by='store_nbr') %>%
    left_join(items,by='item_nbr') %>%
    select(-id,-store_nbr,-item_nbr)
}

###############################################################################
# Convert a dataframe with lots of factor variables into a sparse matrix that
# can be used for xgBoost, PCA, etc.
###############################################################################
df2sparse <- function(d) {
  has_na <- names(d)[is.na(d) %>% colSums > 0]
  for (n in has_na) {
    d[,n] <- ifelse(is.na(d[,n]),'NA',as.character(d[,n])) %>% 
      factor(levels=levels(d[,n]))
  }
  model.matrix(~ .+0, data=d, 
               contrasts.arg = lapply(d[,sapply(d, is.factor)], 
                                      contrasts, contrasts=FALSE)) %>%
    Matrix(sparse=TRUE)
}


