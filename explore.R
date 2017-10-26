# Feature engineering for favorita dataset

library(dplyr)
library(ggplot2)
library(ff)
library(ffbase2)
library(lubridate)

setwd("C:/Users/Craig/Desktop/Live projects/kaggle/favorita")

###############################################################################
# Load training data using ff to get around memory limitations
# Training dataset is 4.7GB and my laptop only has 8GB of RAM
###############################################################################
train <- read.csv.ffdf(file='train.csv',VERBOSE=TRUE)
# 125.5M rows; took about 10 minutes to load
ffsave(train,file='train.ff') # for faster loading next time
ffload(file='train.ff') # start here

###############################################################################
# Sample time series for one store
###############################################################################
all_dates <- data.frame(date=seq(ymd('2013-01-01'),ymd('2017-08-15'),by='1 day'))

store_25 <- train %>%
  filter(store_nbr==25) %>%
  group_by(date) %>%
  summarize(total_sales=sum(unit_sales)) %>% 
  as.data.frame %>%
  mutate(date = ymd(date)) %>%
  full_join(all_dates,by='date') %>% # pad with zeros for missing dates
  mutate(total_sales=ifelse(is.na(total_sales),0,total_sales))

ggplot(store_25,aes(x=date,y=total_sales)) +
  geom_line(color='cornflowerblue') +
  ggtitle('Total transactions for store #25') +
  theme_classic() +
  theme(axis.title=element_blank())

###############################################################################
# Compare time series for stores
# TODO: Something isn't right here -- revisit this.
###############################################################################
stores <- read.csv('stores.csv') # 54 stores
store_row <- function(snum) {
  print(snum)
  tmp <- train %>% 
    filter(store_nbr==snum) %>%
    as.data.frame %>%
    group_by(date) %>%
    summarize(total_sales=sum(unit_sales)) %>% 
    mutate(date = ymd(date)) %>%
    full_join(all_dates,by='date') %>% # pad with zeros for missing dates
    mutate(total_sales=ifelse(is.na(total_sales),0,total_sales))
  t(tmp$total_sales)
}

system.time(store_row(2)) # ~10s; 54 stores = <10 min

#store_totals <- plyr::ldply(c(1,2),store_row)
store_totals <- plyr::ldply(stores$store_nbr,store_row)
write.csv(store_totals,'store_totals.csv',row.names=FALSE) # for later

# Need to drop any constant columns before I can rescale for PCA
apply(store_totals,2,sd) %>% sort %>% head(20)
# Why are 1680-1688 constant? All stores had the same sales on those dates?
all_dates[1680:1688,]
last_day <- train %>%
  filter(date=='2017-08-15') %>%
  as.data.frame
last_day[last_day$store_nbr==1,'unit_sales'] %>% sum
# definitely not zero

store_pca <- prcomp(store_totals,center=TRUE,scale=TRUE)


plot(store_pca) 
summary(store_pca) # 87% in first PC
stores$PC1 <- score_pca$x[,1]
stores$PC2 <- score_pca$x[,2]

# Try coloring points according to some of the categories they give us to
# see whether PCA separates them.
ggplot(stores,aes(x=PC1,y=PC2)) +
  geom_point(size=2) +
  theme_classic()

###############################################################################
# Sample time series for one item
###############################################################################
item_103665 <- train %>%
  filter(item_nbr==103665) %>%
  group_by(date) %>%
  summarize(total_sales=sum(unit_sales)) %>% 
  as.data.frame %>%
  mutate(date = ymd(date)) 

ggplot(item_103665,aes(x=date,y=total_sales)) +
  geom_line(color='tomato3') +
  ggtitle('Total transactions for item #103665') +
  theme_classic() +
  theme(axis.title=element_blank())

# Looks really different from distribution for store

###############################################################################
# How were total sales impacted by April 16th, 2016 earthquake?
###############################################################################
all_sales <- train4 %>%
  group_by(date) %>%
  summarize(tot <- sum(unit_sales)) %>%
  as.data.frame %>%
  mutate(date=ymd(date))

###############################################################################
# For a given item, figure out how much promotions boost sales
# Return 1 (no change) if not statistically-significant
###############################################################################
promo_ratio <- function(item_num,plot=FALSE) {
  tmp <- train %>%
    filter(item_nbr==item_num) %>%
    filter(!is.na(onpromotion)) %>%
    group_by(date,store_nbr,onpromotion) %>%  
    summarize(total_sales=sum(unit_sales)) %>%
    as.data.frame %>%
    mutate(logsales = log(total_sales+1))
  if (plot==TRUE) {
    ggplot(tmp,aes(x=logsales)) +
      geom_density(aes(group=onpromotion,fill=onpromotion),alpha=0.3) +
      theme_classic()
  } 
  else {
    if (mean(tmp$onpromotion) > 0.01 & mean(tmp$onpromotion) < 0.99) {
      tt <- t.test(tmp[tmp$onpromotion==TRUE,'logsales'],
                   tmp[tmp$onpromotion==FALSE,'logsales'],
                   alternative='greater')
      if (tt$p.value > 0.05) { return(1) }
      return( (exp(tt$estimate[1])-1)/(exp(tt$estimate[2])-1) ) 
    }
    else { return(1) }
  }
}

system.time(promo_ratio(2113914) %>% print) # 3.84s total

# Now do this for all items and see where promotions help the most
items <- read.csv('items.csv')
# 4100 rows; may take >4h to run all t-tests. Run overnight
promo <- sapply(items$item_nbr %>% head(200),promo_ratio)

# Really, my goal here should be feature engineering. I can enrich each
# data point with features related to the store, the item, or the day.

# Stores
# Use metadata provided with stores
# Which stores cluster together in terms of correlated fluctuations?
# PCA of sales time series?
# Separate stores into quantiles

# Items
# Use item metadata
# Which items cluster together?
# PCA of sales time series
# Item quantiles
# Promotion sensitivity

# Days
# Year, month, day of week
# Special events
# Public paydays
# Day quantiles
