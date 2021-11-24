##################
# Assignment 5   #
# Deadline:      #
#  Nov. 24:      # 
#     13.30      #
##################


##
# You will look at measurement error in hotel ratings!

# 0) Clear memory and import packages
rm(list=ls())
library(tidyverse)
library(fixest)




# 1) Load Vienna and do a sample selection:
# Apply filters:  3-4 stars, no NA from stars, Vienna actual, 
#   without  extreme value: price <= 600

hotels <- read_csv("https://osf.io/y6jvb/download")
hotels <- hotels %>% filter(accommodation_type=="Hotel") %>%
  filter(city_actual=="Vienna") %>%
  filter(stars>=3 & stars<=4) %>% filter(!is.na(stars)) %>%
  filter(price<=600)



# 2) Create a variable which takes the log price
# And define two cutoffs: k1 = 100, k2=200 for rating count
hotels$lnprice <- log(hotels$price)
k1=100
k2=200

# 3-5) Run 3 regressions on DIFFERENT SAMPLES:
#     reg1: logprice = alpha + beta + rating: data = rating_count < k1
#     reg2: logprice = alpha + beta + rating: data = k1 <= rating_count < k2
#     reg3: logprice = alpha + beta + rating: data = rating_count >= k2
# and save the predicted values as: yha1, yhat2, yhat3 into the hotels tibble

reg1 <- feols(lnprice ~ rating, data=subset(hotels, rating_count<k1),vcov = 'hetero')
reg1
hotels$yhat1<-predict(reg1,hotels)


reg2 <- feols(lnprice ~ rating, data=subset(hotels, rating_count>=k1 & rating_count<k2) ,vcov = 'hetero')
reg2
hotels$yhat2<-predict(reg2,hotels)


reg3 <- feols(lnprice ~ rating, data=subset(hotels, rating_count>=k2),vcov = 'hetero')
reg3 
hotels$yhat3<-predict(reg3,hotels)

# 6) Create a simple summary table for the three models.
etable( reg1 , reg2 , reg3 )

# 7) Create a Graph, which plots the rating agians yhat1 and yhat3 with a simple line
# also add an annotation: yhat1: `More noisy: # of ratings<100`
#                         yhat3: `Less noisy: # of ratings>200`
#
# Take care of labels, axis limits and breaks!

ggplot(data = hotels) +
  geom_line(aes(x = rating, y = yhat1, color = 'blue'), size = 1)+ 
  geom_line(aes(x = rating, y = yhat3, color = 'red'), size = 1)+ 
  scale_color_manual(name = "", values=c('blue', 'red'), labels=NULL, guide = 'none') +
  coord_cartesian(xlim = c(2, 5), ylim = c(3.5, 5)) +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  scale_x_continuous(expand = c(0.01,0.01), limits=c(2,5), breaks=seq(2,5, by=0.5)) +
  labs(x = "Average rating",y = "ln(Hotel price, US dollars)")+
  theme_bw() +
  annotate("text", x = 2.6, y = 4.4, label = "More noisy: # of ratings<100", size=4, color='blue')+
  annotate("text", x = 3.1, y = 3.6, label = "Less noisy: # of ratings>200", size=4, color='red')






