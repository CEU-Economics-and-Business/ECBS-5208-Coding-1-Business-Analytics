########################
## Summary statistics ##
##      and           ##
## further graphs     ##
########################

# INTRODUCING SCRIPT
rm(list=ls())
library(tidyverse)


# Load in clean and tidy data
# Note: we have pre-manipulated the data to have:
#     prices for 04-11-2017, in Vienna and London, only hotels between 3-4 stars 
#     and we have removed hotels which has price larger than 600$ (see Bekes-Kezdi (2020, Ch3))

data_in <- "~/Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_5/data/"
heu <- read_csv(paste0( data_in , "clean/hotels-vienna-london.csv" ))

###############
# Quick check on all HISTOGRAMS
heu %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Alternatively, there is a package for the same command:
# install.packages('pairsD3')
# library(pairsD3)
# pairsD3(heu,big=TRUE)

# Quick check on all variable's summary statistics:
# Gives a quick summary stat
summary(heu)


# Check the most important variables: 
# 1) price
p <- ggplot( heu , aes( x = price , fill = city ) ) +
  geom_histogram( aes(y = ..density..) , alpha = 0.5, binwidth = 20) +
  geom_density( aes(y = ..density..) , alpha = 0.5 , bw = 20) +
  labs(x='Hotel Prices in London and Vienna',y='Density')

## Some themes
# install.packages("ggthemes")
library(ggthemes)
p + theme_economist() + scale_fill_economist()
p + theme_stata() + scale_fill_stata()
p + theme_excel() + scale_fill_excel()
p + theme_wsj() + scale_fill_wsj('colors6', '')
p + theme_gdocs() + scale_fill_gdocs()
# see further themes on: https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/

# 2) distance
ggplot( heu , aes( x = distance , fill = city ) ) +
  geom_histogram( aes(y = ..density..) , alpha = 0.5, binwidth = 0.5) +
  geom_density( aes(y = ..density..) , alpha = 0.5 , bw = 0.5) +
  labs(x='Distnace from city center London and Vienna',y='Density')

###
# New types of plots:
# 1) Box-plots
#   Remember: outliers, lower/upper adjacent = 1.5*IQR, IQR(25,75%) and median (NO MEAN!)
f1 <- ggplot(heu, aes(y = price, x = city)) +
  geom_boxplot(color = "blue", size = 0.5, width = 0.1, alpha = 0.5) +
  labs(x='Cities',y='Price')
f1

# Make it a bit more fancy by adding error-bars
f1 <- f1 + stat_boxplot(geom = "errorbar", width = 0.05,  size = 0.5)
f1
# Add the mean as a dot
f1 + stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")

# 2) Violin plot
# 
ggplot(heu, aes(y = price, x = city)) +
  geom_violin( size=1,  width = 0.5, color = 'blue', fill = 'lightblue', trim = T, show.legend=F, alpha = 0.3) +
  geom_boxplot(color = "black", fill='lightblue', size = 0.5, width = 0.1, alpha = 0.5 ,  outlier.shape = NA) +
  xlab('Cities')+
  ylab('Price')

#########
## (CONDITIONAL) BAR CHARTS
## Idea - maybe hotels which are 
#             close to center / at medium distance / far from the center 
#             are similar to each other in the two cities
#

# Let create a new factor variable
heu$dis_f <- cut( heu$distance , breaks=c(-1,2,4,100) , labels = c('close','medium','far') )

# We are curious about how these hotels distribute in the cities
# 1) Summarize the number of close/medium/far hotels

ds0 <- heu %>% 
  group_by( city , dis_f) %>% 
  summarise( numObs = n())
ds0

## Bar plot
f4 <- ggplot(ds0, aes(x=city, y=numObs, fill = dis_f)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6,  size = 0.5)+ 
  labs(x = "Citys", y = "Number of hotels", fill = "Distance")
f4
# Make the legends more pretty
f4 + scale_fill_discrete(name="Distance from city center:") +
  theme(legend.position = "top") 

## Stacked bar
ggplot(ds0, aes(x=city, y=numObs, fill = dis_f)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6,  size = 0.5) +
  labs(x = "Citys", y = "Number of hotels") +
  scale_fill_discrete(name="Distance from city center:") +
  theme(legend.position = "top") 

## Stacked bar with percentages
ggplot(ds0, aes(x=city, y=numObs, fill = dis_f)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6,  size = 0.5) +
  labs(x = "Citys", y = "Share of the hotels") +
  scale_fill_discrete(name="Distance from city center:") +
  theme(legend.position = "top") 

###
# Conditional box-plots
## Let condition the price on distance levels 
ggplot( heu , aes(y = price, x = city, fill=dis_f ) ) +
  geom_boxplot( show.legend=F ) +
  labs(x='Cities',y='Price')

# If we would like to cluster according to distance
ggplot( heu , aes(y = price, x = city, fill=dis_f ) ) +
  geom_boxplot( show.legend=F ) +
  facet_wrap(~dis_f) +
  labs(x='Cities',y='Price')

# It seems that London has higher prices and larger deviation than Vienna
# Also these graphs shows that the closer we get to the city center the higher the prices!


###########################
###########################
### EXTRA PARTS


#######
# EXTRA 1: Hypothesis testing:
#
# 1) Simple hypothesis testing: 
#     Is the average price is significantly different from 100 $ per night in Vienna?
# or  H0: average hotel price is 100$ for a night in Vienna
# What is H-Alternative?
#

# First create a subset for Vienna hotels only
hotels_vienna <- subset( heu , city == 'Vienna' )

# You can simply run a t-test in R:
t.test( hotels_vienna$price , mu = 100)

# Ok, it is probably not around  100$. Is it different from 110$?
t.test( hotels_vienna$price , mu = 110 )

###
# 2) Two-Sample t-test
#
# Question: Is the average hotel price in Vienna and London the same?
#   H0: avg. price in Vienna - avg. price in London == 0
#   H-Alternative: avg. price in Vienna - avg. price in London != 0
#
# Test for equality of average prices (interpret the result!)
t.test( price ~ city , heu )


#########
#  EXTRA 2: Correlation
#
# BINSCATTER - Scatter plots with averaged values
# We need to do some manipulation:
#   calculate the average prices conditioning on the distance level
ds1 <- heu %>% 
  group_by( dis_f ) %>% 
  summarise( avg_price = mean( price ) )
ds1

# Simple bin-scatter based on our measure of distance
ggplot( ds1 , aes(x = dis_f, y = avg_price)) +
  geom_point( size = 5)+
  labs(x='Distance',y='Average price ($)')+
  ylim(0,300)

# Now let condition on the city as well
ds2 <- heu %>% 
  group_by( dis_f , city ) %>% 
  summarise( avg_price = mean( price ) )
ds2

ggplot( ds2 , aes(x = dis_f, y = avg_price )) +
  geom_point( aes( colour = city ) , size = 5 )+
  labs(x='Distance',y='Average price ($)')


#######
# USE ALL OBSERVATIONS
#   Let's have a different question:
#     How prices and distance relates to each other?
#

## Scatter plot -> use all the observations
f2 <- ggplot( heu , aes(x = distance, y = price )) +
  geom_point(  )+
  labs(x='Distance (km)',y='Price ($)')
f2

# Approximate the conditional mean with a line
f2 + geom_smooth(method=lm,se=F,formula='y~x')
# Approximate with a lo(w)ess non-parametric line
f2 + geom_smooth(method=loess,se=F,formula='y~x',color='red')


## Scatter plot differentiate between cities
f3 <- ggplot( heu , aes(x = distance, y = price, colour = city , shape = city )) +
  geom_point( )+
  labs(x='Distance (km)',y='Price ($)')
f3
# Adding lines conditioned on cities
f3 + geom_smooth(method=lm,formula='y~x')

#####
# Bubble graph
#install.packages("viridis")
library(viridis)
# Use a random sample with 100 observations
set.seed(100)
heu_rs <- sample_n( heu , 50 )
ggplot(data = heu_rs , aes(x = distance , y = price ,                          
                           size = rating_reviewcount, color = city ) ) +  
  geom_point(alpha = 0.5) +  
  scale_size(range = c(.1, 16), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 12.5)) +  
  scale_y_continuous(limits = c(0, 500 ) ) + 
  scale_color_viridis(
    discrete = TRUE, name = "City", option = "viridis") + 
  labs(x = "Distance (km)", y = "Price ($)") + 
  theme_classic()






