#########################
## Data Exploration    ##
##  Billion Price Proj ##
##      CLASS 3        ##
##       CEU           ##
#########################

# Remove variables from the memory
rm(list=ls())

# Call packages
library(tidyverse)
# new package we use for descriptive purposes
# install.packages("modelsummary")
library(modelsummary)



###
## Import data
bpp_orig <- read_csv( 'https://osf.io/yhbr5/download' )
# read_excel( paste0( data_in , "raw/online_offline_ALL_clean.xls" ), guess_max = 40000 )
# Check the variables
glimpse(bpp_orig)

## Create our key variable: price differences
bpp_orig <- mutate( bpp_orig , p_diff = price_online - price )

####
## DESCRIPTIVE STATISTICS
#
# Check the dataset itself
datasummary_skim( bpp_orig )
# alternatively: 
summary( bpp_orig )

# Get a better idea about the key variables
datasummary( price + price_online + p_diff ~ 
               Mean + Median + SD + Min + Max + P25 + P75 + N + PercentMissing , 
             data = bpp_orig )

# Get a better idea about price differences among different countries
datasummary( as.factor( country_s )*p_diff ~ Mean + Median , data = bpp_orig )

##
# Task
# 1) filter the data to 2016 and check the mean and median for each country
#


# Add Range as an external function to the descriptive
# Our first function:
range_ds <- function( x )
              { 
                max( x , na.rm = T ) - min( x , na.rm = T ) 
}

datasummary( price + price_online + p_diff ~ 
               Mean + Median + SD + Min + Max + P25 + P75 + N + PercentMissing + range_ds , 
             data = bpp_orig )

# Extra: create a function which gives you the mode:
mode_ds <- function(v){
  uniqv <- unique( v )
  uniqv[ which.max( tabulate( match( v, uniqv ) ) ) ]
}
datasummary( price + price_online + p_diff ~ 
               Mean + Median + mode_ds , data = bpp_orig )

###
## VISUALIZATION
#
# Check for extreme values - use of Histograms:
#   simple - built in histogram
ggplot( data = bpp_orig ) +
  geom_histogram( aes( x = price ) , fill = 'navyblue' ) +
  labs(x = "Price",
       y = "Count" )

##
# It is clear: need to filter out some data
# FILTER DATA -> filter for "PRICETYPE" is a too large restriction!
#     may check without that filter!
# use %>%  as a command which concatenates multiple commands!
bpp <- bpp_orig %>% 
  filter( is.na(sale_online) ) %>%
  filter(!is.na(price)) %>%
  filter(!is.na(price_online)) %>% 
  filter( PRICETYPE == "Regular Price" )

# Check our newly created datatable:
datasummary( price + price_online + p_diff ~ 
               Mean + Median + SD + Min + Max + P25 + P75 + N , 
             data = bpp )

# Drop obvious errors: prie is larger than $1000
bpp <- bpp %>% 
  filter( price < 1000 )

# Check again our datatable:
datasummary( price + price_online + p_diff ~ 
               Mean + Median + SD + Min + Max + P25 + P75 + N , 
             data = bpp )

# Histogram
ggplot( data = bpp ) +
  geom_density( aes( x = price ) , color = 'blue'  , alpha = 0.1 ) +
  geom_density( aes( x = price_online )  , color = 'red' , alpha = 0.1 ) +
  labs(x = "Price",
       y = "Relative Frequency" )

###
# Task
#   1) Do the same histogram, but now with the price differences
#   2) Add xlim(-5,5) command to ggplot! What changed?



# Check for price differences
chck <- bpp %>% filter( p_diff > 500 | p_diff < -500 )
# Drop them
bpp <- bpp %>% filter( p_diff < 500 & p_diff > -500 )
rm( chck )

######
## Creating factors in R
# tell R that they are nominal qualitative data
bpp$country <- factor( bpp$COUNTRY )
table(bpp$country)

# Two-ways to calculate the mean for each country:
datasummary( p_diff * country ~ Mean + SD + N , data = bpp )

# I show you how tidyverse works, while it produces tibbles, which can be used further!
bpp %>% select( country , p_diff ) %>% 
  group_by( country ) %>% 
  summarise( mean = mean( p_diff ) ,
             sd = sd( p_diff ) , 
             num_obs = n() )

# Create ggplot for countries: histogram
ggplot(data = bpp , aes( x = p_diff , fill = country ) ) +
  geom_histogram( aes( y = ..density.. ), alpha =0.4 ) +
  labs( x = "Price" , y = 'Relative Frequency' ,
        fill = 'Country' ) +
  facet_wrap(~country)+
  xlim(-4,4)

###
# Task:
# 1 )Do the same, but use geom_density instead of geom_histogram!
#     You may play around with the xlim!
# 2) Drop the `facet_wrap` command! What happens? What if instead of `fill` you use `color` or `group`


######
# HYPOTHESIS TESTING

# Test: H0: the average price difference 
#             between price_online - price = 0
#       HA: the avg price diff is non 0.

t.test( bpp$p_diff , mu = 0 )

# Test 2: The online prices are smaller or equal to offline prices
#   H0: price_online - price <= 0
#   HA: price_online - price >  0
t.test( bpp$p_diff , mu = 0 , alternative = "greater" )

# Test 3: The online prices are larger or equal to offline prices
#   H0: price_online - price >= 0
#   HA: price_online - price <  0
t.test( bpp$p_diff , mu = 0 , alternative = "less" )


# Multiple hypothesis testing
testing <- bpp %>% 
  select( country , p_diff ) %>% 
  group_by( country ) %>% 
  summarise( mean_pdiff = mean( p_diff ) ,
             se_pdiff = 1/sqrt(n())*sd(p_diff),
             num_obs = n() )

# Testing in R is easy if one understands the theory!
testing
testing <- mutate( testing , t_stat = mean_pdiff / se_pdiff )
testing
testing <- mutate( testing , p_val = pt( -abs( t_stat ) , df = num_obs - 1 ) )
testing
testing <- mutate( testing ,  p_val = round( p_val , digit = 4 ) )
testing

####
# Association
#

# Association between online-offline prices
ggplot( bpp , aes( x = price_online , y = price ) )+
  geom_point( color = 'red' )+
  labs( x = 'Price online' , y = 'Price offline' )+
  geom_smooth(method = 'lm',formula = y ~ x )

# Bin-scatter
# 1) 'easy way': using equal distances
ggplot( bpp , aes( x = price_online , y = price ) )+
  stat_summary_bin( fun = 'mean' , binwidth = 50, 
                    geom = 'point' , color = 'red',
                    size = 2 )

# 2) 'easy way': using equal distances
#   group by countries
ggplot( bpp , aes( x = price_online , y = price ,
                   color = country ) )+
  stat_summary_bin( fun = 'mean' , binwidth = 50, 
                    geom = 'point',  size = 2 ) +
  labs( x = 'Price online' , y = 'Price offline' , 
        color = "Country" ) +
  facet_wrap(~country,scales = "free",ncol = 2 )+
  theme(legend.position = "none")+
  geom_smooth(method="lm",formula = y~x)

##
# Bin-scatter 2
# Using percentiles instead of equally sized bins

bpp$price_online_10b <- bpp$price_online %>% 
  cut_number( 10 )

bs_summary <- bpp %>% 
  select( price , price_online_10b ) %>% 
  group_by( price_online_10b ) %>% 
  summarise_all( lst(p_min=min,p_max=max,
                     p_mean = mean, 
                     p_median = median,
                     p_sd = sd,
                     p_num_obs = length ))
bs_summary

# Recode interval (factor type) to new numeric variables
bs_summary <- bs_summary %>% 
  separate( price_online_10b , 
            into = c("trash","lower_bound",
                     "upper_bound" ) , 
            sep = "[^0-9\\.]" )

bs_summary <- bs_summary %>% 
  mutate( lower_bound = as.numeric(lower_bound) ) %>% 
  mutate( upper_bound = as.numeric(upper_bound)) %>% 
  select( -trash )

bs_summary <- bs_summary %>% 
  mutate( mid_point = ( lower_bound + upper_bound ) / 2 )

bs_summary

# Bin-scatter plot
ggplot( bs_summary , aes( x = mid_point , y = p_mean ) ) +
  geom_point( size = 2 , color = 'red' ) +
  labs( x = 'Online prices' , y = 'Offline prices' )+
  xlim(0,100)+
  ylim(0,100)

#####
# Correlation and plots with factors

# Covariance
cov(bpp$price,bpp$price_online)

##
# Task:
# Check if it is symmetric!


# Correlation
cor(bpp$price,bpp$price_online)

# Make a correlation table for each country
corr_table <- bpp %>% 
  select( country , price , price_online ) %>% 
  group_by( country ) %>% 
  summarise( correlation = cor( price,price_online) )

corr_table

# Graph to show the correlation pattern by each country
ggplot( corr_table , aes( x = correlation ,
                          y = fct_reorder( country , correlation ) ) ) +
  geom_point( color = 'red' , size = 2 )+
  labs(y='Countries',x='Correlation')


## 
# Task check the same for years and countries to check how the pattern altered!
# Note: 1) use color for prettier output with factor
#       2) You can alter the legend labels with `color=`

