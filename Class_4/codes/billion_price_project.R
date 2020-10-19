###########################
## CLASS 4 - Coding in R ##
##                       ##
## Billion-Price-Project ##
###########################


## Start script
# Remove variables from the memory
rm(list=ls())
# Call packages
library(tidyverse)
library(moments)
# new package we use
#install.packages("readxl")
library(readxl)

## Import data
data_in <- "~/Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_4/data/"
# Reading with excel and telling that the parsing should check the first 40000 observations
bpp_orig <- read_excel( paste0( data_in , "raw/online_offline_ALL_clean.xls" ), guess_max = 40000 )
# Check the variables
glimpse(bpp_orig)

## Create our key variable
bpp_orig <- mutate( bpp_orig , p_diff = price_online - price )


## Check some of the key variables -> introducing the %>% 
# Note: we will learn how to do this without writing this part down 3 times...
# 1) Online and offline prices
p1_sum <- bpp_orig %>% summarise(
  mean     = mean(price),
  median   = median(price),
  std      = sd(price),
  iq_range = IQR(price), 
  min      = min(price),
  max      = max(price),
  skew     = skewness(price),
  numObs   = sum( !is.na( price ) ) )
p2_sum <- bpp_orig %>% summarise(
  mean     = mean(price_online),
  median   = median(price_online),
  std      = sd(price_online),
  iq_range = IQR(price_online), 
  min      = min(price_online),
  max      = max(price_online),
  skew     = skewness(price_online),
  numObs   = sum( !is.na( price_online ) ) )
p3_sum <- bpp_orig %>% summarise(
  mean     = mean(p_diff),
  median   = median(p_diff),
  std      = sd(p_diff),
  iq_range = IQR(p_diff), 
  min      = min(p_diff),
  max      = max(p_diff),
  skew     = skewness(p_diff),
  numObs   = sum( !is.na( p_diff ) ) )

# Join the to table
price_summary <- p1_sum %>% add_row( p2_sum ) %>% add_row( p3_sum )
price_summary
rm( p1_sum , p2_sum , p3_sum )

# Check for extreme values
# Histogram
ggplot( data = bpp_orig ) +
  geom_histogram( aes( x = price ) , color = 'blue'  , alpha = 0.1 ) +
  labs(x = "Price",
       y = "Count" )

# Need to filter out some data
# FILTER DATA -> filter for "PRICETYPE" is a large restriction!
#     may check without that filter!
bpp <- bpp_orig %>% 
  filter(is.na(sale_online)) %>%
  filter(!is.na(price)) %>%
  filter(!is.na(price_online)) %>% 
  filter(PRICETYPE == "Regular Price")

# Drop obvious errors
bpp <- bpp %>% 
  filter( price < 1000 )

# Check the summary stat
p1_sum <- bpp %>% summarise(
  mean     = mean(price),
  median   = median(price),
  std      = sd(price),
  iq_range = IQR(price), 
  min      = min(price),
  max      = max(price),
  skew     = skewness(price),
  numObs   = sum( !is.na( price ) ) )
p2_sum <- bpp %>% summarise(
  mean     = mean(price_online),
  median   = median(price_online),
  std      = sd(price_online),
  iq_range = IQR(price_online), 
  min      = min(price_online),
  max      = max(price_online),
  skew     = skewness(price_online),
  numObs   = sum( !is.na( price_online ) ) )
p3_sum <- bpp %>% summarise(
  mean     = mean(p_diff),
  median   = median(p_diff),
  std      = sd(p_diff),
  iq_range = IQR(p_diff), 
  min      = min(p_diff),
  max      = max(p_diff),
  skew     = skewness(p_diff),
  numObs   = sum( !is.na( p_diff ) ) )



# Join the to table
price_summary <- p1_sum %>% add_row( p2_sum ) %>% add_row( p3_sum )
price_summary
rm( p1_sum , p2_sum , p3_sum )


# Histogram
ggplot( data = bpp ) +
  geom_density( aes( x = price ) , color = 'blue'  , alpha = 0.1 ) +
  geom_density( aes( x = price_online )  , color = 'red' , alpha = 0.1 ) +
  labs(x = "Price",
       y = "Relative Frequency" )

# Check the price differences
ggplot( data = bpp ) +
  geom_density( aes( x = p_diff ) , color = 'blue'  , alpha = 0.1 ) +
  labs(x = "Price differences",
       y = "Relative Frequency" )

# Check for price differences
chck <- bpp %>% filter( p_diff > 500 | p_diff < -500 )
# Drop them
bpp <- bpp %>% filter( p_diff < 500 & p_diff > -500 )
rm( chck )



