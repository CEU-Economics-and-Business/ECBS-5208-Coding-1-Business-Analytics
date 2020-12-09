##########################################
## Descriptive stat of Hotels in Vienna
##
## 2020-10-05

rm(list=ls())
library(tidyverse)

## Import data
data_in <- "~/Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_2/data/"
hotels_vienna <- read_csv(paste0(data_in,"clean/hotels_vienna.csv"))


# In parenthesis
getwd()
setwd()

# glimpse on data
glimpse(hotels_vienna)

# Have a summary
summary(hotels_vienna)

# Select favorite variable
hotels_vienna$price

# The average price of hotels
mean(hotels_vienna$price)

# Number of observations in hotel price vector
length(hotels_vienna$price)

###
# Visualization
ggplot( data = hotels_vienna , aes( x = price ) ) + geom_histogram()


ggplot( data = hotels_vienna , aes( x = price ) ) +
  geom_histogram( fill = "navyblue" )+
  labs(x="Hotel prices ($)",y="Absolute Frequency")


ggplot( data = hotels_vienna , aes( x = price) ) +
  geom_histogram( fill = "navyblue" , binwidth = 80 ) +
  labs(x="Hotel prices ($)",y="Absolute Frequency")

# Relative frequency graph
ggplot( data = hotels_vienna , aes( x = price) ) +
  geom_histogram( aes( y = ..density..) , fill = "navyblue" , binwidth = 20 ) +
  labs(x="Hotel prices ($)",y="Relative Frequency")

# Kernel density estimator
ggplot( data = hotels_vienna , aes( x = price ) ) +
  geom_density( aes( y = ..density.. ), color = "red" , fill = "blue", bw = 15 , alpha = 0.5 ) +
  labs(x="Hotel prices ($)",y="Relative Frequency")


# Kernel density and histogram
ggplot( data = hotels_vienna , aes( x = price ) ) +
  geom_histogram( aes( y = ..density.. ), fill = "green", binwidth = 20 ) +
  geom_density( aes( y = ..density.. ), color = "red" , fill = "blue", bw = 15 , alpha = 0.5 ) +
  labs(x="Hotel prices ($)",y="Relative Frequency")

