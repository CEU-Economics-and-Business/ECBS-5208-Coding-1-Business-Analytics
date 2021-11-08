#######################
## Analysis of       ##
##  Life expectancy  ##
##    and            ##
##  GPD/capita       ##
##                   ##
##      NO. 1        ##
##                   ##
##  Getting the data ##
##                   ##
#######################


# Clear memory
rm(list=ls())

# Call packages
#install.packages('WDI')
library(WDI)
library(tidyverse)


# Reminder on how WDI works - it is an API
# Search for variables which contains GDP
a <- WDIsearch('gdp')
# Narrow down the serach for: GDP + something + capita + something + constant
a <- WDIsearch('gdp.*capita.*constant')

# Get GDP data
gdp_data = WDI(indicator='NY.GDP.PCAP.PP.KD', country="all", start=2019, end=2019)

##
# Task: get the GDP data, along with `population, total' and `life expectancy at birth'
# and save to your raw folder!
a <- WDIsearch('population, total')
b <- WDIsearch('life expectancy at birth')

# Get all the data - 2019 is the latest available data for life expectancy
data_raw <- WDI(indicator=c('NY.GDP.PCAP.PP.KD','SP.DYN.LE00.IN','SP.POP.TOTL'), 
                country="all", start=2019, end=2019)

# Save the raw data file
my_path <- "/Users/agostonreguly/Documents/Egyetem/CEU/Teaching/2021/Coding_in_R/ECBS-5208-Coding-1-Business-Analytics/class_6/data/life_expecatncy/"
write_csv(data_raw, paste0(my_path,'raw/WDI_lifeexp_raw.csv'))

# I have pushed it to Github, we will use that!
# Note this is only the raw files! I am cleaning them in a separate file and save the results to the clean folder!


