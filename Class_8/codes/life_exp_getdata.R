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
install.packages('WDI')
library(WDI)


# How WDI works - it is an API
# Search for variables which contains GDP
a <- WDIsearch('gdp')
# Narrow down the serach for: GDP + something + capita + something + constant
a <- WDIsearch('gdp.*capita.*constant')

# Get data
gdp_data <- WDI(indicator='NY.GDP.PCAP.PP.KD', country="all", start=2019, end=2019)


a <- WDIsearch('population, total')
b <- WDIsearch('life expectancy at birth')

# Get all the data - 2018 is the latest available data for life expectancy
data_raw <- WDI(indicator=c('NY.GDP.PCAP.PP.KD','SP.DYN.LE00.IN','SP.POP.TOTL'), 
                country="all", start=2018, end=2018)

# Save the raw data file
my_path <- "Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_8/data/"
write_csv(data_raw, paste0(my_path,'raw/WDI_lifeexp_raw.csv'))

# I have pushed it to Github, we will use that!
# Note this is only the raw files! I am cleaning them in a separate file and save the results to the clean folder!


