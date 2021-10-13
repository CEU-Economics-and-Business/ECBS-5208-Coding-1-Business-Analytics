##########################
## Dataset Handling     ##
##      CLASS 2         ##
##       CEU            ##
##########################

rm( list = ls() )
# Tidiverse includes readr package which we use for importing datasets!
library( tidyverse )



######
## Importing data:
# 3 options to import data:
#   1) Import by clicking: File -> Import Dataset -> From Text (readr) / this is csv or use other formats
#   2) Import by defining your path:

data_in <- "/Users/agostonreguly/Documents/Egyetem/CEU/Teaching/2021/Coding_in_R/codes_develop/seminar_2/data/"
df_0      <- read_csv(paste0(data_in,"clean/hotels-vienna.csv"))

# Note: your working directory (wd) is accessible by:
getwd()
# and you can set the working directory with `setwd( "YOUR PATH" )`
# if you work with 'projects' it automatically sets your path

# delete your data
rm( df_0 )

#   3) Import by using url - this is going to be our preferred method at this course!
#     Note: importing from the web is almost inferior to use your local disc, 
#       but there are some exceptions:
#         a) The dataset is considerably large (>1GB)
#         b) It is important that there is no `refresh` or change in the data
#       in these case it is good practice to download to your computer the dataset

# Can access (almost) all the datset from osf
# the hotels vienna dataset has the following url:
df <- read_csv(url("https://osf.io/y6jvb/download")) 


# glimpse on data
glimpse( df )

# Check some of the first observations
head( df )

# Have a built in summary for the variables
summary( df )


###
# Exporting your data:
#
# This is a special case: data_out is now the same as data_in (no cleaning...)
data_out <- paste0( data_in , 'clean/' )
write_csv( df , paste0( data_out , 'my_csvfile.csv' ) )

# If due to some reason you would like to export as xls(x)
install.packages( "xlsx" )
library( xlsx )
write.xlsx( df , paste0( data_out , 'my_csvfile.xlsx' ) , sheetName = "Sheet1" )

# Third option is to save as an R object
save( df , file = paste0( data_out , 'my_rfile.RData' ) )

## 
# Extra: using API
#   - tq_get - get stock prices from Yahoo/Google/FRED/Quandl, ect.
#   - WDI    - get various data from World Bank's site
#

# tidyquant
library(tidyquant)
# Apple stock prices from Yahoo
aapl <- tq_get('AAPL',
               from = "2020-01-01",
               to = "2021-10-01",
               get = "stock.prices")

# World Bank
install.packages('WDI')
library(WDI)
# How WDI works - it is an API
# Search for variables which contains GDP
a <- WDIsearch('gdp')
# Narrow down the serach for: GDP + something + capita + something + constant
a <- WDIsearch('gdp.*capita.*constant')
# Get data
gdp_data <- WDI(indicator='NY.GDP.PCAP.PP.KD', country="all", start=2019, end=2019)


##
# Tasks:
#
# 1) Go to the webpage: https://gabors-data-analysis.com/ and find OSF database under `Data and Code`
# 2) Go the the Gabor's OSF database and download manually 
#       the `hotelbookingdata.csv` from `hotels-europe` dataset into your computer.
# 3) load the data from this path
# 4) also load the data directly from the web
# 5) write out this file as xlsx and as a .RData next to the original data.

