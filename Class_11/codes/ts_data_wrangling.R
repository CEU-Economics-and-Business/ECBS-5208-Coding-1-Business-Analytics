#############################
# Handling time-series data #
# Based on                  #
# R for Data Science        #
# Ch: 13                    #
#############################



# clear memory
rm( list = ls() )

# Call packages
library(tidyverse)
# New package to handle time properties:
library(lubridate)
# Data we use: new york fligths in 2013
#install.packages("nycflights13")
library(nycflights13)



# You can get the actual date:
today()
# or 
now()
# Note the difference: 
#   today() only gives the day, 
#   now() gives the exact time with the used time zone!

###
# Reading time-format/converting to date_time
#
# String to date: 
#   usually you import string vector -> need to convert to datetime format
str <- "2020-12-10"
dtt <- ymd( str )
class(dtt)

# with lubridate you should use different functions to read different formats of date
#   alternatively you can use base function: `as.Date( str_vec , "%Y/%m/%d", tz="cet")'
#
# Automatically recognise format (in most cases)
# Month-Day-Year
mdy('January 31st, 2017')
mdy('01-31-2017')
# Day-Month-Year
dmy("31-01-2020")
# ect...

# If hours-minutes-seconds are also important (Coordinated Universal Time - Greenwich Mean Time)
ymd_hms("2020-02-13 13:15:58")
# or
mdy_hm("02/16/2023 09:01")

# You can set the time zone as well to central european time (CET)
ymd_hms("2020-02-13 13:15:58", tz = "CET" )

# Sometimes you have separately the year-month-day variables and want to combine them to one:
make_date(year="1995",month="10",day='15')
# Also works with hours/minutes, and with doubles
make_datetime(1995,10,15,5,32,tz="CET")

##
# Get parts of datetime
#
# In some cases you want to simplify/get parts of your date or want to control for seasonality:
# Get the year component
year("2020-12-20")
# Get the month component
month("2020-12-20")
# day of the mounth. day() would also work
mday("2020-12-20") 
# But maybe you are interested which day of this is in a week (1-Monday, 2-Thuesday,ect.)
#   This is handy if your seasonality has a week base.
wday("2020-12-20") 
# Some occasion you are interested in the number of day within a year
yday("2020-12-20") 
# Check for leap year
leap_year("2020-12-20")

##
# Rounding
#
# In some cases you want to round your datetime variable to a certain level (not really often used...)
dt <- ymd("2020-12-20")
# Get the first day of the month
floor_date(dt,"month")
# Get the first or last day in the month depending which is closer
round_date(dt,"month")
# Get the first day for the next month
ceiling_date(dt,"month")

##
# Time spans - durations-periods
#
# Durations: how much time spent between to time: now and the first class in DA1
my_study <- today() - ymd(20200928)
my_study
# This creates a time object, which can be used in many ways!
class(my_study)

# Calculate the duration
as.duration(my_study)

# You can make manipulations with durations!
dminutes(10) + dseconds(1)
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

# As you see, you have to be careful with durations, 
#   because it uses actual durations in a year and also neglects the time zone
#
# Fix: periods
#
last_year2 <- today() - years(1)
#
# Can use more days or other frequencies
today() - days(c(1,2,3,4))


###
rm( list = ls())
# Data-Aggregation
# We have three data-tables:
#   1) Hungarian GDP levels - quarterly from 1995Q1 - 2020Q3
#   2) Inflation and Unemployment (%) - monthly from 2001-01 - 2020-10
#   3) EUR/HUF exchange rate - daily from 2000-01-04 - 2020-12-07

url_git <- 'https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/Class_11/data/'
gdp <- read_csv(paste0( url_git , 'gdp.csv' ) )
mnb <- read_csv(paste0( url_git , 'inflat_unemp.csv' ) )
eur <- read_csv(paste0( url_git , 'eur_huf.csv' ) )

##
# GDP
# First create proper time-series variable for each datatable
gdp$time <- yq(paste0(gdp$Year,"-",gdp$Quarter))
gdp <- gdp[ !is.na( gdp$GDP ) , ]

# Plot time-series
ggplot( gdp , aes( x = time , y = GDP/10^6 ) )+
  geom_line(color = 'red',size=1) +
  labs(x="Years",y="GDP (in billions)") +
  theme_bw()
# Highly seasonal and exponentially trending...
  
##
# Inflation and unemployment
# First create proper time-series variable for each datatable
mnb$time <- ymd(paste0(mnb$Year,mnb$Month,1))
mnb <- mnb[ complete.cases( mnb ) , ]
  
# Plot time-series
ggplot( mnb )+
    geom_line( aes( x = time , y = Inflat ) , color = 'red',size=1) +
    geom_line( aes( x = time , y = Unemp ) , color = 'blue',size=1) +
    labs(x="Years",y="%") +
  theme_bw()

# These are indeed a random-walks. Unemployment has seasonality, but inflation does not
# Philips-Perron test for unit-root:
PP.test( mnb$Inflat )
PP.test( mnb$Unemp )

##
# EUR-HUF exchange rate
# First create proper time-series variable for each datatable
eur$time <- ymd(eur$Date)

# Plot time-series
ggplot( eur , aes( x = time , y = EUR) )+
  geom_line( color = 'red',size=1) +
  labs(x="Date",y="EUR/HUF") +
  theme_bw()


######
# Aggregation
#

# Base data-table is GDP
df <- gdp %>%  transmute( time = time , gdp = GDP )
rm( gdp )
##
# 1st: Aggregate mnb to quarterly frequency:
# Add years and quarters
mnb <- mnb %>% mutate( year = year(time),
                       quarter = quarter(time) )

# Average for inflation and median for unemployment (average would be also good)
agg_mnb <- mnb %>% select( year, quarter , Unemp , Inflat ) %>% 
            group_by( year , quarter ) %>% 
            summarise( inflat = mean( Inflat ),
                       unemp = median( Unemp ) )

# Add time and select variables
agg_mnb$time <- yq( paste0( agg_mnb$year , "-" , agg_mnb$quarter ))
agg_mnb <- agg_mnb %>% select( time , inflat , unemp )

# Join to df
df <- left_join( df , agg_mnb , by = "time" )
rm( agg_mnb, mnb )

##
# 2nd: Aggregate EUR to quarterly frequency:
# Add years and quarters
eur <- eur %>% mutate( year = year(time),
                       quarter = quarter(time) )

# Last day for each mount
agg_eur<-eur %>% select( time, year, quarter, EUR) %>% 
  group_by( year, quarter ) %>% 
  filter(time==max(time)) %>% ungroup()

# Adjust the time for left_join
agg_eur$time <- yq( paste0( agg_eur$year , "-" , agg_eur$quarter ))
# Join to df
df <- left_join( df , select( agg_eur , time , EUR ) , by = "time" )
rm( agg_eur,eur)

# TO BE CONTINUED...


