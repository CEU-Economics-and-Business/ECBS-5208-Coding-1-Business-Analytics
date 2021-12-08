##############################
#         CLASS 9            #
#  TIME-SERIES ANALYSIS      #
#                            #
# Extra data manipulations   #
#  with time-series          #
##############################

# clear memory
rm( list = ls() )

# Call packages
library(tidyverse)
library(lubridate)


# You can get the actual date:
today()
# or 
now()
# Note the difference: 
#   today() only gives the day, 
#   now() gives the exact time with the used time zone!

##
# Rounding
#
# In some cases you want to round your datetime variable to a certain level (not really often used...)
dt <- ymd("2021-12-08")
# Get the first day of the month
floor_date(dt,"month")
# Get the first or last day in the month depending which is closer
round_date(dt,"month")
# Get the first day for the next month
ceiling_date(dt,"month")


# Data-Aggregation
# We have three data-tables:
#   1) Hungarian GDP levels - quarterly from 1995Q1 - 2020Q3
#   2) Inflation and Unemployment (%) - monthly from 2001-01 - 2020-10
#   3) EUR/HUF exchange rate - daily from 2000-01-04 - 2020-12-07

url_git <- 'https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_9/'
gdp <- read_csv(paste0( url_git , 'data/gdp.csv' ) )
mnb <- read_csv(paste0( url_git , 'data/inflat_unemp.csv' ) )
eur <- read_csv(paste0( url_git , 'data/eur_huf.csv' ) )

##
# GDP
# First create proper time-series variable for each datatable
gdp$time <- yq( paste0( gdp$Year,"-",gdp$Quarter ) )
gdp <- gdp[ !is.na( gdp$GDP ) , ]

# Plot time-series
ggplot( gdp , aes( x = time , y = GDP/10^6 ) )+
  geom_line(color = 'red',size=1) +
  labs(x="Year",y="GDP (billions)") +
  theme_bw()
# Highly seasonal and exponentially trending...

##
# Inflation and unemployment
# First create proper time-series variable for each datatable
mnb$time <- ymd(paste0(mnb$Year,mnb$Month,1))
mnb <- mnb[ complete.cases( mnb ) , ]

# Plot time-series
ggplot( mnb , aes( x = time ) )+
  geom_line( aes( y = Inflat , color = "inflat" ) , size = 1 ) +
  geom_line( aes( y = Unemp  , color = "unemp" ) , size = 1  ) +
  labs(x="Year", y="%") +
  scale_color_manual(name = "Variable",
                     values = c( "inflat" = "blue", "unemp" = "orange"),
                     labels = c("inflat"="Inflation","unemp"="Unemployment")) +
  theme_bw()

# These are indeed a random-walks. Unemployment has seasonality, but inflation does not

##
# EUR-HUF exchange rate
# First create proper time-series variable for each datatable
eur$time <- ymd(eur$Date)

# Plot time-series
ggplot( eur , aes( x = time , y = EUR) )+
  geom_line( color = 'red',size=0.5) +
  labs(x="Date",y="EUR/HUF") +
  theme_bw()


######
# Aggregation
#

# Base data-table is GDP
df <- gdp %>%  transmute( time = time , gdp = GDP/10^6 )
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

# Keep only complete cases - !!!USE WITH CARE!!!
df <- df[ complete.cases(df) , ]


###
# Visualization of the data:

# NO 1: check the time-series in different graps
df_aux <- gather(df, key = measure, value = Rate, 
                 c("gdp", "inflat", "unemp","EUR"))

ggplot( df_aux, aes(x=time, y = Rate , color = measure ) ) + 
  geom_line() +
  facet_wrap( ~ measure , scales = "free" ,
              labeller = labeller( measure = c("EUR"="EUR/HUF exchange rate","gdp"="GDP (billions)",
                                               "inflat"="Inflation (%)",
                                               "unemp"="Unemployment (%)") ) ) +
  labs( x = 'Years' , y = '' ) +
  guides(color = FALSE ) +
  theme_bw()

rm(df_aux)

# NO 2: standardization - good to compare the (co)-movement
#   !!TAKE CONCLUSION ONLY IF STATIONARY!!
stdd <- function( x ){ ( x - mean( x , rm.na = T ) ) / sd( x , na.rm = T ) }

ggplot( df , aes( x = time ) ) +
  geom_line( aes( y = stdd( gdp )    , color = "gdp" ) ) +
  geom_line( aes( y = stdd( inflat ) , color = "inflat" ) ) +
  geom_line( aes( y = stdd( unemp )  , color = "unemp" ) ) +
  geom_line( aes( y = stdd( EUR )    , color = "EUR") ) +
  scale_color_manual(name = "Variable",
                     values = c( "gdp" = "red", "inflat" = "blue",
                                 "unemp" = "orange", "EUR" = "green"),
                     labels = c("gdp" = "GDP", "inflat"="Inflation",
                                "unemp"="Unemployment", "EUR"="EUR")) +
  labs(x="Years",y="Standardized values")

###
# Analyzing time-series properties:
#

##
# UNIT-ROOT TESTS
install.packages("aTSA")
library(aTSA)

# Philips-Perron test for unit-root:
# Type 1: y_t = rho * y_t-1
# Type 2: y_t = alpha + rho * y_t-1
# Type 3: y_t = alpha + delta * t + rho * y_t-1 (Neglect this output!)
#   Reason to neglect: the power of this test is low (agianst e.g. seasonality)
pp.test( df$gdp , lag.short = F)
pp.test( df$inflat , lag.short = F)
pp.test( df$unemp , lag.short = F)
pp.test( df$EUR , lag.short = F)
