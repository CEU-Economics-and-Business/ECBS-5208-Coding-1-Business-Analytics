#############################
# Handling time-series data #
#                           #
# Using                     #
# R for Data Science        #
# Ch: 13                    #
#     AND                   #
# Example with Hungarian    #
#   inflation and other     #
#     covariates            #
#############################



# clear memory
rm( list = ls() )

# Call packages
library(tidyverse)
# New package to handle time properties:
install.packages("lubridate")
library(lubridate)


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
class( dtt )

# with lubridate you should use different functions to read different formats of date
#   alternatively you can use base function: `as.Date( str_vec , "%Y/%m/%d", tz="cet")'
#
# Automatically recognise format (in most cases)
# Month-Day-Year
mdy('January 31st, 2017')
mdy('01-31-2017')
# Day-Month-Year
dmy("31-01-2020")
dmy(31012020)
# ect...

# If hours-minutes-seconds are also important (Coordinated Universal Time - Greenwich Mean Time)
ymd_hms("2020-02-13 13:15:58")
# or
mdy_hm("02/16/2023 09:01")

# You can set the time zone as well to Central European Time (CET)
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
# Get quarter component
quarter("2020-12-20")
# Get the month component
month("2020-12-20")
# day of the month. day() would also work
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
# Time spans - duration vs periods
#
# Duration: how much time spent between to time: now and the first class in DA1
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

# As you see, you have to be careful with duration, 
#   because it uses actual duration in a year and neglects the time zone
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

url_git <- 'https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/Class_12/'
gdp <- read_csv(paste0( url_git , 'data/gdp.csv' ) )
mnb <- read_csv(paste0( url_git , 'data/inflat_unemp.csv' ) )
eur <- read_csv(paste0( url_git , 'data/eur_huf.csv' ) )

##
# GDP
# First create proper time-series variable for each datatable
gdp$time <- yq(paste0(gdp$Year,"-",gdp$Quarter))
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
  geom_line( color = 'red',size=1) +
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

# Based on this: we need to take the changes!

###
#
# Detour: Serial-correlation (a.k.a. Auto-correlation)
source( paste0( url_git , 'codes/ggplot.acorr.R' ) )

# ACF - gives the correlation between y_t and lags: Corr(y_t,y_t-lag)
# PACF - (Partial Autocorrelation Fnc)
#     shows the correlation between Corr(y_t,y_t-lag) | Corr( y_t , y_t-lag-1 )
#     thus what is the correlation between y_t and y_t-lag if 
#       we have controlled for the previous lags already!
#
# In both graph the dashed lines gives 95% CI for statistical value = 0
#   In ACF it means if bars within the line we have a White-Noise: Corr = 0

# Inflation
ggplot.acorr( df$inflat , lag.max = 24, ci= 0.95, 
              large.sample.size = F, horizontal = TRUE)

# GDP
ggplot.acorr( df$gdp , lag.max = 24, ci= 0.95, 
             large.sample.size = F, horizontal = TRUE)


####
# Creating new variables (can neglect and use diff operator in models...)
# GDP -> percentage changes (from level: (y_t-y_t-1)/y_t*100)
# Inflation -> percentage changes (from percent: y_t - y_t-1)
# Unemployment -> percentage changes (from percent: y_t - y_t-1)
# EUR -> percentage changes (from level: (y_t-y_t-1)/y_t*100)
#
df <- df %>% mutate( dp_gdp = ( gdp - lag( gdp , 1 ) ) / gdp * 100,
                     d_inflat = inflat - lag( inflat , 1 ),
                     d_unemp = c( NA , diff( unemp ) ),
                     dp_eur =  ( EUR - lag( EUR , 1 ) ) / EUR * 100 ) 

# Check for Unit-root
pp.test( df$dp_gdp , lag.short = F)
pp.test( df$d_inflat , lag.short = F)
pp.test( df$d_unemp , lag.short = F)
pp.test( df$dp_eur , lag.short = F)
# we are good to continue

# Check the ts
# Different graphs
df_aux <- gather(df, key = measure, value = Rate, 
                 c("dp_gdp", "d_inflat", "d_unemp","dp_eur"))

ggplot( df_aux, aes(x=time, y = Rate , color = measure ) ) + 
  geom_line() +
  facet_wrap( ~ measure , scales = "free" ,
              labeller = labeller( measure = c("dp_eur"="EUR/HUF monthly return (%)","dp_gdp"="GDP growth (%)",
                                               "d_inflat"="Inflation change (%)",
                                               "d_unemp"="Unemployment change (%)") ) ) +
  labs( x = 'Years' , y = '' ) +
  guides(color = FALSE ) +
  theme_bw()
rm(df_aux)

# Extreme value for EUR/HUF on 2009/Q1 and maybe 2009/Q2 -> use a dummy variable for this
# Add additional control variables
df <- df %>% mutate( qrt = quarter( time ),
                     d_2009Q1 = as.numeric(time == "2009-01-01"),
                     d_2009Q2 = as.numeric(time == "2009-04-01"))

###
# Some scatter-plots:

# GDP and inflation
ggplot( df , aes( x = dp_gdp , y = d_inflat ) ) +
  geom_point( color = 'red' ) +
  geom_smooth(method="loess", color="blue",se=F) +
  labs( x='GDP percent changes (un-adjusted)',y='Inflation changes (%)') +
  scale_y_continuous( limits = c(-2.5,2.5), breaks = seq(-2,2, 1) )+
  scale_x_continuous( limits = c(-25,15), breaks = seq(-20,15, 10) )+
  geom_text( aes(label = qrt ) , nudge_y = -0.15, size = 4 ) +
  theme_bw()

# Unemployment and inflation
ggplot( df , aes( x = d_unemp , y = d_inflat ) ) +
  geom_point( color = 'red' ) +
  geom_smooth(method="loess", color="blue",se=F) +
  labs( x='Unemployment changes (%)',y='Inflation changes (%)') +
  scale_y_continuous( limits = c(-2.5,2.5), breaks = seq(-2,2, 1) )+
  scale_x_continuous( limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5) )+
  theme_bw()

# EUR and inflation
ggplot( df , aes( x = dp_eur , y = d_inflat ) ) +
  geom_point( color = 'red' ) +
  geom_smooth(method="loess", color="blue",se=F) +
  labs( x='EUR/HUF exchange rate changes (%)',y='Inflation changes (%)') +
  scale_y_continuous( limits = c(-2.5,2.5), breaks = seq(-2,2, 1) )+
  scale_x_continuous( limits = c(-15,15), breaks = seq(-15,15, 5) )+
  theme_bw()

# Inflation and its lag
ggplot( df , aes( x = lag( d_inflat ) , y = d_inflat ) ) +
  geom_point( color = 'red' ) +
  geom_smooth(method="loess", color="blue",se=F) +
  labs( x='Lag of Inflation changes',y='Inflation changes') +
  scale_y_continuous( limits = c(-2.5,2.5), breaks = seq(-2,2, 1) )+
  scale_x_continuous( limits = c(-2.5,2.5), breaks = seq(-2,2, 1) )+
  theme_bw()

# But remember: for lags the auto-correlation plot is much more efficient!

####
# Model:
#  Let us model the inflation with unemployment, EUR exchange rate and with GDP growth
#   We have taken care of trends
#   We need to control for: seasonality in GDP growth
#                           dummy variable

# For Newey-west estimation
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)

# reg1: d_inflat ~ d_unemp + dp_eur + dp_gdp + seasonal dummies
reg1 <- lm( d_inflat ~ d_unemp + dp_eur + dp_gdp + as.factor( qrt ) , data = df )
reg1a <- coeftest(reg1, vcov.=NeweyWest( reg1 , prewhite=FALSE, lag=4, verbose=TRUE)) 

# reg2: d_inflat ~ d_unemp + dp_eur + dp_gdp + seasonal dummies + control extreme value for EUR/HUF on 2009/Q1 
reg2 <- lm( d_inflat ~ d_unemp + dp_eur*d_2009Q1 + dp_gdp + as.factor( qrt ) , data = df )
reg2a <- coeftest(reg2, vcov.=NeweyWest( reg2 , prewhite=FALSE, lag=4, verbose=TRUE)) 

install.packages("huxtable")
library(huxtable)
huxreg(D_Inflation=reg1a,
       D_Inflation=reg2a,
       statistics = c(N = "nobs"))

# Including lags
# reg3: reg1 + using inflation lags up to 4 lags
reg3 <- lm( d_inflat ~ d_unemp + dp_eur + dp_gdp + as.factor( qrt ) 
                      + lag( d_inflat , 1) + lag( d_inflat , 2) + lag( d_inflat , 3) 
                      + lag( d_inflat , 4), data = df )
reg3a <- coeftest(reg3, vcov.=NeweyWest( reg3 , prewhite=FALSE, lag=4, verbose=TRUE)) 


reg4 <- lm( d_inflat ~ d_unemp + dp_eur + dp_gdp + as.factor( qrt ) 
            + lag( d_inflat ,1 ) + lag( d_inflat ,2 )
            + lag( d_inflat ,3 ) + lag( d_inflat ,4 )
            + lag( dp_eur ,1 ) + lag( dp_eur ,2 )
            + lag( dp_eur ,3 ) + lag( dp_eur ,4 ), data = df )
reg4a <- coeftest(reg4, vcov.=NeweyWest( reg4 , prewhite=FALSE, lag=4, verbose=TRUE)) 


huxreg(D_Inflation=reg3a,
       D_Inflation=reg4a,
       statistics = c(N = "nobs"))
###
# Cumulative effect:
#   create double differences
df <- df %>% mutate( dd_inflat = d_inflat - lag( d_inflat , 1 ),
                     dd_eur = dp_eur - lag( dp_eur , 1 ) )

reg5 <- lm( d_inflat ~ d_unemp + dp_gdp + as.factor( qrt ) 
            + lag( d_inflat , 4 ) + lag( dd_inflat , 1 ) +
              lag( dd_inflat , 2 ) + lag( dd_inflat , 3 )
            + lag( dp_eur , 4 ) + dd_eur +
            + lag( dd_eur , 1 ) + lag( dd_eur , 2 ) + lag( dd_eur , 3 ) , data = df ) 
  
reg5a <- coeftest(reg5, vcov.=NeweyWest(reg5, prewhite=FALSE, lag=4, verbose=TRUE)) 

huxreg(D_Inflation=reg4a,
       D_Inflation=reg5a,
       statistics = c(N = "nobs"))

# Note: coefficient on the sum of lag(d_inflat,1 to 4) in reg4 
#           is equal to lag(d_inflat,4) in reg5a
#       FURTHERMORE:
#       coefficient on the sum of dp_eur and lag(dp_eur,1 to 4) in reg4 
#           is equal to lag( dp_eur , 4 ) in reg5a

# Prediction for time-series:
df <- df %>% mutate( pred_y = c( rep( NA , 5 ) , predict(reg5) ) )

##
# Graphs for visualize fit:

# Fitted and actual time-series plot
ggplot( df , aes( x = time ) ) +
  geom_line( aes( y = d_inflat , color = "inflat" ) ) +
  geom_line( aes( y = pred_y   , color = "pred" ) ) +
  scale_color_manual(name = "Variable",
                     values = c( "inflat" = "red", "pred" = "blue"),
                     labels = c("inflat" = "Inflation Actual", "pred"="Inflation Predicted")) +
  labs(x="Years",y="Inflation and model fit") +
  scale_y_continuous(limits = c(-2.5,2.5), breaks = seq(-2,2, 1))+
  theme_bw()+
  theme(legend.position="bottom")

# Fitted and actual y-hat-y plot:
#   Should check for some patterns in the error: here none
ggplot( df , aes( x = pred_y , y = d_inflat ) ) +
  geom_point( color = 'red' ) +
  geom_line( aes( y = d_inflat, x = d_inflat ) , color = 'grey' ) +
  geom_text( aes(label = paste0( year( time ) , 'Q' , qrt ) ) , nudge_y = -0.25, size = 2.5) +
  labs(x="Predicted inflation values",y="Actual inflation values") +
  scale_y_continuous(limits = c(-2.5,2.5), breaks = seq(-2,2, 1))+
  theme_bw()
  
# You should also investigate the residuals!
# Predicting outside the time horizon, while using lags is not that straightforward -> CH 18

#######
###
# Extra: GDP growth, using yearly differences

df <- df %>% mutate( dl4_gdp = ( log( gdp ) - log( lag( gdp , 4 ) ) )*100 ) 

# It looks odd for 2000-2008, because we use 'current prices'
#     instead of inflation adjusted values
ggplot( df, aes(x=time, y = dl4_gdp ) ) +
  geom_line( color = 'red' ) +
  labs( x = 'Years' , y = 'GDP growth (YoY %)' )

# Philip-perron test is inconclusive...
pp.test(df$dl4_gdp)

# Control for:
#   trending decrease between 2000-2008 (tis_00_08)
#   financial crises 2009: (sis_09Q1_2,sis_09Q3_4)
#   stable growth between 2010-2020 (sis_10_20)
#   Covid-19 recession 2020 (iis_20Q2,iis_20Q3)
nobs <- length(df$time)
df <- df %>% mutate( tis_00_08 = c(1:36,rep(0,nobs-36) ),
                     sis_09Q1_2 = c(rep(0,36),1,1,rep(0,nobs-38)),
                     sis_09Q3_4 = c(rep(0,38),1,1,rep(0,nobs-40)),
                     sis_10_20 = c(rep(0,40),rep(1,nobs-42),0,0),
                     iis_20Q2 = 1*( time == '2020-04-01'),
                     iis_20Q3 = 1*( time == '2020-07-01') )

# Model: controlling on the time intervals
reg6 <- lm( dl4_gdp ~ tis_00_08 + sis_09Q1_2 + sis_09Q3_4 + 
                      sis_10_20 + iis_20Q2 + iis_20Q3 , data = df )

reg6a <- coeftest(reg6, vcov.=NeweyWest(reg6, prewhite=FALSE, lag=4, verbose=TRUE)) 

# Save the predicted values (lines)
df <- df %>% mutate( pred_gdp = c( rep( NA , 4 ) , predict( reg6 ) ) )

# Showing different time-periods (regimes) - Note GDP (%YoY) is unadjusted! 
ggplot( df, aes( x=time ) ) +
  geom_line( aes( y = dl4_gdp ) , color = 'red' ) +
  geom_line( aes( y = pred_gdp ) , color = 'blue' ) +
  annotate("text", x = ymd("2005-01-01"), y = 15, label = "Economy slows down", angle=-30) +
  annotate("rect", xmin = ymd("2000-01-01"), xmax = ymd("2008-10-01"), ymin = -12, ymax = 20,
           alpha = .2, fill = "yellow" )+
  annotate("text", x = ymd("2009-07-01"), y = 15, label = "Financial crises", angle=90) +
  annotate("rect", xmin = ymd("2008-10-01"), xmax = ymd("2010-01-01"), ymin = -12, ymax = 20,
           alpha = .2, fill = "purple" )+
  annotate("text", x = ymd("2015-01-01"), y = 10, label = "Stable growth") +
  annotate("rect", xmin = ymd("2010-01-01"), xmax = ymd("2019-10-01"), ymin = -12, ymax = 20,
           alpha = .2, fill = "green" )+
  annotate("text", x = ymd("2020-03-01"), y = 15, label = "Covid-19 crises", angle=90) +
  annotate("rect", xmin = ymd("2019-10-01"), xmax = ymd("2020-10-01"), ymin = -12, ymax = 20,
           alpha = .2, fill = "blue" )+
  scale_y_continuous(limits = c(-12,20), breaks = seq(-10,20, 5))+
  labs( x = 'Years' , y = 'GDP growth (YoY %)' )

# Of course you can mix these 'indicators' with continuous x-s, 
#   which makes the model even more sense







