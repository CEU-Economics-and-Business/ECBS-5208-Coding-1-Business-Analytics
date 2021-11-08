#######################
## Analysis of       ##
##  Life expectancy  ##
##    and            ##
##  Total GDP        ##
##      OR           ##
##  GPD/capita       ##
##                   ##
##      NO. 3        ##
##                   ##
## Analysis of       ##
#       the data     ##
#     SOLUTIONS      ##
##                   ##
#######################



# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
# Estimate piecewise linear splines
library(lspline)
# Fixest for estimating
library(fixest)
# For scaling ggplots
require(scales)

# Call the data from github
my_url <- "https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_6/data/life_expecatncy/clean/WDI_lifeexp_clean.csv"
df <- read_csv( my_url )



####
# 
# Quick check on all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins=20)+
  theme_bw()

summary( df )

######
# Create new variable: Total GDP:
#     note we could have download an other GDP total variable for this,
#     but for comparison purposes, let use the exact same data and 
#     concentrate on difference which are only due to transforming the variables.

df <- df %>% mutate( gdptot = gdppc*population )


######
# Check basic scatter-plots!
#   Two competing models:
#     1) lifeexp = alpha + beta * gdptot
#     2) lifeexp = alpha + beta * gdppc
#
# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log
#
# 1) lifeexp - gdptot: level-level model without scaling
ggplot( df , aes(x = gdptot, y = lifeexp)) +
  geom_point() +
  geom_smooth(method="loess",formula = 'y~x')+
  labs(x = "Total GDP (2017 int. const. $, PPP )",y = "Life expectancy  (years)") 

# You can change the scale for Total GDP for checking log-transformation
ggplot( df , aes(x = gdptot, y = lifeexp)) +
  geom_point() +
  geom_smooth(method="loess",formula = 'y~x')+
  labs(x = "Total GDP (2017 int. const. $, PPP , ln scale )",y = "Life expectancy  (years)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# You can change the scale for Total GDP and life-expectancy for checking log-transformation
ggplot( df , aes(x = gdptot, y = lifeexp ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Total GDP (2017 int. const. $, PPP , ln scale )",y = "Life expectancy  (years, ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans() )

###
# 2) lifeexp - gdppc: level-level model without scaling
ggplot( df , aes(x = gdppc, y = lifeexp)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "GDP/capita (2017 int. const. $, PPP )",y = "Life expectancy  (years)") 

# You can change the scale for GDP/capita for checking log-transformation
ggplot( df , aes(x = gdppc, y = lifeexp)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "GDP/capita (2017 int. const. $, PPP , ln scale )",y = "Life expectancy  (years)") +
  scale_x_continuous( trans = log_trans(), breaks = seq(0, 120, by = 20))

# You can change the scale for GDP/capita and life-expectancy for checking log-transformation
ggplot( df , aes(x = gdppc, y = lifeexp ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "GDP/capita (2017 int. const. $, PPP , ln scale )",y = "Life expectancy  (years, ln scale)") +
  scale_x_continuous( trans = log_trans(), breaks = seq(0, 120, by = 20)) +
  scale_y_continuous( trans = log_trans() )

####
# Conclusions:
#   1) taking log of gdptot is needed, but still non-linear pattern in data/need to use 'approximation' interpretation
  #     - feasible to check and we do it due to learn how to do it, 
  #           but in practice I would skip this -> over-complicates analysis
#   2) using only gdppc is possible, but need to model the non-linearity in data
#       - Substantive: Level changes is harder to interpret and our aim is not to get $ based comparison
#       - Statistical: log transformation is way better approximation make simplification!
#   3) taking log of gdppc is making the association close to linear!
#   4) taking log for life-expectancy does not matter -> use levels!
#       - Substantive: it does not give better interpretation
#       - Statistical: you can compare models with the same y, no better fit
#       - Remember: simplest the better!
  
# Take Log of gdp/capita and log GDP total
df <- df %>% mutate( ln_gdppc = log( gdppc ),
                     ln_gdptot = log( gdptot ) )


######
# Make some models:
#   w ln_gdptot:
#     reg1: lifeexp = alpha + beta * ln_gdptot
#     reg2: lifeexp = alpha + beta_1 * ln_gdptot + beta_2 * ln_gdptot^2
#     reg3: lifeexp = alpha + beta_1 * ln_gdptot + beta_2 * ln_gdptot^2 + beta_3 * ln_gdptot^3
#   w ln_gdppc:
#     reg4: lifeexp = alpha + beta * ln_gdppc
#     reg5: lifeexp = alpha + beta_1 * ln_gdppc + beta_2 * ln_gdppc^2
#     reg6: lifeexp = alpha + beta_1 * ln_gdppc * 1(gdppc < 50) + beta_2 * ln_gdppc * 1(gdppc >= 50)
#   Extra: weighted-ols:
#     reg7: lifeexp = alpha + beta * ln_gdppc, weights: population

###
# Two ways to handle polynomials: 
#
# 1) Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_gdptot_sq = ln_gdptot^2,
                     ln_gdptot_cb = ln_gdptot^3,
                     ln_gdppc_sq = ln_gdppc^2 )
#
# 2) Use 'poly(x,n)' function, which creates polynomials of x up to order n
#     use this approach for graphs! may use it for models: 
#                   positive - simpler, less new variables, 
#                   negative - uglier names, harder to compare
#     Note: poly() creates rotates your variables automatically to get mean independent variables
#       use raw = TRUE if you dont want to rotate your variables.

# Do the regressions
#
# Using `feols' with classical standard errors
# Reminder: formula: y ~ x1 + x2 + ..., note: intercept is automatically added
reg_b <- feols( lifeexp ~ ln_gdptot , data = df )
reg_b

# First model:
reg1 <- feols( lifeexp ~ ln_gdptot , data = df , vcov = "hetero" )
reg1

# Visual inspection:
ggplot( data = df, aes( x = ln_gdptot, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Second and third model with gdptot
reg2 <- feols( lifeexp ~ ln_gdptot + ln_gdptot_sq , data = df , vcov = "hetero" )
summary( reg2 )
# Plot
ggplot( data = df, aes( x = ln_gdptot, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

reg3 <- feols( lifeexp ~ ln_gdptot + ln_gdptot_sq + ln_gdptot_cb , data = df , vcov = "hetero" )
ggplot( data = df, aes( x = ln_gdptot, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )

# Compare these models:
etable( reg1 , reg2 , reg3 )
# From these we consider reg1 and reg3


##
# Models with gdp per capita
reg4 <- feols( lifeexp ~ ln_gdppc , data = df , vcov = "hetero" )
summary( reg4 )
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

reg5 <- feols( lifeexp ~ ln_gdppc + ln_gdppc_sq , data = df, vcov = "hetero" )
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

# Compare results with gdp per capita:
etable( reg4 , reg5 )
# Conclusion: reg5 is not adding new information

# Compare reg1, reg3 and reg4 to get an idea log transformation is a good idea:
etable( reg1 , reg3 , reg4 )
# R2 measure is much better for reg4...


# Regression with piecewise linear spline:
# 1st define the cutoff for gdp per capita
cutoff <- 50
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln <- log( cutoff )
# Use simple regression with the lspline function
?lspline
reg6 <- feols(lifeexp ~ lspline( ln_gdppc , cutoff_ln ), data = df , vcov = "hetero" )
summary( reg6 )
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )


# Weighted-OLS: use reg4 setup and weight with population
reg7 <- feols(lifeexp ~ ln_gdppc, data = df , weights = df$population , vcov = "hetero" )
summary( reg7 )

# Adding a pretty graph
ggplot(data = df, aes(x = ln_gdppc, y = lifeexp)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(1, 15)) +
  coord_cartesian(ylim = c(50, 85)) +
  labs(x = "ln(GDP per capita, thousand US dollars) ",y = "Life expectancy  (years)")+
  annotate("text", x = 4, y = 80, label = "USA", size=5)+
  annotate("text", x = 2.7, y = 79, label = "China", size=5)+
  annotate("text", x = 2,  y = 68, label = "India", size=5)


#####
# Creating model summary with etable

etable( reg4 , reg6 , reg7 , headers = c('Simple','L.Spline','Weighted'))

#####
# Based on model comparison our chosen model is reg4 - lifeexp ~ ln_gdppc
#   Substantive: - level-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - Comparatively high R2 and captures variation well


######
# Residual analysis.

# feols output is a `list` (to be more precise an `object') with different elements
# Check the `Value` section
?feols

# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$lifeexp - df$reg4_y_pred 
# Or alternatively just use reg4$residuals

# Show again the scatter plot
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Find countries with largest negative errors
df %>% top_n( -5 , reg4_res ) %>% 
      select( country , lifeexp , reg4_y_pred , reg4_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
       select( country , lifeexp , reg4_y_pred , reg4_res )







  