############################
##        CLASS 4         ##
##    GGPLOT Extras       ##
##                        ##
############################

rm( list= ls())

# Load packages
library( tidyverse )

# Play with the old hotel-vienna dataset
df <- read_csv( "https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_4/data/hotels-vienna-london.csv" )


# 1) Use different themes:
#
# Mostly used: theme_bw(), theme_grey(), theme_gray(), theme_linedraw(), theme_light, theme_dark,
#              theme_minimal(), theme_classic(), theme_void()
#       See more on: https://ggplot2.tidyverse.org/reference/ggtheme.html
#
# Some `extra` or nice themes:
#   theme_economist(), theme_stata()
#
# and many others...
#

ggplot( filter( df , city == 'Vienna' ) , aes( x = price ) ) +
  geom_histogram( alpha = 0.8, binwidth = 20 , color='white',
                  fill = 'navyblue') +
  labs(x='Hotel Prices in  Vienna',y='Density')+
  theme_bw()


##
# Task: 
#   Play around with themes!
#

##
# Creating your own theme -> go the theme_bluewhite function

# Import your source code. 
#   Note: need to be in the working directory or specify the path!
source("theme_bluewhite.R")

# Using our new imported theme
ggplot( filter( df , city == 'Vienna' ) , aes( x = price ) ) +
  geom_histogram( alpha = 0.8, binwidth = 20 ) +
  labs(x='Hotel Prices in  Vienna',y='Density')+
  theme_bluewhite()


###
# Specifying axis numbering
f1 <- ggplot( filter( df , city == 'Vienna' ) , aes( x = price ) ) +
          geom_histogram( alpha = 0.8, binwidth = 20 , color='white',
                          fill = 'navyblue') +
          labs(x='Hotel Prices in  Vienna',y='Density')+
          theme_bw()
f1
# Set the axis: 
#   1) if continuous variable: `scale_()_continuous`
#   2) if discrete/categorical variable: `scale_()_discrete`

#   a) limit -> changes the l
f1 + scale_x_continuous( limits = c( 0 , 750 ) )
#   b) set tickers, called 'breaks'
f1 + scale_x_continuous( limits = c( 0 , 750 ) , breaks = c( 0 , 100 , 150 , 200 , 250 , 400 , 500 , 600 )  )

##
# Task: - use only one graph!
#  1) Set limits between 0  and 500 for x axis
#  2) Set the breaks with binwidth of 50 for X. Use a function instead of typing in each of them!
#  3) set the limits for Y between 0 and 100
#  4) Set the breaks with binwidth of 10 for Y



# Adding lines, texts, ect. to your graph:
# Add mean and median as lines and annotate them!
#
# 1) add a line as the mean
yval = 60
f1 <- f1 + geom_segment( aes(x = mean( df$price , na.rm = T ), y = 0, 
                    xend = mean( df$price , na.rm = T ), yend = yval) , color = 'red', size = 1 )
f1
# 2) add annotation which says it is the mean
f1 <- f1 + 
  annotate( "text" , x = mean( df$price , na.rm = T ) + 20 , y = yval - 5 , label = 'Mean' , color = 'red')
f1

# 3) Calculate the median as a 50th percentile 
#   (if you wish to add oter percentiles as well, otherwise, just use `median` ) 

median_price <- quantile( df$price , .50)
# Add both of them to the figure
f1 + 
  annotate( "text" , x = median_price + 10 , y = yval + 5 , label = 'Median' , color = 'blue') +
  geom_segment( aes(x = median_price, y = 0, 
                    xend = median_price, yend = yval) , color = 'blue', size = 1 )


##
# Example for annotated boxplot

library(grid)
#install.packages("pBrackets")
library(pBrackets) 

ggplot( df , aes(y = price, x = city)) +
  geom_boxplot(color = "blue", size = 0.5, width = 0.1, alpha = 0.5) +
  labs(x='Cities',y='Price') +
  stat_boxplot(geom = "errorbar", width = 0.05,  size = 0.5) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  annotate( "text" , x = 1.5 , y = 255 , label = 'Conditional mean') + 
  geom_segment(aes(x = 1.5, y = 240, xend = 1.1, yend = 210),
               arrow = arrow(length = unit(0.15, "cm")), colour = "red") +
  geom_segment(aes(x = 1.5, y = 240, xend = 1.9, yend = 120),
               arrow = arrow(length = unit(0.15, "cm")), colour = "red") +
  annotate( "text" , x = 1.5 , y = 70 , label = 'Conditional median') + 
  geom_segment(aes(x = 1.5, y = 80, xend = 1.1, yend = 180),
               arrow = arrow(length = unit(0.15, "cm")), colour = "blue") +
  geom_segment(aes(x = 1.5, y = 80, xend = 1.9, yend = 100),
               arrow = arrow(length = unit(0.15, "cm")), colour = "blue") +
  annotate( "text" , x = 0.7 , y = 100 , label = '25th percentile') + 
  geom_segment(aes(x = 0.7, y = 110, xend = 0.9, yend = 130),
               arrow = arrow(length = unit(0.15, "cm")), colour = "blue") +
  annotate( "text" , x = 0.7 , y = 300 , label = '75th percentile') + 
  geom_segment(aes(x = 0.7, y = 280, xend = 0.9, yend = 260),
               arrow = arrow(length = unit(0.15, "cm")), colour = "blue") +
  annotate( "text" , x = 1.5 , y = 510 , label = 'Upper adjecent value:') +
  annotate( "text" , x = 1.5 , y = 490 , label = '75th percentile + 1.5*IQR') +
  geom_segment(aes(x = 1.5, y = 475, xend = 1.1, yend = 450),
               arrow = arrow(length = unit(0.15, "cm")), colour = "black") +
  theme_bw()





