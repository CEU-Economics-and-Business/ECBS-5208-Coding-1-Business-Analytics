############################
# Random Number Generation #
#     CLASS 5              #
############################

rm( list = ls() )

# 1) Uniform distribution
n <- 10
x <- runif( n , min = 0 , max = 10 )

set.seed( 123 )
x <- runif( n , min = 0 , max = 10 )
x

# Normal distribution
n <- 100000
y <- rnorm( n , mean = 1 , sd = 2 )
library(tidyverse)
df <- tibble( var1 = y )
ggplot( df , aes( x = var1 ) ) +
  geom_histogram( aes( y = ..density.. ) , fill = 'navyblue') +
  stat_function( fun = dnorm , args = list( mean = 1, sd = 2 ),
                  color = 'red' , size = 1.5 )

# Other distributions:
# rbinom, rexp, rlnorm, ect. 

###
# Sampling
# Case 1) Without replacement:
# Note: lenght( y ) <= 100
sample_1 <- sample( y , 100, replace = FALSE )
sample_1

# Case 2) With replacement
# Note length( y ) can be smaller
sample_2 <- sample( y , 2000 , replace = TRUE )

###
# TASK:
# Write a for loop with 1000 iteration:
#  in each iteration calculate the mean of y
#    based on 100 observations and save it to a vector
n <- 100000
y <- rnorm( n , mean = 1 , sd = 2 )
iter_num <- 1000
mean_vec <- double( length = iter_num )
for ( i in 1 : iter_num ){
  set.seed( i )
  sample_i <- sample( y , 100 , replace = FALSE )
  mean_vec[ i ] <- mean( sample_i )
}
df_m <- tibble( means = mean_vec )
ggplot( df_m , aes( x = means ) ) +
  geom_histogram( aes( y = ..density.. ) , fill = 'navyblue') +
  stat_function( fun = dnorm , args = list( mean = 1, 
                                            sd = ( 2 / sqrt( 100 ) ) ),
                 color = 'red' , size = 1.5 )

