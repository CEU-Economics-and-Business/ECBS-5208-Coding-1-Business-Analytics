############################
##        Class 6         ##
##  Conditional execution ##
##        loops           ##
############################

## Set memory
rm(list=ls())

####
# Conditional execution
#
# Simply ask the computer to do A if B is true 
#   and do something else if B is not true

# Case 1)
x <- 5
if ( x > 0 ){
  print("Positive number")
}

# Case 2)
if ( x > 0 ){
  print("Positive number")
} else {
  print("Negative number")
}

# Case 3) Multiple statements
y <- 0
if ( y > 0 ){
  print("Positive")
} else if ( y < 0 ){
  print("Negative")
} else {
  print("Zero")
}

# Case 4) Multiple conditions
rm(z)
if ( all( c(TRUE,FALSE) ) ){
  z <- 5
}
# Play around
x <- -2
y <- 3
if ( y >= 0 && x >= 0 ){
  z <- y + x
} else if ( y >= 0  && x < 0 ){
  z <- y - x
} else if ( y < 0 && x >= 0 ){
  z <- x - y # -y + x
} else if ( y < 0 && x < 0 ){
  z <- -y - x
}

### Notes:
# 1) If you create a variable: 
#     a) create beforehand and rename it
#     b) make sure your conditional covers all the possibilities
if ( x > 0 ){ print("positive value") } else { print("negative value ")}


########
# Loops
#   great for repetitive work in a compact format
# Two types of loops:
#   1) You know how many times you want to make something
#   2) You do not know how many times you want to make something
#       but you have a criteria/condition when to stop
#

# Case 1) purest form
for ( i in 1 : 5) {
  print( i )
}

# Case 2)
for ( i in seq( 2 , 5 ) ) {
  print( i )
}

# Case 3)
for ( i in c(2,8,9,-10) ) {
  print( i )
}

# Case 4)
for ( i in list(2,'a',TRUE) ) {
  print( i )
}

# Useful tool: seq_along()
my_v <- c(10,24,52,2)
seq_along(my_v)
for (i in seq_along( my_v ) ) {
  print( my_v[ i ] )
}

# Create a cumulative sum loop
v <- c(10,6,5,32,45,23)
cs_v <- 0 
for (i in seq_along( v ) ) {
  cs_v <- cs_v + v[ i ]
}
cs_v

cumsum(v)

# Create a vector with for loop
cs_v2 <- double( length = length( v ) )
for ( i  in seq_along( v ) ) {
  if ( i > 1 ){
    cs_v2[ i ] <- cs_v2[ i - 1 ] + v[ i ]
  } else {
    cs_v2[ 1 ] <- v[ i ]
  }
}
cs_v2

if ( all( cs_v2 == cumsum( v ) ) ){
  print("Good job!")
} else {
  print("Please refine me!")
}

####
## Measuring CPU time
# Install some developer package
# install.packages("devtools")
library(devtools)
# devtools::install_github("jabiru/tictoc")
library(tictoc)

iter_num <- 1000000
# Sloppy way
tic("Sloppy way")
cs_v3 <- c()
for (i in 1:iter_num) {
  cs_v3 <- c( cs_v3 , i )
}
toc()

# Proper way
tic("Proper way")
cs_v4 <- double( length = iter_num )
for ( i in 1 : iter_num ){
  cs_v4[ i ] <- i
}
toc()

# While loop
x <- 0 
while ( x < 10 ){
  x <- x + 1
}
x

# Instead of loop use for with condition
maxiter <- 5
x <- 0 
flag <- FALSE
for ( i in 1 : maxiter ){
  if ( x < 10 ){
    x <- x + 1
  } else{
    flag <- TRUE
    break
  }
}
if ( !flag ){
  warning('For loop did not converged, reached maximum iteration!')
}



