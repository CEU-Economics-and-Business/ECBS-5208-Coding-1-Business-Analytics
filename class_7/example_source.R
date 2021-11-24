##
# Example to use the command `source`

rm( list = ls() )


# Create two simple vectors
N <- 100
x <- rnorm( N , mean = 5 , sd = 2 )
y <- runif( N , min = 0 , max = 10 )

# call script file, which includes functions
source( 'script_w_funcs.R' )
# Note: all your functions are called!

z <- my_addition( x , y )
calculate_mean( x )
calculate_mean( z )

# Relative vs Absolute referencing:

# for relative referencing you need to know your working directory
getwd()
# and if file exists there or within a folder, then you can call it, as we did.
source( 'script_w_funcs.R' )
# you may set the working directory as well.

# advantage: you can provide a package and one just need to set the same working directory 
#   then there are no issues of having different folder structure/operational system
# disadvantage: if you change the working directory, you will not be able to call your functions.

# absolute path:
source( '/Users/agostonreguly/Documents/Egyetem/CEU/Teaching/2021/Coding_in_R/ECBS-5208-Coding-1-Business-Analytics/class_7/script_w_funcs.R' )

# advantage: no matter in which project you work, you can call it
# disadvantage: if share the codes then you need to rewrite it.
