#####################
##  Random numbers ##
##    and          ##
##  Functions      ##
#####################

# Clear memory and load packages
rm(list=ls())
library(tidyverse)

#######
# Random numbers
#   Random numbers are often used in data science:
#     - get a random (sub)-sample
#     - bootstrapping
#     - other 'stochastic' optimization or 
#     - in some estimation (typically with ML)
#

# Keys for random numbers:
#   1) Theoretical distribution - from what kind of distribution should it sample
#       a) Actually it is a hard problem computationally 
#           -> methods implemented are pretty good, but not perfect (pseudo random methods)
#   2) Reproducible
#       a) If you estimate something which uses random numbers you should allow for the 'key'
#       b) In some cases this does not matter, the result 'averages out'
#
# Great opportunity for short presentation!










