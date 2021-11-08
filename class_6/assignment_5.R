##################
# Assignment 5   #
# Deadline:      #
#  Nov. 24:      # 
#     13.30      #
##################


##
# You will look at measurement error in hotel ratings!

# 0) Clear memory and import packages (tidyverse and fixest is enough for this assignment)


# 1) Load Vienna and do a sample selection:
# Apply filters:  3-4 stars, no NA from stars, Vienna actual, 
#   without  extreme value: price <= 600


# 2) Create a variable which takes the log price
# And define two cutoffs: k1 = 100, k2=200 for rating count


# 3-5) Run 3 regressions on DIFFERENT SAMPLES:
#     reg1: logprice = alpha + beta + rating: data = rating_count < k1
#     reg2: logprice = alpha + beta + rating: data = k1 <= rating_count < k2
#     reg3: logprice = alpha + beta + rating: data = rating_count >= k2
# and save the predicted values as: yhat1, yhat2, yhat3 into the hotels tibble



# 6) Create a simple summary table for the three models.


# 7) Create a Graph, which plots the rating agians yhat1 and yhat3 with a simple line
# also add an annotation: yhat1: `More noisy: # of ratings<100`
#                         yhat3: `Less noisy: # of ratings>200`
#
# Take care of labels, axis limits and breaks!





