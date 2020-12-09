2+2

myString <- "Hello world!"
print(myString)

# We can define numbers
a <- 2
b <- 3

a+b-(a*b)^a

c <- a + b
d <- a*c/b*c

# Use of logical operators
a == b
2 == 3
( a + 1 ) == b
a <- 2

a != b

# other logical operators
2 == 2 & 3 == 2
2 == 2 | 3 == 2

# Remove variables from workspace
rm(d)

##
# Create vectors
v <- c(2,5,10)
# Operations with vectors
z <- c(3,4,7)

v+z
v*z
a+v

# Number of elements
num_v <- length(v)
num_v

# Create vector from vectors
w <- c(v,z)
w
length(w)
# Gives an error
length(W)

# Note: be careful w operation
q <- c(2,3)
v+q
v+c(2,3,2)


## Extra:
null_vector <- c()
# NaN value
nan_vec <- c(NaN,1,2,3,4)
nan_vec + 3
# Inf values
inf_val <- Inf
5/0
sqrt(2)^2 == 2

# Convention to name your vaiables
my_fav_var <- "bla"
myFavVar <- "bla"
# Do not use such things
my_favourite_variable <- "bla"

# Difference between doubles and integers
int_val <- as.integer(1.6)
doub_val <- as.double(1)

#
typeof(int_val)
typeof(myString)

##
# INDEXING - goes w []
v[1]
v[2:3]
v[c(1,3)]

# Fix the addition of v+q
v[1:2] + q 

####
# Lists
my_list <- list("a",2,0==1)
my_list2 <- list(c("a","b"),c(1,2,3),sqrt(2)^2==2)

my_list2[1]
my_list2[[1]]
my_list2[1][2]


## Practice and read R for Data Science Chapter 16
