##########################
##     HOMEWORK         ##
##      CLASS 2         ##
##       CEU            ##
##########################

# 0) Clear work space
rm(list=ls())

# 1) Load both data from github page and inspect (summary,glimpse)
#   Hint: you will need the `raw.github...` url address

df1 <- read_csv( 'https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_schools.csv' )
df2 <- read_csv( 'https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_scores.csv' )

# 2) Merge the two data table into one called `df`
df <- left_join( df1 , df2 , by = "district" )
rm( df1 , df2 )

# 3) Put the district variable into numeric format
df$district <- as.numeric( df$district )

# 4) Create two new variables from `school_county`: 
#     - school should contain only the name of the school - character format
#     - county should only contain the name of the county - factor format
df <- df %>% separate( school_county , into = c("school","county"), sep = " - " )


# 5) Find missing values, write 1 sentence what and why to do what you do and execute.
# as they seems to be completly random, we can drop these observations
df <- df %>% filter( !is.na( english ) , !is.na( math ) , !is.na( read ) )


# 6) Create a new variable called `score` which averages the english, math and read scores
df <- df %>% mutate( score = ( english + math + read ) / 3 )

# 7) Find the county which has the largest number of schools in the data 
#     and calculate the average and the standard deviation of the score:
# Create a table - find the largest -> Sonoma
max( table( df$county ) )
# create a logical vector for Sonoma
logSonoma <- df$county == "Sonoma"
mean(df$score[logSonoma])
sd(df$score[logSonoma])


