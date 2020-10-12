# Create a data-frame
library(tidyverse)
df <- data_frame(id=c(1,2,3,4,5,6),
                 age=c(25,30,33,22,26,38),
                 grade=c("A","A+","B","B-","B+","A"))
View(df)

# Indexing in data-frame
df[ 1 ]

# How to install package:
install.packages("moments")
#install.packages("tidyverse")
library(tidyverse)

# Go back to indexing
df[2,2]
# Create values/vectors
df[[1]]
df[1]
# Good news: df$var_name == df[[var_column]]
df$id
# Lets find age of 3rd observation or id==3
df$age[3]
df$age[df$id==3]
# Indexing with logicals
df$age
df$id==3

## Functions
age2 <- df$age[1:3]
# sum of age
age2[1]+age2[2]+age2[3]

sum(age2)
?sum
sum(age2,NaN,na.rm = TRUE)
# Calculate the mean
mean(age2)
# Standard deviation of age
sd(age2)

