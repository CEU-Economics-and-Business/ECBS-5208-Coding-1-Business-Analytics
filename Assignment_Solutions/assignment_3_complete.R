#########################
## Assignment 3        ##
##      CLASS 3        ##
##  Deadline:          ##
##  2021/10/27 13:30   ##
#########################

# In this assignment we are interested if 
# Males go more frequently to doctors than females. In order to answer this question we do an analysis on
#   a cross-section data originating from the 1977–1978 Australian Health Survey.

# 0) Clear work space
rm( list = ls() )

# 1) Load tidyverse and modelsummary packages and install and load the "AER" package
library( tidyverse )
library( modelsummary )
library( AER )


# Load the data of doctor visits from AER package (more info: https://rdrr.io/cran/AER/man/DoctorVisits.html)
data("DoctorVisits")

df <- DoctorVisits
rm( DoctorVisits )

# 2) Make a quick data summary on all the variables
datasummary_skim( df )

# 3) Make descriptive statistics on the variable visits conditional on the gender
datasummary( visits * gender ~ 
               Mean + Median + SD + Min + Max + P25 + P75 + N , 
             data = df )

# 4) Create a ggplot where you display the conditional distributions of visits given the gender
ggplot( data = df , aes( x = visits , fill = gender ) ) +
  geom_density( alpha = 0.8 ) +
  labs(x = "Number of visits",
       y = "Relative Frequency" )

# 5-6) Would you consider this a useful graph? Use instead a stacked bar graph!
#   a) create a new tibble, which groups by gender and visits and count the cases (summarise)
#   b) create a ggplot using aux with the geometry: geom_bar. 
#       You should specify the following arguments: stat = "identity", position = "fill"
aux <- df %>% select( gender , visits ) %>% 
        group_by( gender , visits ) %>% 
        summarise( count = n() )

ggplot( aux , aes( x = visits , y = count, fill = gender)) +
  geom_bar( stat = "identity", position = "fill", width = 0.6,  size = 0.5) +
  labs(x = "Number of visits", y = "Share of the male/female") +
  theme(legend.position = "top") 


# 7) Test whether the number of visits are the same for male and female
#   Hint: check the t.test function and use `~` sign to define the two groups.
#   You should have a Welch-Two Sample t-test and its results.
t.test( visits ~ gender, data = df )


