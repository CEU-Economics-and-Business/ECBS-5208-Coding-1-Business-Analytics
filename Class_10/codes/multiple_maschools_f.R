#######################
# Multiple regression #
#                     #
# Test scores and     #
#   student-teacher   #
#     ratio           #
#                     #
#######################


rm(list=ls())

# Libraries
#install.packages("AER")
library(AER)
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)


##########
# 1) Research question:
#     - IDEA: Want to understand the pattern (possible connection) between,
#               student's performance and learning environment!
#     - Question: What is the pattern between student-to-teacher ratio and test scores?
#     - Intention: Close to causality: interpretable coefficient.

# Get the data
data("MASchools")
df <- MASchools
rm( MASchools )

# Data description: https://www.rdocumentation.org/packages/AER/versions/1.2-9/topics/MASchools
# The Massachusetts data are district-wide averages for public elementary school
#   districts in 1998. The test score is taken from the Massachusetts Comprehensive 
#   Assessment System (MCAS) test, administered to all fourth graders in Massachusetts 
#   public schools in the spring of 1998. The test is sponsored by the Massachusetts 
#   Department of Education and is mandatory for all public (!) schools. The data analyzed 
#   here are the overall total score, which is the sum of the scores on the English, Math, 
#   and Science portions of the test. Data on the student-teacher ratio, the percent of 
#   students receiving a subsidized lunch and on the percent of students still learning 
#   english are averages for each elementary school district for the 1997--1998 school year 
#   and were obtained from the Massachusetts department of education. Data on average district 
#   income are from the 1990 US Census.


# Re-iterated research question:
#   Does better student-teacher ratio yield better score results in MCAS in 1998 in Massachusetts?
#
#####
# Model setup
# Outcome variable:      score4  - 4th grade score (math + English + science).
# Parameter of interest: stratio - Student-teacher ratio
#
# Thinking about potential confounders:
#   - background of students (socio-economic status: mother language, income level,
#                             ethnicity, parent's education level, ect. )
#   - type of school (public/private school, specialization, wealth of school)
#
#
####
# What we have:
#
# Qualitative variables by geography:
#   - district     - character. District code -> qualitative var
#   - municipality - character. Municipality name -> qualitative var
#
# Schools' wealth/measure
#   - income  - Per capita income (using 1990 census)
#   - scratio - Students per computer
#
# Shcools' expenditure variables - use only "exptot", the others are only for robustness check
#   - expreg  - Expenditures per pupil, regular
#   - expspecial - Expenditures per pupil, special needs.
#   - expbil  - Expenditures per pupil, bilingual.
#   - expocc  - Expenditures per pupil, occupational.
#   - exptot  - Expenditures per pupil, total.
#
# Schools' special students measures
#   - special - Special education students (per cent)
#   - lunch   - Percent qualifying for reduced-price lunch.
#   - english - Percent of English learners
#
# Proxys for teacher
#   - salary  - Average teacher salary
#
# Alternative outcome - great for external validity
#   - score8 - 8th grade score (math + English + science).
#
#

####
# 2) What are the possible problems with the data - data quality
#   - Representative sample?
#       - Half of the Massachusetts schools - representative only for this state
#   - Measurement errors in the variables?
#       - In general, we do not have variable on the size of the school:
#           - matter for salary, income/capita and for score(s)
#   - What they truly capture and what you want to use them for?
#       - Expenditures also capture the size of the school...
#
#
####
# 3) Descriptives
#
# Quick check on all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( df )

# Check the main parameter of interests and potential confounders:

# score 4
ggplot( df , aes(x = score4)) +
  geom_histogram(binwidth = 5,fill='navyblue') +
  labs(x = "Averaged values of test scores for schools") 

# stratio
ggplot( df , aes(x = stratio)) +
  geom_histogram(binwidth = 0.5,fill='navyblue') +
  labs(x = "Student to teacher ratio") 

# english
ggplot( df , aes(x = english)) +
  geom_histogram(binwidth = 0.5,fill='navyblue') +
  labs(x = "Ratio of english speakers (mother tounge)") 

# Create a dummy variable from english learner:
# 1 if ratio of english speakers is larger than 1%
# 0 otherwise
df <- df %>% mutate( english_d = 1*(english>1))


# As this is an already cleaned dataset there is no much to do w.r.t. cleaning...
# Scaling: already these are in percent or not so large values, thus scaling is not necessary


####
# 4) Checking some scatter-plots:
# Create a general function to check the pattern
chck_sp <- function(x_var){
  ggplot( df , aes(x = x_var, y = score4)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Averaged values of test scores") 
}

# Our main interest: student-to-teacher ratio:
chck_sp(df$stratio)
# The pattern changes around 17-18 students/teacher...
#   test it with P.L.S
#   Would be a quadratic transformation be useful?

# English learners
chck_sp(df$english)
chck_sp(df$english_d)
# It seems there is a different average (and sd) for 0 and 1

# Income per capita
chck_sp(df$income)
chck_sp( log( df$income ) )
# Result: log-transformation

# Student to computer ratio
chck_sp( df$scratio )
# Pretty much seems like uncorrelated...

# Expenditure total (neglect the others for now)
chck_sp(df$exptot)
# Not much informative... need to check whether it matters in the regression

# Special education students (per cent)
chck_sp(df$special)
# Pretty much linear and seems important...

# Percent qualifying for reduced-price lunch.
chck_sp(df$lunch)
# Seems like there is a break around 15% -> try P.L.S

# Average teacher salary
chck_sp(df$salary)
# Seems like there is a break around 35 and maybe around 40 -> try P.L.S

# Think about weightening: there is no natural candidate for this...

####
# 4) Comparing explanatory variables 
#
# Check the correlations
#
numeric_df <- keep( df , is.numeric )
cT <- cor(numeric_df , use = "complete.obs")

# Check for highly correlated values:
sum( cT >= 0.8 & cT != 1 ) / 2
# Find the correlations which are higher than 0.8
id_cr <- which( cT >= 0.8 & cT != 1 )
pair_names <- expand.grid( variable.names(numeric_df) , variable.names(numeric_df) )
# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr
# Results:
#   - only those which are 
#       a) possible outcomes, 
#       b) not inteded to include in the main regression
# Remove the un-needed variables
rm( numeric_df, id_cr, pair_names )

# Think about interactions:
#   For now it is not needed:
#     1) The parameter of interest does not include interaction.
#     2) No strong reason for controlling for such interaction.
#

#####
# 6) Modelling
#
# Start from simple to complicated
# Remember: few hundreds obs, 5-10 variable could work
#
# Main regression: score4 = b0 + b1*stratio
#   reg1: NO controls, simple linear
#   reg2: NO controls, use piecewise linear spline(P.L.S) with a knot at 18
# Use reg2 and control for:
#   reg3: english learner dummy
#   reg4: reg3 + Schools' special students measures (lunch with P.L.S, knot: 15; and special)
#   reg5: reg4 + salary with P.L.S, knots at 35 and 40, exptot, log of income and scratio


# reg1: NO control, simple linear regression
reg1 <- lm_robust(  , data = df )
summary( reg1 )

# reg2: NO controls, use piecewise linear spline(P.L.S) with a knot at 18
reg2 <- lm_robust( , data = df )
summary( reg2 )

# Extra for reg2: 
# Now, use interactions for stratio: let a dummy be: stratio > 18, 
#     and add their interaction as well
# How it is different from P.L.S? Is the parameter of interest statistically different? 
# Hint: use 2*Std.Error or CI Lower and CI Upper for comparing intervals!
reg21 <- lm_robust(  , data = df )
summary( reg21 )

###
# Side note: if want to find knots automatically (sometimes slow, and does not converge...)
library(segmented)
reg1_lm <- lm( score4 ~ stratio , data = df )
fit_seg <- segmented( reg1_lm , seg.Z = ~stratio, psi = list( stratio=17 ) )
summary(fit_seg)


###
# Models with controls:
#
# reg3: control for english learners dummy (english_d) only. 
#   Is your parameter different? Is it a confounder?

reg3 <- lm_robust( , data = df )
summary( reg3 )

# Extra for reg3
# You may wonder: what if the student-to-teacher ratio is different for those school, 
#   where the % of english learners are more than 1% (english_d)
# We can test this hypothesis! use interactions!
reg31 <- lm_robust( , data = df )
summary( reg31 )

# You can look at Pr(>|t|) to check if they are zero or
# you can use 'linearHypothesis' to whether these coefficients are zero simultaneously:
#   in this case you have to use c("beta1=0,beta2=0") format!



##
# reg4: reg3 + Schools' special students measures (lunch with P.L.S, knot: 15; and special)
reg4 <- lm_robust(  , data = df )
summary( reg4 )

##
# Control for: wealth measures as well:
#   reg5: reg4 + salary with P.L.S, knots at 35 and 40, exptot, log of income and scratio
#
# Reminder: this is already 12 variables...
reg5 <- lm_robust(  , data = df )
summary( reg5 )

##
# Summarize our findings:
data_out <- "/Users/agostonreguly/Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_10/out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5),
         type = 'html',
         custom.header = list("Average test scores for 4th graders"=1:5),
         custom.model.names = c("(1)","(2)","(3)","(4)","(5)"),
         custom.coef.names = c("Intercept","student/teacher","student/teacher (<18)","student/teacher (>=18)",
                               "english_dummy","lspline(lunch,15)1","lspline(lunch,15)2","special",
                               "lspline(salary,c(35,40))1","lspline(salary,c(35,40))2","lspline(salary,c(35,40))3",
                               "exptot","log( income )","scratio"),
         omit.coef = "english|lunch|special|salary|exptot|income|scratio",
         reorder.coef = c(2:4,1),
         file = paste0( data_out ,'MASchools.html'), include.ci = FALSE,
         single.row = FALSE, siunitx = TRUE,
         custom.gof.rows = list( English = c("NO","NO","YES","YES","YES"),
                                 Other_Special = c("NO","NO","NO","YES","YES"),
                                 Wealth_Measures = c("NO","NO","NO","NO","YES")))

###
# What if `special' is a `bad control` -> it soaks up the effect on teacher's importance!
#   Let check the result without 'special' value
# Run:
#   reg51 which is the same as reg5, but without
#   create a html with the name of 'MASchools_alter.html



#########
# If purpose is PREDICTION (!!!WHICH IS NOT THE CASE HERE!!!)
#   we can calculate some additional important fit measures:
#
# 1) y_hat-y plot - use reg5
# You may try: - not handling missing values properly...
df <- mutate( df , y_hat = reg5$fitted.values )

# Predict is more general and can handle missing values...
df <- mutate( df , y_hat = predict( reg5 , df ) )

# Create: y_hat-y plot
ggplot( data = df ) +
  geom_point (aes( x = y_hat , y = score4 ) ,  color="red")+
  geom_line( aes( x = score4 , y = score4 ) , color = "navyblue" , size = 1.5 )+
  labs( x = "Predicted test scores", y = "Actual test scores")

# Get BIC and AIC measures for the model:
#
# Unfortunately lm_robust does not have this... 
# You can use simple lm (remember, in this case SEs are not important!)

# Does adding wealth measure increase the prediction? 
#   - We need ALWAYS the same number of observations when comparing models!
reg4_lm <- lm( score4 ~ lspline( stratio , 18 ) + english_d 
               + lspline(lunch,15) + special , data = subset(df,complete.cases(df) ) )

reg5_lm <- lm( score4 ~ lspline( stratio , 18 ) + english_d 
               + lspline(lunch,15) + special 
               + lspline(salary,c(35,40)) + exptot 
               + log( income ) + scratio , data = subset(df,complete.cases(df) ) )

BIC(reg4_lm,reg5_lm)
AIC(reg4_lm,reg5_lm)

##
# EXTERNAL VALIDITY:
#
# Task: instead of score4 use score8 as an outcome variable.
# Need to check the pattern of associations as well!


#####
# Home-work: apply the same methods for California schools
# data("CASchools")
# For details see: https://www.rdocumentation.org/packages/AER/versions/1.2-9/topics/CASchools








