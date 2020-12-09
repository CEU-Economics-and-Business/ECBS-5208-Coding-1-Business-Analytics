#######################
# Probability models  #
#                     #
# Health              #
#   and               #
#     smoking         #
#                     #
#######################

# Based on Chapter 11
# share-health dataset
# version 0.9


# CLEAR MEMORY
rm(list=ls())


#install.packages("pscl")
#install.packages("modelsummary")
#install.packages("mfx")
#install.packages("margins")

# Import libraries
library(tidyverse)
library(lspline)
library(estimatr)
library(mfx)
library(margins)
library(pscl)
library(modelsummary)
library(stargazer)


##
# Loading the data
w_dir <- "Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_10/"


######
# 1. PART - Create workfile

# load in clean and tidy data and create workfile
share <- read.csv(paste0(w_dir,"data/clean/share-health.csv"),stringsAsFactors = F)

# Creating binary variable for health: takes 1 if sphus is 1 or 2, otherwise 0.
share$healthy <- ifelse( share$sphus>0 & share$sphus<=5, 
                         ifelse(share$sphus==1 | share$sphus==2, 1, 0), NA)
table(share$healthy)

# Before remove observations, where healthy is missing value, check their characteristics!
# Now we are going to skip this step...
share <- share[!is.na(share$healthy), ]

# baseline is wave 4 (2011) and the endline is wave 6 (2015)
share <- share %>% mutate( baseline = ifelse(share$wave==4, 1, 0),
                           endline  = ifelse(share$wave==6, 1, 0) )

table(share$baseline)
table(share$endline)

# We are curious, who stays healthy at endline:
#   1 if endline and healthy is 1, 0 if endline is 1, but healthy is 0, otherwise NA
share$temp <- ifelse(share$endline==1, 
                     ifelse(share$healthy==1, 1, 0), NA)
table(share$temp)

# Now we can create `stayshealthy' variable: take the maximum value for temp given mergeid
#   Needs to group by mergeid (individuals) -> to filter out duplicates
#     and check if temp is equal to 1, 0 or NA and choose the largest.
# This step takes for a while -> open sum_stat.R script!
share <- share %>% group_by(mergeid) %>% mutate(stayshealthy = max(temp, na.rm=TRUE)) 
table(share$stayshealthy)
# Delete temporary variable
share$temp <- NULL

# keep if:
#   1) endline health outcome non-missing
#   2) baseline observations (endline outcome already defined for them)
#   3) age 50-60 at baseline
#   4) healthy individuals at baseline

share <- share %>% filter(
            stayshealthy == 1 | stayshealthy == 0,
            baseline == 1 ,
            age >= 50 & age <= 60,
            healthy == 1 )


# re-define smoking to be 0-1 (5 here means 0)
share$smoking[share$smoking == 5] <- 0
share$ever_smoked[share$ever_smoked == 5] <- 0
# keep those with non-missing observations for smoking at baseline
share <- share %>% filter( smoking == 1 | smoking == 0 ,
                           ever_smoked == 1 | ever_smoked == 0 )

##
# Adjust other variables: 
# exerc - doing weekly exercises more than once: if br015 = 1,
#         otherwise it is 0, if negative -> missing value
share$exerc <- ifelse(share$br015==1, 1, ifelse(share$br015>0 & share$br015!=1 , 0, NA))
table(share$exerc)

# bmi - Body mass index
share$bmi <- ifelse(share$bmi<0, NA, share$bmi)
summary(share$bmi)

# Rename:income_pct_w4 to income10
names(share)[names(share) == 'income_pct_w4'] <- 'income10'
# Married status: 1-married, 2-registered partner status, others are non-married categories
share$married <- ifelse(share$mar_stat==1 | share$mar_stat==2, 1, 0 )
# Education years
share$eduyears <- ifelse(share$eduyears_mod<0, NA, share$eduyears_mod)
summary(share$eduyears)
# Remove eduyears_mod value
share$eduyears_mod <- NULL

# Remove if any of a newly created variable is missing
share <- share[!is.na(share$bmi) & !is.na(share$eduyears) & !is.na(share$exerc), ]

# Call a function from a file:
source(paste0(w_dir,'/codes/sum_stat.R'))

# Make descriptive statistics for selected variables
desc_stat <- sum_stat( share , var_names = c('stayshealthy','smoking','ever_smoked','female',
                                              'age','income10','eduyears','bmi','exerc'),
                               stats = c('mean','median','min','max','sd') )

##
# TO DO:
#   read 'country_id.csv' and join by country to share data
#    in the end you need to have country_str for each observations, 
#     where country_str stands for country strings

# Import
country_id <- read.csv(paste0(w_dir,"data/clean/country_id.csv"))
# left join to share by country
share <- left_join( share , country_id, by = "country" )

# Check the number of people stayed healthy by countries
table(share$country_str,share$stayshealthy)

# Remove non-needed variables
rm( desc_stat , country_id )

# save ready-to-analyze data
write.csv( share, paste0( w_dir , "data/clean/share-health-filtered.csv"), row.names = F)

# In case of massing up the models:
share <- read.csv( paste0( w_dir , "data/clean/share-health-filtered.csv") )

####
# 2. PART - SATURATED LPM MODELS
#

# Linear probability models of good health at endline and smoking

# 1st model: current smoker on RHS
lpm1 <- lm( stayshealthy ~ smoking, data=share )
summary( lpm1, vcov=sandwich )

# Get the predicted values
share$pred1 <- predict( lpm1 )

# Compare smoking with predicted values and real outcomes
table(share$pred1, share$smoking)
table(share$stayshealthy, share$smoking)

# Create weights for prettier plot
share<-share %>%
  group_by(smoking, stayshealthy) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))

ggplot(data = share, label=smoking) +
  geom_point( aes(x = smoking, y = pred1), size = 2, color="red", shape = 16) +
  geom_line(  aes(x = smoking, y = pred1), colour="red",  size = 0.7) +
  geom_point( aes(x = smoking, y = stayshealthy, size=weight_2), fill = "blue", color="blue",
                  shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Current smoker",y = "Staying healthy / Predicted probability of ")+
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,1))

# 2nd model: current smoker and ever smoked on RHS
lpm2 <- lm(stayshealthy ~ smoking + ever_smoked, data=share)
summary(lpm2, vcov=sandwich)

stargazer(list(lpm1, lpm2), digits=3, out=paste(w_dir,"out/saturated_lmp.html",sep=""))
rm(lpm2)
rm(lpm1)


####
# 3. PART - LINEAR PROBABILITY MODELS & PREDICTION
#
# Using more RHS variables!
#   first check some functional forms

# For pretty plots create weigths for education
share<-share %>%
  group_by( eduyears, stayshealthy ) %>%
  mutate( weight = n()/100 )

# Education years vs staying healthy
ggplot(data = share, aes(x=eduyears, y=stayshealthy)) +
  geom_point(aes(x = eduyears, y = stayshealthy, size=weight), color="red", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  geom_smooth(method="loess", color="blue") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,25), breaks = seq(0,25,4))+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  labs(x = "Years of education",y = "Probability of staying healthy ")
# Seems like non-linear... Use P.L.S with knots at 8 (elementary only) and 18 (Diploma)

# 10 Income categories vs staying healthy
ggplot(data = share, aes(x=income10, y=stayshealthy)) +
  geom_point(aes(x = income10, y = stayshealthy ), color="red", shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  geom_smooth(method="loess", color="blue") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(1,10), breaks = seq(1,10,1))+
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
  labs(x = "Income group within country (deciles)",y = "Probability of staying healthy ")
# Seems like linear...

# Age vs staying healthy
ggplot(data = share, aes(x=age, y=stayshealthy)) +
  geom_smooth(method="loess", color="blue") +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent) +
  labs(x = "Age at interview (years)",y = "Probability of staying healthy") 
# Seems like linear...

# BMI vs staying healthy
ggplot(data = share, aes(x=bmi, y=stayshealthy)) +
  geom_smooth(method="loess", se=F, color="blue", size=1.5) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  labs(x = "Body mass index",y = "Stays healthy") +
  scale_x_continuous(limits = c(10,50), breaks = seq(10,50, 10))
# Seems like non-linear... Use P.L.S with knot at 35

###
# lpm3: linear probability model with many covariates:
#   smoking + ever_smoked + female + age + eduyears + income10 + bmi + exerc 
#       + as.factor(country)
#   use the P.L.S transformations:
#     eduyears: with knots at 8 (elementary only) and 18 (Diploma)
#     bmi: with knot at 35
#   and include country dummy variables as.factor(country) -> 
#     -> it automatically drops the first category: 11 (Austria), which is now the reference category

lpm3 <-lm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
            income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share)
summary(lpm3, vcov=sandwich)
# Save the model output
stargazer(lpm3, digits=3, out=paste(w_dir,"out/lpm_rich.html",sep=""))

# Check predicted probabilities: is there any interesting values?
# predicted probabilities
share$pred_lpm <- predict(lpm3)
# Summary
summary(share$pred_lpm)

# Show the predicted probabilities' distribution
ggplot(data=share, aes(x=pred_lpm)) +
  geom_histogram( aes( y = ..density.. ), fill = 'navyblue', binwidth=0.02) +
  coord_cartesian(xlim = c(0, 1.2)) +
  labs(x = "Predicted probability of staying healthy (LPM)",y = "Percent")

# We are interested in the top 1% and bottom 1% characteristics!
#   Is there any significant difference?

# Create bins which categorize the predicted values between 1-100
share <- share %>% 
  mutate(q100_pred_lpm = ntile(pred_lpm, 100))

# Make a summary statistics, using sum_stat for the bottom (q100_pred_lpm==1) 
#   and top 1% (q100_pred_lpm==100), using stats = c('mean','median','sd')
#   and variables c('smoking','ever_smoked','female','age','eduyears','income10','bmi','exerc')
#   use the num_obs = F input for sum_stat

# Bottom 1% means low probability of stayhealthy
# 'smoking','ever_smoked','female','age','eduyears','income10','bmi','exerc'
b1 <- share %>% filter( q100_pred_lpm == 1 )
var_interest <- c('smoking','ever_smoked','female','age','eduyears','income10','bmi','exerc')
stat_interest <- c('mean','median','sd')
sum_stat(b1,var_interest,stat_interest,num_obs = F)

# Top 1% means high probability of stayhealthy
t1 <- share %>% filter( q100_pred_lpm == 100 )
sum_stat(t1,var_interest,stat_interest,num_obs = F)

# You may change the variable names to remove...
rm(lpm3,t1,b1,stat_interest,var_interest)

####
# 4. PART - LOGIT AND PROBIT MODELS
#
# Lets compare
# lpm versus logit and probit
# with all right-hand-side variables
# If comparing different estimation methods for the same model setup:
#   good practice to make a 'formula' variable!
model_formula <- formula( stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
                            income10 + lspline(bmi, c(35)) + exerc + as.factor(country) )

# lpm (repeating the previous regression)
lpm <-lm( model_formula , data=share)
summary(lpm, vcov=sandwich)

# logit coefficients:
#   alternatively: familiy='binomial' automatically gives you logit, but not probit...
logit <- glm( model_formula , data=share, family=binomial(link="logit") )
summary(logit)
glance(logit)

# predicted probabilities 
share$pred_logit <- predict.glm(logit, type="response")
summary(share$pred_logit)

# Calculate logit marginal differences
logit_marg <- logitmfx( model_formula, data=share, atmean=FALSE, robust = T)
print(logit_marg)

# Or can calculate manually - create variables:
#education
share$eduyears0_8 <- ifelse(share$eduyears <=8, share$eduyears ,8)
share$eduyears8_18 <- ifelse(share$eduyears <=8, 0, ifelse(share$eduyears>8 & share$eduyears<18, share$eduyears-8, share$eduyears-(share$eduyears-18)-8))
share$eduyears18 <- ifelse(share$eduyears<=18, 0, share$eduyears-18)
#bmi
share$bmi16_35 <- ifelse(share$bmi <=35, share$bmi ,35)
share$bmi35 <- ifelse(share$bmi<=35, 0, share$bmi-35)

logit2 <- glm(formula = stayshealthy ~ smoking + ever_smoked + female + age + eduyears0_8 + eduyears8_18 + eduyears18 + 
                income10 + bmi16_35 + bmi35 + exerc + as.factor(country), data=share, family='binomial')

logit_marg2 <- margins(logit2)
summary(logit_marg2)

##
# Probit coefficients
probit <- glm(stayshealthy ~ smoking + ever_smoked + female + age + lspline(eduyears, c(8,18)) + 
                income10 + lspline(bmi, c(35)) + exerc + as.factor(country), data=share, family=binomial(link="probit"))
summary(probit)

# predicted probabilities 
share$pred_probit<- predict.glm(probit, type="response") 
summary(share$pred_probit)

# probit marginal differences
probit_marg <- probitmfx(formula = stayshealthy ~ smoking + ever_smoked + female + age + eduyears0_8 + eduyears8_18 + eduyears18 + 
                           income10 + bmi16_35 + bmi35 + exerc + as.factor(country), data=share, atmean=FALSE)
print(probit_marg)

###
# Creating a model summary output with msummary
cm <- c('(Intercept)' = 'Constant')
pmodels <- list(lpm, logit, logit_marg, probit, probit_marg)
msummary( pmodels ,
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|R2|PseudoR2',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         coef_omit = 'as.factor(country)*',
         output = paste0(w_dir,"prob_models_coeff.html")
)

# adding pseudo R2 (not work for mfx)
glance_custom.glm <- function(x) data.frame(`PseudoR2` = pR2(x)["McFadden"])
cm <- c('(Intercept)' = 'Constant')
msummary(list(lpm, logit, probit),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         coef_omit = 'as.factor(country)*'
)

##
# TO DO:
# Comparing predicted probabilities of logit and probit to LPM
ggplot(data = share) +
  geom_point(aes(x=pred_lpm, y=pred_probit, color="Probit"), size=1,  shape=16) +
  geom_point(aes(x=pred_lpm, y=pred_logit,  color="Logit"), size=1,  shape=16) +
  geom_line(aes(x=pred_lpm, y=pred_lpm,    color="45 degree line"), size=1) +
  labs(x = "Predicted probability of staying healthy (LPM)", y="Predicted probability")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous(expand = c(0.00,0.0), limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_color_manual(name = "", values=c("green", "red","blue"))+
  theme(legend.position=c(0.55,0.08),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4))

# Also can use stargazer...
#stargazer(list(lpm, logit, probit), digits=3, out=paste(w_dir,"out/T11_reg3_R.html",sep=""))



####
# 5. PART - GOODNESS OF FIT
#

# re-estimate the simplest lpm
lpmbase <- lm(stayshealthy ~ smoking, data=share)
share$pred_lpmbase <- predict(lpmbase) 


# DISTRIBUTION OF PREDICTED PROBABILITIES BY OUTCOME
# LPM simple model
ggplot(data = share,aes(x=pred_lpmbase)) + 
  geom_histogram(data=subset(share[share$stayshealthy == 1, ]), 
                 aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(share[share$stayshealthy == 0, ]), 
                 aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Did not stay healthy","Stayed healthy")) +
  scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Did not stay healthy","Stayed healthy")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,80), breaks = seq(0,80,20))+
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))


# LPM rich model
ggplot(data = share,aes(x=pred_lpm)) + 
  geom_histogram(data=subset(share[share$stayshealthy == 1, ]), 
                 aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(share[share$stayshealthy == 0, ]), 
                 aes(fill=as.factor(stayshealthy), color=as.factor(stayshealthy), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Did not stay healthy","Stayed healthy")) +
  scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Did not stay healthy","Stayed healthy")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,20), breaks = seq(0,20,4)) +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))


#####
# Summary statistics on predicted probabilities:
#
# TO DO:
#   Create a CONDITIONAL sum_stat on stayhealth for:
#     "pred_lpmbase","pred_lpm","pred_logit","pred_probit" 
#   use: "mean","median","min","max","sd"
#
#   Hint: you may do two tables for staying healthy and not staying healty and use sum_stat on those
#  
ss_1 <- subset( share , share$stayshealthy==1 )
ss_0 <- subset( share , share$stayshealthy==0 )

ss_1s <- sum_stat(ss_1,c("pred_lpmbase","pred_lpm","pred_logit","pred_probit"),
                  c("mean","median","min","max","sd"),num_obs = F)
ss_0s <- sum_stat(ss_0,c("pred_lpmbase","pred_lpm","pred_logit","pred_probit"),
                  c("mean","median","min","max","sd"),num_obs = F)
ss_1s
ss_0s


rm(logit, logit_marg, logit_marg2, logit2, lpm, lpmbase, probit, probit_marg,
   ss_1 , ss_0 , ss_1s, ss_0s )

###
# Bias and Calibration curve
#
# Lets use the logit model!
#
# Biased prediction? Calculate bias!
#   Hint: bias = mean(prediction) - mean(actual)
mean_pred_logit <- mean( share$pred_logit )
bias <- mean_pred_logit - mean( share$stayshealthy )
# Not really... it is really tiny!


# Note dplyr:: is important to specify which package's 'select' is used!
actual_vs_predicted <- share %>%
                        ungroup() %>% 
                        dplyr::select(actual = stayshealthy, 
                                       predicted = pred_logit) 
num_groups <- 10

calibration_d <- actual_vs_predicted %>%
      mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
      group_by(predicted_score_group) %>%
      dplyr::summarise(mean_actual = mean(actual), 
                       mean_predicted = mean(predicted), 
                       num_obs = n())

g1 <- ggplot( calibration_d,aes(x = mean_actual, y = mean_predicted)) +
      geom_point( color='red', size=1.5, alpha=0.8) +
      geom_line(  color='red', size=1  , alpha=0.8) +
      geom_abline( intercept = 0, slope = 1, color='blue') +
      labs( x = "Actual event probability", y = "Predicted event probability") +
      scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1)) +
      scale_y_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1,0.1))
g1


# To do: create a function which creates a graphs!
# Inputs:
#   dataset, outcome, pred_prob_name, num_groups
# Output:
#   bias, calibration_d, and make a graph 


##
# 7. PART - CONFUSION TABLES
#
# Handy for classification!
#   will learn more on DA3

# Create a new data_frame just for simplicity!
df <- data.frame(share$pred_lpmbase, share$pred_lpm, share$pred_logit, share$pred_probit)

# Set the treshold value
threshold <- 0.5

# Decide for each observations and each prediction, if larger than the treshold value!
for (i in 1:nrow(df)) {
  for (j in 1:ncol(df)) {
    if (df[i,j]>threshold) {df[i,j]=1}
    else {df[i,j]=0}
  }
}

# confusion matrix - does it seems similar?
for (j in 1:ncol(df)){
  print(prop.table(table(df[, j], share$stayshealthy)))
}

rm(df, i, j)
