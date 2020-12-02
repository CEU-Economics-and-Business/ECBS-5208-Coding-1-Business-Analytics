#################################
# Health and smoking, using     #
# easySHARE dataset             #
#                               #
#   cleaning and data wrangling #
#################################


##
# 1) Download data from: https://releases.sharedataportal.eu/releases
#   a) Search for: easySHARE Release 7.1.0
#   b) Download: easySHARE_rel7-1-0_Stata.zip (17.41MB)
#     NOT "easySHARE_rel7_1_0_R.zip (17.11MB)"
#     I want to show how to call .dta files...


# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(haven)
library(tidyverse)

# CHECK WORKING DIRECTORY - CHANGE IT TO YOUR WORKING DIRECTORY
dir <- "/Users/agostonreguly/Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_10/data/"


# load in raw data and save in csv
share <- haven::read_stata(paste0(dir, "raw/easySHARE_rel7-1-0.dta"))

# Rename br015_ to br015 (Sports or activities that are vigorous)
names(share)[names(share) == 'br015_'] <- 'br015'

# Select the necessary subset:
share <- subset(share, select=c(mergeid, wave, country, country_mod, int_year, 
                                int_month, female, age, eduyears_mod, sphus, 
                                br015, smoking, ever_smoked, income_pct_w4, 
                                bmi, mar_stat))

# Write to clean folder
write.csv(share, paste0(dir, "clean/share-health.csv"), row.names = F)
