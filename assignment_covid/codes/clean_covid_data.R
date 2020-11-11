########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 2            ##
## Clean   data       ##
########################


# Clear memory and call packages
rm(list=ls())
library(tidyverse)

# Read the raw files
my_path <- "Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/Codes_4_prep/Assignment_covid/data/"
# covid data
cv <- read_csv(paste0(my_path,'raw/covid_09_11_2020_raw.csv'))
# population data
pop <- read_csv(paste0(my_path,'raw/pop_WDI_2019.csv'))

####
# COVID DATA CLEANING
#
# Check covid data
glimpse( cv )

# Drop not needed variables
cv <- cv %>% select( -c( FIPS,Admin2,Last_Update,Lat,Long_,Combined_Key,Incidence_Rate,Case.Fatality_Ratio))

# One observation to be one country
# Check e.g. China:
cv %>% filter( Country_Region == 'China')

# Create new data table now only contains the countries
cv2 <- cv %>% 
  group_by( Country_Region ) %>% 
  summarise_if(is.numeric,lst( sum ) )

# Rename variables
cv2 <- cv2 %>% rename( country   = Country_Region ,
                       confirmed = Confirmed_sum,
                       death     = Deaths_sum,
                       recovered = Recovered_sum,
                       active    = Active_sum )

####
# Clean population data
#

## Check the observations:
# 1) Filter out grouping observations based on using digits
pop <- pop %>% filter( !grepl("[[:digit:]]", pop$iso2c) )

# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# 2nd drop specific values
drop_id <- c("EU","HK","OE")
pop <- pop %>% filter( !grepl( paste( drop_id , collapse="|"), pop$iso2c ) ) 

# 3rd drop values with certain starting char
# Get the first letter from iso2c
fl_iso2c <- substr(pop$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW")
# Filter out everything which starts X or Z except countries in retain_id
pop <- pop %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                          !grepl( paste( retain_id , collapse="|"), pop$iso2c ) ) ) 

rm( drop_id, fl_iso2c , retain_id )

# Retain and rename variables which are going to be used later
pop <-pop %>% transmute( country = country,
                         population=SP.POP.TOTL )


################
# MERGE the two data table
##


df <- full_join(cv2,pop)

# Correct some country names by hand
use_name <- c("Congo, Rep.","Congo, Dem. Rep.","Czech Republic","Korea, Rep.","Kyrgyz Republic",
              "Laos","St. Kitts and Nevis","St. Lucia","St. Vincent and the Grenadines",
              "Slovak Republic","United States","Myanmar")

alter_name <- c("Congo (Brazzaville)","Congo (Kinshasa)","Czechia","Korea, South","Kyrgyzstan",
                "Lao PDR","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines",
                "Slovakia","US","Burma")

# Simply use a for-cycle to change the name for the countries (note: ordering is important)
for ( i in seq_along( use_name ) ){
  df$country[ df$country == alter_name[ i ] ] <- use_name[ i ]
}

# Write a for-loop to find those which are partial or complete matches!
# 1) auxillary table for countries without any population value
aux <- df %>% filter( is.na(population) )
# 2) Get the name of the countries
countries_nm <- aux$country
# 3) Iterate through all potential partial matches
for ( i in seq_along( countries_nm ) ){
  # Select those observations where partial match exists
  log_select <- str_detect( df$country , countries_nm[ i ] )
  # Get the population values for partial matches
  c_partial <- df$population[ log_select ]
  # If there is a match: only two countries are selected and one is missing the other has population:
  if ( length( c_partial ) == 2 & sum( is.na( c_partial ) ) == 1 ){
    # Replace the missing value with the match
    df$population[ log_select & is.na(df$population)] = c_partial[ !is.na( c_partial ) ]
    # Remove the replaced variable
    df <- df %>% filter( !(log_select & is.na( df$confirmed ) ) )
  }
}

# 4) Check the results:
df %>% filter( is.na(population) )
# These are:
#   a) cruiser ships which stuck in national territory (Diamond Princess, MS Zaandam )
#   b) disputed territories which are accepted by covid statistics but not by world bank 
#       (Western Sahara, Taiwan or Kosovo)
#   c) we have no population data on them (Ertirea, Holy See (Vatican))

#####
# Handle missing values:
View( df %>% filter( !complete.cases(df) ) )
# Drop if population, confirmed cases or death is missing
df <- df %>% filter( !( is.na( population ) | is.na( confirmed ) | is.na( death ) ))


#####
# Save clean data
my_path <- "Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/Codes_4_prep/Assignment_covid/data/"
# COVID data
write_csv( df , paste0(my_path,'clean/covid_pop_09_11_2020_clean.csv'))




