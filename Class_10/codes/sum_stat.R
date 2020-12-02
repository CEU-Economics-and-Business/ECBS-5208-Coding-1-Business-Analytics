#####
# This is a function which makes summary statistics 
# for many variables
##

# Inputs:
# df - tibble (data_frame) object
# var_names - string vector, containing variable names in df
# stats - string vector containing any of the following:
#     Central Tendency: mean, median, mode,
#     Quartiles: 1st_qu. (1st quartile), 3rd_qu (3rd quartile)
# num_obs - LOGICAL
#     if true, it gives the number of missing observations and the number of used observations
#     to calculate the statistics

##
# TASK: ADD THE FOLLOWING STATISTICS AS WELL:
#
#     Support: min, max
#     Dispersion: sd, var, range (max-min), iqr (inter-quartile range - 3rd quartile - 1st quartile)
#      
#
# Output:
# Tibble: in columns the variables
#         in rows the requested summary statistics (1st column contains the statistic's name)

sum_stat <- function( df , var_names , stats , num_obs = TRUE ){
  k <- length( var_names )
  built_in_stat <- c('mean','median','mode','min','max','1st_qu.','3rd_qu',
                     'sd','var','range','iqr')
  do_stat <- intersect( stats , built_in_stat )
  if ( is_empty(do_stat) ){
    stop('Error, no such statistics is implemented! Choose from: mean,median,min,max,1st_qu.,3rd_qu')
  }
  # By default add the number of missing observations and the number of used observations
  m <- length( do_stat )
  if ( num_obs ){
    do_stat <- c( do_stat , "# missing", "# used obs")
  }
  # Create tibble for output
  sum_stat <- as_tibble( matrix( 0 , nrow = m , ncol = k ) , name_repair = "unique" )
  for ( j in 1 : k ) {
    # Get the data for the j'th variable
    var_j <- df[ var_names[ j ] ]
    if ( num_obs ){
      # Count the missing values and add to statistics
      sum_stat[ m + 1 , j ] <- as.integer( sum( is.na( var_j ) ) )
      # Count observations used
      sum_stat[ m + 2 , j ] <- as.integer( sum( !is.na( var_j ) ) )
    }
    # Remove missing values
    var_j <- var_j[ !is.na( var_j ) ]
    # Name the sum_stat's column
    colnames( sum_stat )[ j ] <- var_names[ j ]
    for ( i in 1 : m ) {
      # Central tendency
      if (do_stat[ i ] == "mean"){
        sum_stat[[i,j]] <- mean( var_j )
      } else if (do_stat[ i ] == "median"){
        sum_stat[i,j] <- median( var_j )
      } else if (do_stat[ i ] == "mode"){
        sum_stat[i,j] <- mode( var_j )
      } 
      # Support
      else if (do_stat[ i ] == "min"){
        sum_stat[i,j] <- min( var_j )
      } else if (do_stat[ i ] == "max"){
        sum_stat[i,j] <- max( var_j )
      } 
      # Quartiles
      else if (do_stat[ i ] == "1st_qu."){
        sum_stat[i,j] <- quantile( var_j , probs = 0.25 )
      } else if (do_stat[ i ] == "3rd_qu"){
        sum_stat[i,j] <- quantile( var_j , probs = 0.75)
      } 
      # Dispersion
      else if (do_stat[ i ] == "sd"){
        sum_stat[i,j] <- sd( var_j )
      } else if (do_stat[ i ] == "var"){
        sum_stat[i,j] <- var( var_j )
      } else if (do_stat[ i ] == "range"){
        sum_stat[i,j] <- max( var_j ) - min( var_j )
      } else if (do_stat[ i ] == "iqr"){
        sum_stat[i,j] <- quantile( var_j , probs = 0.75) - quantile( var_j , probs = 0.25)
      } 
    }
  }
  # Finally add a column which contains the requested statistics and relocate to first position
  sum_stat <- sum_stat %>% 
    mutate( statistics = do_stat ) %>% 
    relocate( statistics )
  
  return( sum_stat )
}
