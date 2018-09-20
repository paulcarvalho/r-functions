### AUTHOR: Paul Carvalho    #####
### Date created: 03/15/2018 #####
### Version 2                #####

# DESCRIPTION: Check for similar spelling within character or factor vector and run through user interaction 
#              to correct spelling
# INPUTS:  df_in                = dataframe containing character or factor vector to check spelling.
#          var_name             = bare parameter name indicating the vector to check spelling. 
#          distance_sensitivity = positive integer indicating approximate string distance between characters in char
#                                 list.
# OUTPUTS: updated dataframe

# Libraries ______________________________________________________________________________________________________
library(rlang)
library(dplyr)

# Check spelling of vector _______________________________________________________________________________________
check.spelling = function(df_in, var_name, distance_sensitivity){
  # turn bare args into quosures
  quo_var <- enquo(var_name)
  
  # quo_text(quo_var) gets the string of the column name from bare parameter input
  char.list <- as.character(df_in[[quo_text(quo_var)]])
  
  # Remove na's from the list of characters
  char.list <- na.omit(char.list)
  
  # check spelling and obtain a list of words that have similar spelling
  x = unique(char.list)
  list.1 = NA
  list.2 = NA
  list.3 = NA
  count = 0
  for(i in 1:length(x)){
    for(j in i:length(x)){
      dist = adist(x[i],x[j])
      if(dist > 0 && dist <= distance_sensitivity){
        count = count+1
        list.1[count] = x[i]
        list.2[count] = x[j]
        list.3[count] = dist
      }
    }
  }
  full.list = cbind(list.1,list.2,list.3)
  refined.list.1 = NA
  refined.list.2 = NA
  refined.list.3 = NA
  count.1 = 0
  for(k in 1:length(full.list[,1])){
    if(k == 1){
      count.1 = count.1+1
      refined.list.1[count.1] = full.list[1,1]
      refined.list.2[count.1] = full.list[1,2]
      refined.list.3[count.1] = full.list[1,3]
    } else {
      flag = 0
      for(m in 1:length(refined.list.1)){
        if(full.list[k,1] == refined.list.1[m] && full.list[k,2] == refined.list.2[m]){
          flag = 1
        } else if (full.list[k,1] == refined.list.2[m] && full.list[k,2] == refined.list.1[m]){
          flag = 1
        }
      }
      if(flag == 0){
        count.1 = count.1+1
        refined.list.1[count.1] = full.list[k,1]
        refined.list.2[count.1] = full.list[k,2]
        refined.list.3[count.1] = full.list[k,3]
      }
    }
  }
  final.list = data.frame(spelling.1 = refined.list.1,
                          spelling.2 = refined.list.2,
                          distance = refined.list.3)
  list.order = order(final.list$distance)
  sorted.final.list = final.list[list.order,]
  
  # If function does not detect differences in spelling, then do nothing
  if(is.na(sorted.final.list$spelling.1[1])){
    cat("No differences detected for specified distance sensitivity!")
  
  # If differences detected, then enter interactive loop
  } else {
    # User interaction to replace spelling
    for(i in 1:(length(sorted.final.list[,1]))){
      cat("1.\"", as.character(sorted.final.list$spelling.1[i]),"\"", "   2.\"", as.character(sorted.final.list$spelling.2[i]),"\"\n", sep="")
      cat("Distance =", as.character(sorted.final.list$distance[i]))
      val <- eval(parse(text=readline(prompt="Enter number for correct spelling or click enter to manually insert correct spelling: ")))
      # Save correct and incorrect spelling
      if(length(val)==0 || val>2){
        correct.sp <- readline(prompt="Enter correct spelling: ")
        incorrect.sp1 <- sorted.final.list$spelling.1[i]
        incorrect.sp2 <- sorted.final.list$spelling.2[i]
      } else if(val == 1){
        correct.sp    <- sorted.final.list$spelling.1[i]
        incorrect.sp1 <- sorted.final.list$spelling.2[i]
      } else if(val == 2){
        correct.sp    <- sorted.final.list$spelling.2[i]
        incorrect.sp1 <- sorted.final.list$spelling.1[i]
      }
      # Replace spelling and adjust factor levels
      df_in[[quo_text(quo_var)]] <- gsub(incorrect.sp1, correct.sp, df_in[[quo_text(quo_var)]])
      if(length(val)==0 || val>2){
        df_in[[quo_text(quo_var)]] <- gsub(incorrect.sp2, correct.sp, df_in[[quo_text(quo_var)]])
      }
    } # End of interactive for loop
  } 

  return(df_in)
} # End of function











