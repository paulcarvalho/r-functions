################################################################################################################
# 
# AUTHOR: Paul Carvalho    
# DATE CREATED: 03/15/2018
# DATE MODIFIED: 10/28/2019 
#
# DESCRIPTION: Check for similar spelling within, or between, character or factor vectors and run through user 
#              interaction to make spelling corrections.              
#
# INPUTS:  
#   df_1: dataframe containing character or factor vector to check spelling.
#   var_1: bare parameter name (ie, without quotes) indicating the vector to check spelling.
#   df_2: second dataframe with additional character vector (optional).
#   var_2: bare parameter name (ie, without quotes) indicating the vector to add to the first vector (optional).
#   distance_sensitivity: positive integer indicating approximate string distance between characters in char
#                         vector (see ?adist for more details). Values 1 to 3 should be sufficient to catch  
#                         spelling mistakes.
#
# OUTPUTS: updated dataframe with spelling corrections.
#
################################################################################################################

# Libraries
install.packages("rlang")
install.packages("dplyr")
library(rlang)
library(dplyr)

# Function
check.spelling <- function(df_1, var_1, df_2 = NULL, var_2 = NULL, distance_sensitivity){
  
  # turn bare inputs into quosures
  quo_var <- enquo(var_1)
  if(is.null(df_2) == FALSE){
    quo_var_1 <- enquo(var_2)  
  }
  
  # quo_text(quo_var) gets the string of the column name from bare parameter input
  char.list <- as.character(df_1[[quo_text(quo_var)]])
  if(is.null(df_2) == FALSE){
    char.list.1 <- as.character(df_2[[quo_text(quo_var_1)]])
  }

  # Remove na's from the list of characters
  char.list <- na.omit(char.list)
  if(is.null(df_2) == FALSE){
    char.list.1 <- na.omit(char.list.1)
  }

  # check spelling and create a list of words that have similar spelling
  x <- unique(char.list)
  df_list <- data_frame(x = x, name=rep("df_1",length(x)))
  
  # If there is a second dataframe, add unique spelling to dataframe
  if(is.null(df_2) == FALSE){
    y <- unique(char.list.1)
    df_x <- data_frame(x = x, name = rep("df_1",length(x)))
    df_y <- data_frame(x = y, name = rep("df_2",length(y)))
    df_list <- rbind(df_x,df_y)
  }
  
  # Need to run this nested for loop to get the length of the dataframe
  tmp_count <- 0
  for(i in 1:length(df_list$x)){
    for(j in i:length(df_list$x)){
      dist <- adist(df_list$x[i], df_list$x[j])
      if(dist > 0 && dist <= distance_sensitivity){
        tmp_count <- tmp_count + 1
  }}}
  
  # create an empty dataframe shell of the correct length
  df_dist <- data.frame(name_1 = character(length=tmp_count),
                        df_name_1 = character(length=tmp_count),
                        name_2 = character(length=tmp_count),
                        df_name_2 = character(length=tmp_count),
                        distance = numeric(length=tmp_count))

  # create data frame with list of similar spelling
  count <- 0
  for(i in 1:length(df_list$x)){
    for(j in i:length(df_list$x)){
      dist <- adist(df_list$x[i], df_list$x[j])
      if(dist > 0 && dist <= distance_sensitivity){
        count <- count + 1
        df_dist$name_1[count] <- df_list$x[i]
        df_dist$name_2[count] <- df_list$x[j]
        df_dist$df_name_1[count] <- df_list$name[i]
        df_dist$df_name_2[count] <- df_list$name[j]
        df_dist$distance[count] <- dist
  }}}

  # create a new list that omits repeated pairs
  tmp_name_1 <- NULL
  tmp_df_name_1 <- NULL
  tmp_name_2 <- NULL
  tmp_df_name_2 <- NULL
  tmp_distance <- NULL
  
  # iterate through the dataframe and remove repeated pairs
  count_1 <- 0
  for(k in 1:length(df_dist$name_1)){
    if(k == 1){
      count_1 <- count_1 + 1
      tmp_name_1[count_1] <- df_dist$name_1[k]
      tmp_name_2[count_1] <- df_dist$name_2[k]
      tmp_df_name_1[count_1] <- df_dist$df_name_1[k]
      tmp_df_name_2[count_1] <- df_dist$df_name_2[k]
      tmp_distance[count_1] <- df_dist$distance[k]
    } else {
      flag <-  0
      for(m in 1:length(tmp_name_1)){
        if(df_dist$name_1[k] == tmp_name_1[m] && df_dist$name_2[k] == tmp_name_2[m]){
          flag <- 1
        } else if (df_dist$name_1[k] == tmp_name_2[m] && df_dist$name_2[k] == tmp_name_1[m]){
          flag <- 1
        }
      }
      if(flag == 0){
        count_1 <- count_1 + 1
        tmp_name_1[count_1] <- df_dist$name_1[k]
        tmp_name_2[count_1] <- df_dist$name_2[k]
        tmp_df_name_1[count_1] <- df_dist$df_name_1[k]
        tmp_df_name_2[count_1] <- df_dist$df_name_2[k]
        tmp_distance[count_1] <- df_dist$distance[k]

  }}}
  
  # create a refined list without repeated pairs in similar spelling
  refined_df_dist <- data.frame(name_1=tmp_name_1,
                                df_name_1=tmp_df_name_1,
                                name_2=tmp_name_2,
                                df_name_2=tmp_df_name_2,
                                distance=tmp_distance)
  list.order <- order(refined_df_dist$distance)
  refined_df_dist <- refined_df_dist[list.order,]

  # If function does not detect differences in spelling, then do nothing
  if(is.na(refined_df_dist$name_1[1])){
    cat("No differences detected for specified distance sensitivity!")

  # If differences detected, then enter interactive loop
  } else {
    # User interaction to replace spelling
    for(i in 1:(length(refined_df_dist$name_1))){
      cat("\n1.\"", as.character(refined_df_dist$name_1[i]),"\"","[",as.character(refined_df_dist$df_name_1[i]),"]",
          "   2.\"", as.character(refined_df_dist$name_2[i]),"[",as.character(refined_df_dist$df_name_2[i]),"]","\"\n", sep="")
      cat("Distance =", as.character(refined_df_dist$distance[i]))
      cat("\n1. If spelling 1 is correct")
      cat("\n2. If spelling 2 is correct")
      cat("\n3. If both incorrect and you want to enter the correct spelling")
      cat("\n4. If both correct or you do not want to make any substitutions")
      val <- eval(parse(text=readline(prompt="Enter option: ")))
      # Save correct and incorrect spelling
      if(val==1){
        correct.sp    <- refined_df_dist$name_1[i]
        incorrect.sp1 <- refined_df_dist$name_2[i]
        # substitute spelling and adjust factor levels - no changes will be made to df_2
        df_1[[quo_text(quo_var)]] <- gsub(incorrect.sp1, correct.sp, df_1[[quo_text(quo_var)]])
      } else if(val==2) {
        correct.sp    <- refined_df_dist$name_2[i]
        incorrect.sp1 <- refined_df_dist$name_1[i]
        # substitute spelling and adjust factor levels
        df_1[[quo_text(quo_var)]] <- gsub(incorrect.sp1, correct.sp, df_1[[quo_text(quo_var)]])
      } else if(val==3){
        correct.sp <- readline(prompt="Enter correct spelling: ")
        incorrect.sp1 <- refined_df_dist$name_1[i]
        incorrect.sp2 <- refined_df_dist$name_2[i]
        # substitute spelling and adjust factor levels
        df_1[[quo_text(quo_var)]] <- gsub(incorrect.sp1, correct.sp, df_1[[quo_text(quo_var)]])
        df_1[[quo_text(quo_var)]] <- gsub(incorrect.sp2, correct.sp, df_1[[quo_text(quo_var)]])
      } else if(val==4){
        # do nothing
      } else {
        cat("\nInvalid option")
  }}}

  return(df_1)
}










