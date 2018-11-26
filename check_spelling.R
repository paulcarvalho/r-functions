### AUTHOR: Paul Carvalho    #####
### Date created: 03/15/2018 #####

# DESCRIPTION: Check for similar spelling within character or factor vector and run through user interaction 
#              to make changes
# INPUTS:  df_in                = dataframe containing character or factor vector to check spelling.
#          var_name             = bare parameter name indicating the vector to check spelling.
#          df_in_1              = second data frame with additional character vector.
#          var_name_1           = bare parameter name indicating the vector to add to the first vector.
#          distance_sensitivity = positive integer indicating approximate string distance between characters in char
#                                 list.
# OUTPUTS: updated dataframe

# Libraries ______________________________________________________________________________________________________
library(rlang)
library(dplyr)

# Check spelling of vector _______________________________________________________________________________________
check.spelling = function(df_in, var_name, df_in_1=NULL, var_name_1=NULL, distance_sensitivity){
  # turn bare args into quosures
  quo_var <- enquo(var_name)
  if(is.null(var_name_1) == FALSE){
    quo_var_1 <- enquo(var_name_1)  
  }
  
  # quo_text(quo_var) gets the string of the column name from bare parameter input
  char.list <- as.character(df_in[[quo_text(quo_var)]])
  if(is.null(var_name_1) == FALSE){
    char.list.1 <- as.character(df_in_1[[quo_text(quo_var_1)]])
  }

  # Remove na's from the list of characters
  char.list <- na.omit(char.list)
  if(is.null(var_name_1) == FALSE){
    char.list.1 <- na.omit(char.list.1)
  }

  # check spelling and obtain a list of words that have similar spelling
  x <- unique(char.list)
  df_list <- data_frame(x=x, name=rep("df_1",length(x)))
  # Only run this code if there is a second dataframe
  if(is.null(df_in_1) == FALSE){
    y <- unique(char.list.1)
    df_x <- data_frame(x=x, name=rep("df_1",length(x)))
    df_y <- data_frame(x=y, name=rep("df_2",length(y)))
    df_list <- rbind(df_x,df_y)
  }
  
  # Need to run this nested for loop to get the length of the data frame
  tmp_count <- 0
  for(i in 1:length(df_list$x)){
    for(j in i:length(df_list$x)){
      dist = adist(df_list$x[i],df_list$x[j])
      if(dist > 0 && dist <= distance_sensitivity){
        tmp_count <- tmp_count+1
      }
    }
  }
  df_dist = data.frame(name_1=character(length=tmp_count),
                       df_name_1=character(length=tmp_count),
                       name_2=character(length=tmp_count),
                       df_name_2=character(length=tmp_count),
                       distance=numeric(length=tmp_count))
  # need to make sure these vectors are characters and not factors
  df_dist$name_1 <- as.character(df_dist$name_1)
  df_dist$df_name_1 <- as.character(df_dist$df_name_1)
  df_dist$name_2 <- as.character(df_dist$name_2)
  df_dist$df_name_2 <- as.character(df_dist$df_name_2)

  count = 0
  for(i in 1:length(df_list$x)){
    for(j in i:length(df_list$x)){
      dist = adist(df_list$x[i],df_list$x[j])
      if(dist > 0 && dist <= distance_sensitivity){
        count <- count+1
        # print(df_dist$name_1[count])
        # print(df_list$x[i])
        df_dist$name_1[count]    <- df_list$x[i]
        df_dist$name_2[count]    <- df_list$x[j]
        df_dist$df_name_1[count] <- df_list$name[i]
        df_dist$df_name_2[count] <- df_list$name[j]
        df_dist$distance[count]  <- dist
        
      }
    }
  }

  # create a new list that omits repeated pairs
  tmp_name_1 <- NULL
  tmp_df_name_1 <- NULL
  tmp_name_2 <- NULL
  tmp_df_name_2 <- NULL
  tmp_distance <- NULL
  # refined_df_dist <- data.frame(name_1=character(),
  #                               df_name_1=character(),
  #                               name_2=character(),
  #                               df_name_2=character(),
  #                               distance=numeric())

  
  count.1 = 0
  for(k in 1:length(df_dist$name_1)){
    if(k == 1){
      count.1 <- count.1+1
      tmp_name_1[count.1]    <- df_dist$name_1[k]
      tmp_name_2[count.1]    <- df_dist$name_2[k]
      tmp_df_name_1[count.1] <- df_dist$df_name_1[k]
      tmp_df_name_2[count.1] <- df_dist$df_name_2[k]
      tmp_distance[count.1]  <- df_dist$distance[k]

    } else {
      flag = 0
      for(m in 1:length(tmp_name_1)){
        if(df_dist$name_1[k] == tmp_name_1[m] && df_dist$name_2[k] == tmp_name_2[m]){
          flag = 1
        } else if (df_dist$name_1[k] == tmp_name_2[m] && df_dist$name_2[k] == tmp_name_1[m]){
          flag = 1
        }
      }
      if(flag == 0){
        count.1 = count.1+1
        tmp_name_1[count.1]    <- df_dist$name_1[k]
        tmp_name_2[count.1]    <- df_dist$name_2[k]
        tmp_df_name_1[count.1] <- df_dist$df_name_1[k]
        tmp_df_name_2[count.1] <- df_dist$df_name_2[k]
        tmp_distance[count.1]  <- df_dist$distance[k]

      }
    }
  }
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
    }

    # for(i in 1:(length(sorted.final.list[,1]))){
    #   cat("\n1.\"", as.character(sorted.final.list$spelling.1[i]),"\"", "   2.\"", as.character(sorted.final.list$spelling.2[i]),"\"\n", sep="")
    #   cat("Distance =", as.character(sorted.final.list$distance[i]))
    #   cat("\n1. If spelling 1 is correct")
    #   cat("\n2. If spelling 2 is correct")
    #   cat("\n3. If both incorrect and you want to enter the correct spelling")
    #   cat("\n4. If both correct or you do not want to make any substitutions")
    #   val <- eval(parse(text=readline(prompt="Enter option: ")))
    #   # Save correct and incorrect spelling
    #   if(val==1){
    #     correct.sp    <- sorted.final.list$spelling.1[i]
    #     incorrect.sp1 <- sorted.final.list$spelling.2[i]
    #     # substitute spelling and adjust factor levels
    #     df_in[[quo_text(quo_var)]] <- gsub(incorrect.sp1, correct.sp, df_in[[quo_text(quo_var)]])
    #   } else if(val==2) {
    #     correct.sp    <- sorted.final.list$spelling.2[i]
    #     incorrect.sp1 <- sorted.final.list$spelling.1[i]
    #     # substitute spelling and adjust factor levels
    #     df_in[[quo_text(quo_var)]] <- gsub(incorrect.sp1, correct.sp, df_in[[quo_text(quo_var)]])
    #   } else if(val==3){
    #     correct.sp <- readline(prompt="Enter correct spelling: ")
    #     incorrect.sp1 <- sorted.final.list$spelling.1[i]
    #     incorrect.sp2 <- sorted.final.list$spelling.2[i]
    #     # substitute spelling and adjust factor levels
    #     df_in[[quo_text(quo_var)]] <- gsub(incorrect.sp1, correct.sp, df_in[[quo_text(quo_var)]])
    #     df_in[[quo_text(quo_var)]] <- gsub(incorrect.sp2, correct.sp, df_in[[quo_text(quo_var)]])
    #   } else if(val==4){
    #     # do nothing
    #   } else {
    #     cat("\nInvalid option")
    #   }
    # } # End of interactive for loop
  }

  return(df_in)
} # End of function











