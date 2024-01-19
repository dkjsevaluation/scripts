
df_var2 <- df_var[c(5,6),]

vars_list <- list()

for (i in 1:nrow(df_var)) {
  # Get the name from column V1
  name <- df_var[i, 1]
  print(name)
  # Find the corresponding nested list in fblist_num3
  sublist <- fblist_num3[[name]]
  # Check if the sublist exists
  if (is.null(sublist)) {
    message(paste0("No sublist found for ", name))
  }
  # Get the variable name from column V2
  V2 <- df_var[i, 2]
  print(V2)
  # Loop through each data frame in the sublist
  for (j in 1:length(sublist)) {
    # Check if the data frame contains the variable from column V2
    if (V2 %in% names(sublist[[j]])) {
      # Extract the variable and add it to the list with a name based on V2 and V1
      var <- sublist[[j]][[V2]]
      new_length <- 500
      num_nas <- new_length - length(var)
      length(var) <- new_length
      var[(length(var) - num_nas + 1):length(var)] <- NA
      var_name <- paste0(df_var[i, "V1"], "_", V2, "_", j)
      vars_list[[var_name]] <- var
      V2 <- NULL
    }
  }
}


View(df_WISS_nac)
View(freq_wiss_nac)

df_name <- df_WISS_nac


  df_var <- as.data.frame(cbind(df_name[,1], df_name[,2]))
  colnames(df_var) <- c("V1", "V2")
  # Remove the "_desc" suffix from the V2 column
  df_var[, "V2"] <- gsub("_desc$", "", df_var[, "V2"])

   # Create an empty list to hold the extracted variables
  vars_list <- list()

  # Loop through each row in df_Wissvar
  for (i in 1:nrow(df_var[4,])) {

    # Get the name from column V1
    name <- df_var[i, 1]
    # Find the corresponding nested list in fblist_num3
    sublist <- fblist_num3[[name]]
    # Check if the sublist exists
    if (is.null(sublist)) {
      message(paste0("No sublist found for ", name))
    }
    # Get the variable name from column V2
    V2 <- df_var[i, 2]
    # Check if V2 exists in the nested list
    if (!(V2 %in% names(sublist[[1]]))) {
      message(paste0("No variable named ", V2, " in sublist ", name))
    }
    # Loop through each data frame in the sublist
    for (j in 1:length(sublist)) {
      # Check if the data frame contains the variable from column V2
      if (grep(V2, names(sublist[[j]]), fixed = TRUE)) {
        # Extract the variable and add it to the list with a name based on V2 and V1
        var <- sublist[[j]][[V2]]
        new_length <- 500
        num_nas <- new_length - length(var)
        length(var) <- new_length
        var[(length(var) - num_nas + 1):length(var)] <- NA
        var_name <- paste0(df_var[i, "V1"], "_", V2, "_", j)
        vars_list[[var_name]] <- var
      }
    }
  }
  return(vars_list)
}


var <- sublist[[2]][[V2]]
new_length <- 500
num_nas <- new_length - length(var)
length(var) <- new_length
var[(length(var) - num_nas + 1):length(var)] <- NA


if (!(V2 %in% names(sublist[[2]]))) {
  message(paste0("No variable named ", V2, " in sublist ", name))
}
