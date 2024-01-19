library(dplyr)
library(psych)
library(splithalfr)
library(ggplot2)
#library(tidyr)
library(patchwork)
library(stringr)
library(dkjslama)

###########################
############ Get Data #####
###########################

###create empty list for fbdata
fblist2 <- list()

### get vector with fbnames
fbnames <- c("Evaluation_JbK_vorher", "Evaluation_Schulerfolgdigital_retro", "TechnovationGirls01", "TechnovationGirls02")

## get Lamapoll data for every fbname and put it into list fbdata with fbname as name of df
for (i in fbnames) {
  df <- as.data.frame(get.lamapoll.data(i, "we@dkjs.de", "wGl5v4fz"))
  df <- df %>%
    mutate(across(where(is.integer), as.numeric))
  fblist2[[i]] <- df
}


#### to do: make a df, in which the variable names and variable descriptions (full questions) are matched for each df
## check whether API can do that and implement into package {lamapoll} as function


##################################
############ Variable selection ##
##################################

### use only numeric variables
# done: CODE muss drin bleiben
fblist2_num <- list()
for (j in 1:length(fblist2)) {
  df_num <- fblist2[[j]] %>% select(where(is.numeric) | matches("CODE"))
  fblist2_num[[length(fblist2_num) + 1]] <- df_num
  
  print(paste("Number of variables in df", j, ": ", ncol(fblist2[[j]])))
  print(paste("Number of variables in df_num", j, ": ", ncol(df_num)))
}
names(fblist2_num) <- names(fblist2)


##  CODE Groß- und Kleinschreibung vereinheitlichen
fblist2_num <- lapply(fblist2_num, function(df) {
  # identify the relevant variables
  var_names <- names(df)[grepl("^CODE", names(df))]
  
  # convert uppercase characters to lowercase in each relevant variable
  for (var_name in var_names) {
    df[[var_name]] <- tolower(df[[var_name]])
  }
  
  # return the modified data frame
  return(df)
})


##################################
############ Scale Building ######
##################################

#### this requires building a filter system that applies different ways of calculating scales and reliability
#### depending on the first two prefixes in the variable name as well as the "_s1" part of the variable name.
#### lets build modules based on Tabelle Variablenbenennung!
#### in the end/at the beginning, loop through all modules for every df in fblist2_num

#create list to fill in
fblist2_num2 <- vector("list", length = length(fblist2_num))

#### Module 1 for WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM; Module 2 for ERWA; ...TN .... ZUFR

############### Module 1

###########################
#### Sort Items into scales
#### done: extend for ".KJ" at the end of a variable name
#### done: check for different "_s0", which should be identical for all items in a scale
#### to do: build different scales for ROLLE!!!! only needed for reliability calculation and report?
####  either: make a function with the role as input that translates it to regex
####  or: detect ROLLE occurrences and use it to build all scale variants?
####  neither! attatch scale to original df and use dyplr::group_by(Rolle), scrap the rolle-dependent reliability.

#create list to fill in
fblist2_num2 <- vector("list", length = length(fblist2_num))

# loop over the list with fb data
for (k in 1:length(fblist2_num)) {
  # Extract the part of the variable names without "_i1" etc.
  variable_names <- str_replace(names(fblist2_num[[k]]), "_i\\d+", "")
  variable_names <- str_replace(variable_names, "_\\.", ".")
  
  # Get only the unique values
  unique_variable_names <- unique(variable_names)
  
  # extract variable names for scales
  scalevars <- unique_variable_names[grep("^(WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM)_", unique_variable_names)]
  
  # Create empty list with names from scalevars
  scalevar_dfs <- vector("list", length = length(scalevars))
  names(scalevar_dfs) <- scalevars
  
  # Loop over each element in scalevars
  for (i in 1:length(scalevars)) {
    scalevarname <- scalevars[i]
    scalevarprefix <- sub("_.*", "", scalevarname)
    scalevarsuffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)
    
    matching_vars <- grep(paste0("^", scalevarprefix, ".*_", scalevarsuffix, "$"), names(fblist2_num[[k]]), value = TRUE)
    
    if (length(matching_vars) > 0) {
      scalevar_dfs[[i]] <- fblist2_num[[k]][, matching_vars, drop = FALSE]
      
      # extract suffix from scalevarname
      suffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)
      
      # filter variables in the current scalevar_dfs
      scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^.*_", suffix, "$"), names(scalevar_dfs[[i]])), drop = FALSE]
      
      # extract prefix from scalevarname
      prefix <- sub("(.*?_.*?_).*", "\\1", scalevarname)
      
      # filter variables in the current scalevar_dfs to keep only variables matching the prefix
      scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^", prefix, ".*"), names(scalevar_dfs[[i]])), drop = FALSE]
    }
  }
  
  # Select only those list elements that are data frames with at least two columns
  #scalevar_dfs2 <- scalevar_dfs[sapply(scalevar_dfs, is.list)]   # retired
  scalevar_dfs <- scalevar_dfs[sapply(scalevar_dfs, function(x) ncol(x) >= 2)]
  
  for (i in seq_along(scalevar_dfs)) {
    
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    
    # check if the data frame has more than two columns
    if (ncol(scalevar_dfs[[i]]) > 2) {
      
      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))
      
      #### to do: a for loop/an if clause to only do the following calculations for item bundles based on similar "_s1" etc
      
      # calculate the row means for the data frame and add as new column
      scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
        mutate(!!paste0("SK.", names(scalevar_dfs)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))
      
      # calculate reliability and attach as new column
      alpha <- tryCatch(
        psych::alpha(scalevar_dfs[[i]], check.keys = TRUE),
        error = function(e) NULL
      )
      
      if (!is.null(alpha)) {
        scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
          mutate(!!paste0("REL.", names(scalevar_dfs)[i]) := alpha %>% {. ->> tmp} %>% '[['(1) %>% pull(std.alpha) %>% .[1])
      } else {
        scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
          mutate(!!paste0("REL.", names(scalevar_dfs)[i]) := NA)
        next # skip to next iteration of for loop
      }
    }
    # check if the data frame has two columns
    if (ncol(scalevar_dfs[[i]]) < 3) {
      
      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))
      
      # calculate the row means for the data frame and add as new column
      scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
        mutate(!!paste0("SK.", names(scalevar_dfs)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))
      
      # calculate reliability and attach as new column
      scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
        mutate(
          !!paste0("REL.", names(scalevar_dfs)[i]) := tryCatch(
            spearman_brown(na.omit(.[,1]), na.omit(.[,2])),
            error = function(e) NA
          )
        )
      
      # scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      #  mutate(!!paste0("REL.", names(scalevar_dfs)[i]) := spearman_brown(na.omit(.[,1]), na.omit(.[,2])))
    }
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
  }
  
  # copy to prepared list fblist2_num2
  fblist2_num2[[k]] <- scalevar_dfs
  # Assign names to the list elements
  names(fblist2_num2)[k] <- paste0(names(fblist2_num[k]))
}

# replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
# Function to replace NaN values with NA
replace_nan_with_na <- function(df) {
  if (inherits(df, "data.frame")) {
    df[] <- lapply(df, function(x) ifelse(is.nan(x), NA, x))
  }
  return(df)
}

# Iterate over each data frame in the list
for (i in seq_along(fblist2_num2)) {
  fblist2_num2[[i]] <- replace_nan_with_na(fblist2_num2[[i]])
}


### nur berechnete SK und REL behalten und an original dfs ranpappen

combined_dfs_list <- list()

# Iterate through the list elements in fblist2_num2
for (list_name in names(fblist2_num2)) {
  list_element <- fblist2_num2[[list_name]]
  combined_df <- NULL
  
  # Iterate through the data frames within each list element
  for (df in list_element) {
    # Check if combined_df is NULL (first data frame)
    if (is.null(combined_df)) {
      combined_df <- df
    } else {
      # Use cbind to combine the data frames horizontally
      combined_df <- cbind(combined_df, df)
    }
  }
  
  # Add the combined data frame to the named list
  combined_dfs_list[[list_name]] <- combined_df
}

# Loop through each data frame in the list
for (i in 1:length(combined_dfs_list)) {
  df <- combined_dfs_list[[i]]
  
  # Use grep to select columns that match the pattern
  selected_columns <- grep("^SK\\.|^REL\\.", names(df), value = TRUE)
  
  # Subset the data frame to keep only the selected columns
  df <- df[, selected_columns]
  
  # Update the data frame in the list
  combined_dfs_list[[i]] <- df
}

# Initialize an empty list to store the combined data frames
combined_dfs_list2 <- list()

num_data_frames <- length(fblist2_num)

# Iterate through the data frames and combine them
for (i in 1:num_data_frames) {
  # Combine data frames based on their indexes
  df1_combined <- cbind(fblist2_num[[i]], combined_dfs_list[[i]])
  
  # Add the combined data frames to list_c
  combined_dfs_list2[[paste0("combined_", i)]] <- df1_combined
}

names(combined_dfs_list2) <- names(fblist2_num)


##### result is a df with all scales and their reliability
### now matching...
## first match, then split the list and apply above code on each list.


### then statistics
## bei überjährigen Datensätzen in der Excel einen Vermerk machen


