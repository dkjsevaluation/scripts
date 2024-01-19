
# get the scale types and differentiate mean scales and sum scales
library(stringr)
# load scale types
scale_types <-read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Skalenbildung_EvaFB.xlsx")
sum_scales <- scale_types %>% filter(str_detect(Berechnung, "Summe")) %>% filter(!is.na(Skala))
# clean from empty spaces and add underscore
sum_scale_suffixes <- str_replace_all(sum_scales$Skala, "\\s+", "")
sum_scale_suffixes <- str_replace_all(sum_scale_suffixes, "^", "_")

# test: sum_scale_suffixes <- c("_s14", "_s2")

# make an empty list to store scale-sorted data
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
          #scalevar_dfs2 <- scalevar_dfs[sapply(scalevar_dfs, is.list)]   # retired, not needed anymore
  scalevar_dfs <- scalevar_dfs[sapply(scalevar_dfs, function(x) ncol(x) >= 2)]
  
### trennen für summenskalen und mittelwerte
  scalevars_dfs1 <- list()
  scalevars_dfs2 <- list()
  
  # Loop through the names of data frames in scalevar_dfs
  for (df_name in names(scalevar_dfs)) {
    if (any(str_detect(df_name, paste0(".*(", paste(sum_scale_suffixes, collapse = "|"), ")\\D")))) {
      # If true, assign the data frame to scalevars_dfs1
      scalevars_dfs1[[df_name]] <- scalevar_dfs[[df_name]]
    } else {
      # If false, assign the data frame to scalevars_dfs2
      scalevars_dfs2[[df_name]] <- scalevar_dfs[[df_name]]
    }
  }  
  
  ### für summenskalen  
  
  for (i in seq_along(scalevars_dfs1)) {
    
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    
    # check if the data frame has more than two columns
    if (ncol(scalevars_dfs1[[i]]) > 2) {
      
      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))
      
      # calculate the row means for the data frame and add as new column
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs1)[i]) := rowSums(.[, 1:ncol(.)], na.rm = TRUE))
      
      # calculate reliability and attach as new column
      alpha <- tryCatch(
        psych::alpha(scalevars_dfs1[[i]], check.keys = TRUE),
        error = function(e) NULL
      )
      
      if (!is.null(alpha)) {
        scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs1)[i]) := alpha %>% {. ->> tmp} %>% '[['(1) %>% pull(std.alpha) %>% .[1])
      } else {
        scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs1)[i]) := NA)
        next # skip to next iteration of for loop
      }
    }
    # check if the data frame has two columns
    if (ncol(scalevars_dfs1[[i]]) < 3) {
      
      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))
      
      # calculate the row means for the data frame and add as new column
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs1)[i]) := rowSums(.[, 1:ncol(.)], na.rm = TRUE))
      
      # calculate reliability and attach as new column
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate(
          !!paste0("REL.", names(scalevars_dfs1)[i]) := tryCatch(
            spearman_brown(na.omit(.[,1]), na.omit(.[,2])),
            error = function(e) NA
          )
        )
      
      # scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
      #  mutate(!!paste0("REL.", names(scalevars_dfs1)[i]) := spearman_brown(na.omit(.[,1]), na.omit(.[,2])))
    }
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    scalevars_dfs1 <<- scalevars_dfs1
  }
  
  ### für mittelwertskalen  
  
  for (i in seq_along(scalevars_dfs2)) {
    
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    
    # check if the data frame has more than two columns
    if (ncol(scalevars_dfs2[[i]]) > 2) {
      
      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))
      
      # calculate the row means for the data frame and add as new column
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs2)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))
      
      # calculate reliability and attach as new column
      alpha <- tryCatch(
        psych::alpha(scalevars_dfs2[[i]], check.keys = TRUE),
        error = function(e) NULL
      )
      
      if (!is.null(alpha)) {
        scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs2)[i]) := alpha %>% {. ->> tmp} %>% '[['(1) %>% pull(std.alpha) %>% .[1])
      } else {
        scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs2)[i]) := NA)
        next # skip to next iteration of for loop
      }
    }
    # check if the data frame has two columns
    if (ncol(scalevars_dfs2[[i]]) < 3) {
      
      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))
      
      # calculate the row means for the data frame and add as new column
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs2)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))
      
      # calculate reliability and attach as new column
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate(
          !!paste0("REL.", names(scalevars_dfs2)[i]) := tryCatch(
            spearman_brown(na.omit(.[,1]), na.omit(.[,2])),
            error = function(e) NA
          )
        )
      
      # scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
      #  mutate(!!paste0("REL.", names(scalevars_dfs2)[i]) := spearman_brown(na.omit(.[,1]), na.omit(.[,2])))
    }
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    scalevars_dfs2 <<- scalevars_dfs2
  }
  
  ### coalesce scalevars_dfs1 und 2
  
  scalevar_dfs <- c(scalevars_dfs1, scalevars_dfs2)
  
### copy to prepared list fblist2_num2
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
