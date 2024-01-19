
# Create empty list to fill in later
desclist4 <- list()

# Loop over FBs (named lists)
for (i in names(fblist_num4)) {
  FBnames <- fblist_num4[[i]]

  # Create a new list element in desclist for each FB
  desclist4[[i]] <- list()

  # Loop over named dfs inside FBs
  for (j in names(FBnames)) {

    # select variables with "WISS_vor"
    Wn <- grep("^WISS_nac", names(fblist_num4[[i]][[j]]), value = TRUE)

    # Loop over selected Wn variables
    for (k in seq_along(Wn)) {
      Wn2 <- Wn[k]

      # When found, use psych::describe() for descriptives on it
      if (length(Wn) > 0) {
        Wn2_name <- paste0(Wn2)
        desclist4[[i]][[Wn2_name]] <- psych::describe(fblist_num4[[i]][[j]][[Wn2]])
      }
    }
  }
}

## generate empty df to fill in later
df_trend_Wn <- data.frame(FB = character(),
                          Item = character(),
                          N_Wv = numeric(),
                          M_Wv = numeric(),
                          SD_Wv = numeric(),
                          stringsAsFactors = FALSE)


# Loop over each list element in desclist4
for (i in seq_along(desclist4)) {
  FBname <- names(desclist4)[i]
  FBdf_list <- desclist4[[i]]

  # Loop over each named df in the list element
  for (j in seq_along(FBdf_list)) {
    Wn_df <- FBdf_list[[j]]
    Wn_name <- names(FBdf_list)[j]

    # Extract columns from the df and add it to the results
    mean_col <- Wn_df[["mean"]]
    sd_col <- Wn_df[["sd"]]
    n_col <- Wn_df[["n"]]
    se_col <- Wn_df[["se"]]
    df_trend_Wn <- rbind(df_trend_Wn, data.frame(FBname = FBname,
                                                 Item = Wn_name,
                                                 M_Wn = mean_col,
                                                 SD_Wn = sd_col,
                                                 N_Wn = n_col,
                                                 stringsAsFactors = FALSE))
  }
}








#####################

describe_variables <- function(fblist, variable_name) {

  # Create empty list to fill in later
  desclist <- list()

  # Loop over FBs (named lists)
  for (i in names(fblist)) {
    FBnames <- fblist[[i]]

    # Create a new list element in desclist for each FB
    desclist[[i]] <- list()

    # Loop over named dfs inside FBs
    for (j in names(FBnames)) {

      # select variables with the specified variable name
      selected_vars <- grep(paste0("^", variable_name), names(fblist[[i]][[j]]), value = TRUE)

      # Loop over selected variables
      for (k in seq_along(selected_vars)) {
        var_name <- selected_vars[k]

        # When found, use psych::describe() for descriptives on it
        if (length(selected_vars) > 0) {
          var_name_desc <- paste0(var_name, "_desc")
          desclist[[i]][[var_name_desc]] <- psych::describe(fblist[[i]][[j]][[var_name]])
        }
      }
    }
  }

  # generate empty df to fill in later
  df <- data.frame(FB = character(),
                   Item = character(),
                   N = numeric(),
                   M = numeric(),
                   SD = numeric(),
                   stringsAsFactors = FALSE)

  # Loop over each list element in desclist
  for (i in seq_along(desclist)) {
    FBname <- names(desclist)[i]
    FBdf_list <- desclist[[i]]

    # Loop over each named df in the list element
    for (j in seq_along(FBdf_list)) {
      var_df <- FBdf_list[[j]]
      var_name_desc <- names(FBdf_list)[j]

      # Extract columns from the df and add it to the results
      mean_col <- var_df[["mean"]]
      sd_col <- var_df[["sd"]]
      n_col <- var_df[["n"]]
      se_col <- var_df[["se"]]
      df <- rbind(df, data.frame(FB = FBname,
                                 Item = var_name_desc,
                                 N = n_col,
                                 M = mean_col,
                                 SD = sd_col,
                                 stringsAsFactors = FALSE))

    }
  }
  # Add suffix to N, M, and SD column names
  new_names <- c("Item", "N", "M", "SD")
  new_names_with_suffix <- paste0(new_names, "_", variable_name)
  colnames(df)[which(names(df) %in% new_names)] <- new_names_with_suffix

  # Reorder columns so that FB and Item come first
  df <- df[, c("FB", new_names_with_suffix)]

  return(df)

}


df_WISS_vor <- describe_variables(fblist_num4, "WISS_vor")
View(df_WISS_vor)
df_WISS_nac <- describe_variables(fblist_num4, "WISS_nac")
View(df_WISS_nac)
df_WISS_ver <- describe_variables(fblist_num4, "WISS_ver")
View(df_WISS_ver)
