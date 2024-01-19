#### make descriptives for all the variables and put them into lists

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
    Wv <- grep("^WISS_vor", names(fblist_num4[[i]][[j]]), value = TRUE)
    Wn <- grep("^WISS_nac", names(fblist_num4[[i]][[j]]), value = TRUE)
    Wver <- grep("^WISS_ver", names(fblist_num4[[i]][[j]]), value = TRUE)

    # Loop over selected Wv variables
    for (k in seq_along(Wv)) {
      Wv2 <- Wv[k]

      # When found, use psych::describe() for descriptives on it
      if (length(Wv) > 0) {
        Wv2_name <- paste0(Wv2)
        desclist4[[i]][[Wv2_name]] <- psych::describe(fblist_num4[[i]][[j]][[Wv2]])
      }
    }
    # Loop over selected Wn variables
    for (m in seq_along(Wn)) {
      Wn2 <- Wn[m]

      # When found, use psych::describe() for descriptives on it
      if (length(Wn) > 0) {
        Wn2_name <- paste0(Wn2)
        desclist4[[i]][[Wn2_name]] <- psych::describe(fblist_num4[[i]][[j]][[Wn2]])
      }
    }

    # Loop over selected Wver variables
    for (m in seq_along(Wver)) {
      Wver2 <- Wver[m]

      # When found, use psych::describe() for descriptives on it
      if (length(Wver) > 0) {
        Wver2_name <- paste0(Wver2)
        desclist4[[i]][[Wver2_name]] <- psych::describe(fblist_num4[[i]][[j]][[Wver2]])
      }
    }

  }
}

# Loop over each list element in desclist4
for (i in seq_along(desclist4)) {
  FBname <- names(desclist4)[i]
  FBdf_list <- desclist4[[i]]

  # Loop over each named df in the list element
  for (j in seq_along(FBdf_list)) {
    Wv_df <- FBdf_list[[j]]
    <- FBdf_list[[j]]


    # Extract the "rel" column from the df and add it to the results
    mean_col <- Wv_df[["mean"]]
    sd_col <- Wv_df[["sd"]]
    n_col <- Wv_df[["n"]]
    se_col <- Wv_df[["se"]]
    df_trend <- rbind(df_trend, data.frame(FBname = FBname,
                                           M_Wv = mean_col,
                                           SD_Wv = sd_col,
                                           N_Wv = n_col,
                                           stringsAsFactors = FALSE))
  }
}
