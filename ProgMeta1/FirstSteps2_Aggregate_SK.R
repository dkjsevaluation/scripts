################################
######## Aggregate Data ########
################################
fblist_num3 <- fblist_num2


######## put all descriptives in a list inside a list (First Layer FB, second Layer Scales)
### done: attach reliability of scale to the descriptives

# Convert all NaN to NA

for (i in seq_along(fblist_num3)) {
  for (j in seq_along(fblist_num3[[i]])) {
    fblist_num3[[i]][[j]][sapply(fblist_num3[[i]][[j]], is.nan)] <- NA
  }
}

# Create empty list to fill in later
desclist3 <- list()

# Loop over FBs (named lists)
for (i in names(fblist_num3)) {
  FBnames <- fblist_num3[[i]]

  # Create a new list element in desclist for each FB
  desclist3[[i]] <- list()

  # Loop over named dfs inside FBs
  for (j in names(FBnames)) {

    # select variables starting with SK. and REL.
    SKname <- grep("^SK\\.", names(fblist_num3[[i]][[j]]), value = TRUE)
    RELname <- grep("^REL\\.", names(fblist_num3[[i]][[j]]), value = TRUE)

    # Loop over selected SK variables
    for (k in seq_along(SKname)) {
      SKname2 <- SKname[k]

      # When found, use psych::describe() for descriptives on it
      if (length(SKname) > 0) {
        desc_name <- paste0(SKname2)
        desclist3[[i]][[desc_name]] <- psych::describe(fblist_num3[[i]][[j]][[SKname2]])
      }
    }

    # Loop over selected REL variables
    for (l in seq_along(RELname)) {
      RELname2 <- RELname[l]

      # When found, compute maximum value and add to desclist3
      if (length(RELname2) > 0) {
        rel_name <- "rel"
        desclist3[[i]][[desc_name]][[rel_name]] <- max(fblist_num3[[i]][[j]][[RELname2]])
      }
    }
  }
}

####################################################################
################# Create summary df for Meta-Analysis stage2 #######
####################################################################

# Make empty dataframe to fill in later
dfstage2 <- data.frame(FBname = character(), Skala = character(),
                       M = numeric(), SD = numeric(), N = numeric(),
                       SE = numeric(), REL = numeric(), stringsAsFactors = FALSE)


# Loop over each list element in desclist3
for (i in seq_along(desclist3)) {
  FBname <- names(desclist3)[i]
  FBdf_list <- desclist3[[i]]

  # Loop over each named df in the list element
  for (j in seq_along(FBdf_list)) {
    Skala <- names(FBdf_list)[j]
    Skala_df <- FBdf_list[[j]]

    # Extract the "rel" column from the df and add it to the results
    rel_col <- Skala_df[["rel"]]
    mean_col <- Skala_df[["mean"]]
    sd_col <- Skala_df[["sd"]]
    n_col <- Skala_df[["n"]]
    se_col <- Skala_df[["se"]]
    dfstage2 <- rbind(dfstage2, data.frame(FBname = FBname,
                                     Skala = Skala,
                                     M = mean_col,
                                     SD = sd_col,
                                     N = n_col,
                                     SE = se_col,
                                     rel = rel_col,
                                     stringsAsFactors = FALSE))
  }
}

#### add columns for meta analysis identifying constructs
### one column for the highest construct level
dfstage2$Wirk_o<- stringr::str_extract(dfstage2$Skala, "(?<=SK\\.)[^_]+")

### one column for scale _s
dfstage2$Skalenart <- stringr::str_extract(dfstage2$Skala, "_s\\d+")

### one column for the highest level joined by _s
dfstage2$Wirk_o_s <- paste0(dfstage2$Wirk_o, dfstage2$Skalenart)

### one column for both construct levels
dfstage2$Wirk_ou <- stringr::str_extract(dfstage2$Skala, "(?<=SK\\.)([^_]+)_([^_]+)")

### one column for both construct levels without index number
dfstage2$Wirk_ou2 <- sub("\\d+", "", dfstage2$Wirk_ou)

### one column for both construct levels without index number joined by _s
dfstage2$Wirk_ou2_s <- paste0(dfstage2$Wirk_ou2, dfstage2$Skalenart)

### one column for the lowest level level
dfstage2$Wirk_u <- sub(".*_", "", dfstage2$Wirk_ou)

### reorder columns
new_order <- c("FBname","Skala","M","SD","N","SE","rel",
               "Wirk_o","Wirk_o_s","Wirk_ou", "Wirk_ou2_s",
               "Skalenart","Wirk_ou2","Wirk_u")
dfstage2 <- dfstage2[, new_order]

#######################################################################################################
####### make tables for every unique level and scale combination on highest aggregation level Wirk_o ##
#######################################################################################################

# get unique values of Wirk_o_s
unique_Wirk_o_s <- unique(dfstage2$Wirk_o_s)

# Create an empty list to store data frames
result_list_o <- list()

# loop over unique values of Wirk_o_s
library(stringr)
for (i in unique_Wirk_o_s) {

  # subset dfstage2 for current value of Wirk_o_s
  current_df <- subset(dfstage2, Skalenart == paste0("_s", str_extract(i, "\\d+")))
  current_df <- subset(current_df, Wirk_o == paste0(str_extract(i, "[A-Z]{1,6}")))

  # add current_df to the result list
  result_list_o[[i]] <- current_df

}

### make a table for descriptives of each i in result_list

## !!!! calculations for weighted means use division. Right now, there is no "protection" against
## division by zero, which could result in NaN as result.A protection would look like this.
# Calculate the inverse variance weighted mean and store it in prog_results_desc_o
#if (sum(1/result_list_o[[i]][["SD"]]) != 0) {
#  prog_results_desc_o[i, "sd_weighted_M"] <- sum((result_list_o[[i]][["M"]]) / (result_list_o[[i]][["SD"]]))/(sum(1/result_list_o[[i]][["SD"]]))
#} else {
#  prog_results_desc_o[i, "sd_weighted_M"] <- NA
#}

# Create empty df to fill later
prog_results_desc_o <- data.frame(construct = character(), N_FBs = numeric(),
                                unweighted_M = numeric(), n_weighted_M = numeric(),
                                sd_weighted_M = numeric(),
                                rel_weighted_M = numeric())

# Loop over the list of data frames
for (i in seq_along(result_list_o)) {

  # Extract the name of the data frame and store it in the 'construct' column
  prog_results_desc_o[i, "construct"] <- names(result_list_o)[i]

  # Calculate the mean of the 'M' variable and store it in the 'unweighted_M' column
  prog_results_desc_o[i, "unweighted_M"] <- mean(result_list_o[[i]]$M)

  # Calculuate the n weighted mean and store it in prog_results_desc
  prog_results_desc_o[i, "n_weighted_M"] <- sum((result_list_o[[i]]$M) * ((result_list_o[[i]]$N)-1))/sum((result_list_o[[i]]$N)-1)

  # Calculuate the inverse variance weighted mean and store it in prog_results_desc
  # Replace any zeros or negative values in SD with a small non-zero valuen to prevent division by zero
  SD <- result_list_o[[i]]$SD
  SD[SD <= 0] <- 0.00001
    prog_results_desc_o[i, "sd_weighted_M"] <- sum(result_list_o[[i]]$M / SD) / sum(1 / SD)

  # Calculate the reliability weighted mean, excluding NAs in rel vector
  # if NA is present in rel, the corresponding M is excluded from the calculation
  prog_results_desc_o[i, "rel_weighted_M"] <- with(result_list_o[[i]], {
    w <- rel / sum(rel, na.rm = TRUE)
    m <- M
    sum(w[!is.na(rel)] * m[!is.na(rel)], na.rm = TRUE)
  })

  # Count elements of i and attach to df
  prog_results_desc_o[i, "N_FBs"] <- length(unique(result_list_o[[i]][["FBname"]]))
}

# Print the resulting data frame
View(prog_results_desc_o)

#########################################################################################################
####### make tables for every unique level and scale combination on lower aggregation level Wirk_ou2_s ##
#########################################################################################################


# get unique values of Wirk_o_s
unique_Wirk_ou2_s <- unique(dfstage2$Wirk_ou2_s)

# Create an empty list to store data frames
result_list_ou <- list()

# loop over unique values of Wirk_o_s
library(stringr)
for (i in unique_Wirk_ou2_s) {

  # subset dfstage2 for current value of Wirk_ou2_s
  current_df <- subset(dfstage2, Skalenart == paste0("_s", str_extract(i, "\\d+")))
  current_df <- subset(current_df, Wirk_ou2 == paste0(str_extract(i, "[A-Z]{1,6}_[a-z]{1,6}")))

  # add current_df to the result list
  result_list_ou[[i]] <- current_df

}

### make a table for descriptives of each i in result_list

## !!!! calculations for weighted means use division. Right now, there is no "protection" against
## division by zero, which could result in NaN as result.A protection would look like this.
# Calculate the inverse variance weighted mean and store it in prog_results_desc_o
#if (sum(1/result_list_o[[i]][["SD"]]) != 0) {
#  prog_results_desc_o[i, "sd_weighted_M"] <- sum((result_list_o[[i]][["M"]]) / (result_list_o[[i]][["SD"]]))/(sum(1/result_list_o[[i]][["SD"]]))
#} else {
#  prog_results_desc_o[i, "sd_weighted_M"] <- NA
#}

# Create empty df to fill later
prog_results_desc_ou <- data.frame(construct = character(), N_FBs = numeric(),
                                  unweighted_M = numeric(), n_weighted_M = numeric(),
                                  sd_weighted_M = numeric(),
                                  rel_weighted_M = numeric())

# Loop over the list of data frames
for (i in seq_along(result_list_ou)) {

  # Extract the name of the data frame and store it in the 'construct' column
  prog_results_desc_ou[i, "construct"] <- names(result_list_ou)[i]

  # Calculate the mean of the 'M' variable and store it in the 'unweighted_M' column
  prog_results_desc_ou[i, "unweighted_M"] <- mean(result_list_ou[[i]]$M)

  # Calculuate the n weighted mean and store it in prog_results_desc
  prog_results_desc_ou[i, "n_weighted_M"] <- sum((result_list_ou[[i]]$M) * ((result_list_ou[[i]]$N)-1))/sum((result_list_ou[[i]]$N)-1)

  # Calculuate the inverse variance weighted mean and store it in prog_results_desc
  # Replace any zeros or negative values in SD with a small non-zero valuen to prevent division by zero
  SD <- result_list_ou[[i]]$SD
  SD[SD <= 0] <- 0.00001
  prog_results_desc_ou[i, "sd_weighted_M"] <- sum(result_list_ou[[i]]$M / SD) / sum(1 / SD)

  # Calculate the reliability weighted mean, excluding NAs in rel vector
  # if NA is present in rel, the corresponding M is excluded from the calculation
  prog_results_desc_ou[i, "rel_weighted_M"] <- with(result_list_ou[[i]], {
    w <- rel / sum(rel, na.rm = TRUE)
    m <- M
    sum(w[!is.na(rel)] * m[!is.na(rel)], na.rm = TRUE)
  })

  # Count elements of i and attach to df
  prog_results_desc_ou[i, "N_FBs"] <- length(unique(result_list_ou[[i]][["FBname"]]))
}

# Print the resulting data frame
View(prog_results_desc_ou)

