
# select joined FBs like in Stiftungsbericht_new_2

fblist_num8 <- list()

for (i in seq_along(fblist_num4)) {
  df_list <- list()
  for (j in seq_along(fblist_num4[[i]])) {
    if ((is.data.frame(fblist_num4[[i]][[j]])) & (grepl("joined", names(fblist_num4[[i]])[j]))) {
      df_list[[names(fblist_num4[[i]])[j]]] <- fblist_num4[[i]][[j]]
    }
  }
  fblist_num8[[i]] <- df_list
}

fblist_num8 <- unlist(fblist_num8, recursive = FALSE)
View(fblist_num8)

# select only variables starting with "^(WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM)_" or SK.


for (i in seq_along(fblist_num8)) {
  # Select only the needed variables
  fblist_num8[[i]] <- fblist_num8[[i]] %>% select(starts_with("SK.") | 
                                                    starts_with("CODE") | 
                                                    starts_with("WISS_") |
                                                    starts_with("ERFA_") |
                                                    starts_with("HAND_") |
                                                    starts_with("FAEH_") |
                                                    starts_with("NETZ_") |
                                                    starts_with("TEIL_") |
                                                    starts_with("AFFM_") )
}

# replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
library(dplyr)
for (i in seq_along(fblist_num8)) {
  fblist_num8[[i]] <- fblist_num8[[i]] %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))
}


# select all but everything-NA variables
for (i in seq_along(fblist_num8)) {
  fblist_num8[[i]] <- fblist_num8[[i]] %>%
    select_if(~!all(is.na(.)))
}


## make a list with matching t0 and t1 variables
# for all variables without _vor and _nac and _ver

pair_list2 <- list()

for (i in 1:length(fblist_num8)) {
  df_test <- fblist_num8[[i]]
  df_select_vor <- df_test  %>% select(matches("_t0")) %>% select(-contains("_vor") & -contains("_nac") & -contains("_ver"))  
  select_vor <- names(df_select_vor)
  
  df_select_nac <- df_test %>% select(matches("_t1"))  %>% select(-contains("_vor") & -contains("_nac") & -contains("_ver"))  
  select_nac <- names(df_select_nac)
  
  vari_df <- data.frame(Item_vor = select_vor, stringsAsFactors = FALSE)
  vari_df2 <- data.frame(Item_nac = select_nac, stringsAsFactors = FALSE)
  
  vari_df$signi <- NA
  vari_df$digit <- NA
  vari_df$pre <- NA
  
  vari_df2$signi <- NA
  vari_df2$digit <- NA
  vari_df2$pre <- NA
  
  for (j in 1:nrow(vari_df)) {
    
    ### prepare df for matching 
    ## vor
    #get item string
    row_data <- vari_df[j, "Item_vor"]
   
    # capture the matching string: everything before the _s
    match_string <- str_extract(row_data, ".*?(?=_s)")
    vari_df$signi[j] <- match_string
    
    # capture the first matching group: everything before the first underscore
    match_pre <- str_extract(match_string, "^((.*?)_[a-z0-9]{3,5})")
    vari_df$pre[j] <- match_pre
    
    # capture the digit in signi if its there, otherwise put an NA
    match_digit <- str_extract(match_string, "(?<=_i)\\d+")
    
    if (is.na(match_digit)) {
      vari_df$digit[j] <- NA
    } else {
      vari_df$digit[j] <- match_digit
    }
    vari_df$digit[j] <- match_digit
  }
  
  for (k in 1:nrow(vari_df2)) {
    
    ### prepare df for matching 
    ## nac
    #get item string
    row_data <- vari_df2[k, "Item_nac"]
    # capture the matching string: everything before the first
    match_string <- str_extract(row_data, ".*?(?=_s)")
    vari_df2$signi[k] <- match_string
    
    # capture the first matching group: everything before the first underscore
    match_pre <- str_extract(match_string, "^((.*?)_[a-z0-9]{3,5})")
    vari_df2$pre[k] <- match_pre
    
    # capture the digit in signi if its there, otherwise put an NA
    match_digit <- str_extract(match_string, "(?<=_i)\\d+")
    
    if (is.na(match_digit)) {
      vari_df2$digit[k] <- NA
    } else {
      vari_df2$digit[k] <- match_digit
    }
    vari_df2$digit[k] <- match_digit
  }
  
  df_pairs <- full_join(vari_df, vari_df2, by = c("pre", "digit"))
  df_pairs2 <- df_pairs %>% select(Item_vor,Item_nac)
  
  # Create a unique name for the data frame based on the df_test
  result_name <- paste0("result_", length(pair_list2) + 1)
  
  # Add the df_pairs2 data frame to the pair_list with the unique name
  pair_list2[[result_name]] <- df_pairs2
  
  # rename the df
  df_name <- names(fblist_num8[i])
  names(pair_list2)[names(pair_list2) == result_name] <- df_name
}
View(pair_list2)

## now for _vor and _nac 

pair_list3 <- list()

for (i in 1:length(fblist_num8)) {
  df_test <- fblist_num8[[i]]
  df_select_vor <- df_test  %>% select(contains("_vor"))  
  select_vor <- names(df_select_vor)
  
  df_select_nac <- df_test %>% select(contains("_nac"))  
  select_nac <- names(df_select_nac)
  
  vari_df <- data.frame(Item_vor = select_vor, stringsAsFactors = FALSE)
  vari_df2 <- data.frame(Item_nac = select_nac, stringsAsFactors = FALSE)
  
  vari_df$signi <- NA
  vari_df$digit <- NA
  vari_df$pre <- NA
  
  vari_df2$signi <- NA
  vari_df2$digit <- NA
  vari_df2$pre <- NA
  
  for (j in 1:nrow(vari_df)) {
    
    ### prepare df for matching 
    ## vor
    #get item string
    row_data <- vari_df[j, "Item_vor"]
    # capture everything before and after the first underscore
    match_string <- str_extract(row_data, "^((.*?)_[a-z0-9]{3,5})")
    vari_df$signi[j] <- match_string
    
    # capture everything before the first underscore
    match_pre <- str_extract(match_string, "^(.*?)(?=_)")
    vari_df$pre[j] <- match_pre
    
    # capture the digit in signi if its there, otherwise put an NA
    match_digit <- str_extract(match_string, "(\\d+)")
    
    if (is.na(match_digit)) {
      vari_df$digit[j] <- NA
    } else {
      vari_df$digit[j] <- match_digit
    }
    vari_df$digit[j] <- match_digit
  }
  
  for (k in 1:nrow(vari_df2)) {
    
    ### prepare df for matching 
    ## nac
    #get item string
    row_data <- vari_df2[k, "Item_nac"]
    # capture everything before and after the first underscore
    match_string <- str_extract(row_data, "^((.*?)_[a-z0-9]{3,5})")
    vari_df2$signi[k] <- match_string
    
    # capture everything before the first underscore
    match_pre <- str_extract(match_string, "^(.*?)(?=_)")
    vari_df2$pre[k] <- match_pre
    
    # capture the digit in signi if its there, otherwise put an NA
    match_digit <- str_extract(match_string, "(\\d+)")
    
    if (is.na(match_digit)) {
      vari_df2$digit[k] <- NA
    } else {
      vari_df2$digit[k] <- match_digit
    }
    vari_df2$digit[k] <- match_digit
  }
  
  df_pairs <- full_join(vari_df, vari_df2, by = c("pre", "digit"))
  df_pairs2 <- df_pairs %>% select(Item_vor,Item_nac)
  
  # Create a unique name for the data frame based on the df_test
  result_name <- paste0("result_", length(pair_list3) + 1)
  
  # Add the df_pairs2 data frame to the pair_list with the unique name
  pair_list3[[result_name]] <- df_pairs2
  
  # rename the df
  df_name <- names(fblist_num8[i])
  names(pair_list3)[names(pair_list3) == result_name] <- df_name
}
View(pair_list3)

## 'coalesce' both pair lists

shared_ID <- intersect(names(pair_list2), names(pair_list3))

# Append rows for common names
for (name in shared_ID) {
  pair_list3[[name]] <- rbind(pair_list3[[name]], pair_list2[[name]])
}

## !!!! only keep complete rows, filter cases with only a t0 but no t1
for (i in seq_along(pair_list3)) {
  pair_list3[[i]] <- pair_list3[[i]] %>% filter(complete.cases(.))
}




# Print the updated pair_list3
View(pair_list3[[1]])


####################


# Initialize an empty list to store the resulting data frames
stats_list2 <- list()


# Loop through each data frame in pair_list
for (i in 1:length(pair_list3)) {
  
  # Get the name of the current data frame
  df_name <- names(pair_list3)[i]
  
  # Find the corresponding data frame in fblist_num8
  fb_df <- fblist_num8[[df_name]]
  fb_df <- fb_df %>% filter(CODE != "")
  
  # Get the variable pairs from the current data frame
  variable_pairs <- pair_list3[[df_name]]
  
  # Initialize a data frame to store the results for this data frame
  df_zus <- data.frame(FB = character(),
                       Konstrukt = character(),
                       vorher = character(),
                       nachher = character(),
                       N_intra = numeric(),
                       M_vor = numeric(),
                       SD_vor = numeric(),
                       M_nac = numeric(),
                       SD_nac = numeric(),
                       M_dff = numeric(),
                       SD_dff = numeric(),
                       p_test = numeric(),
                       d_value = numeric(),
                       pwr_test = numeric(),
                       kendall = numeric(),
                       stringsAsFactors = FALSE)
  
    ## loop
  
  tryCatch({  
       # Loop through each variable pair
    for (j in 1:nrow(variable_pairs)) {
      con_vor <- variable_pairs$Item_vor[j]
      con_nac <- variable_pairs$Item_nac[j]
      
      vorher <- fb_df[, grepl(con_vor, colnames(fb_df))]
      nachher <- fb_df[, grepl(con_nac, colnames(fb_df))]
      
      # Calculate the difference
      mean_diff <- nachher - vorher
      
      wsr_test <- coin::wilcoxsign_test(vorher ~ nachher)
      kendall_t <- Kendall::Kendall(vorher, nachher)
      
      wsr_p <- coin::pvalue(wsr_test)
      wsr_z <- abs(wsr_test@statistic@teststatistic)
      wsr_r <- wsr_z/sqrt(nrow(wsr_test@statistic@x))
      #h <- 4                                                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #d <- sqrt(h * wsr_r) / sqrt(1-wsr_r^2)                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #n <- sum(!is.na(fblist_num8[[i]][[3]]))
      #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
      #d <- wsr_r
      #fblist_num8[[i]]$SK.diff_eff <- d
      d <- (sqrt(4) * wsr_r)/(sqrt(1-wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner
      
      library(MKpower)      ### the power simulation works on a minimum of n >= 6, when n < 6 the working minimum gets applied
      pwr_m <- mean(mean_diff, na.rm = T)
      pwr_sd <- sd(mean_diff, na.rm = T)
      rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
      pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x)-1,
                                   n.max = nrow(wsr_test@statistic@x)+5, step.size = 1, iter = 1000, BREAK = FALSE)
      pwr_df <- as.data.frame(cbind(pwr[["n"]], pwr[["emp.power"]]))
      target_n <- nrow(wsr_test@statistic@x)
      closest_row <- which.min(abs(pwr_df$V1 - target_n))
      closest_value <- pwr_df[closest_row, "V2"]
      closest_value
      diff_pwr <- closest_value
      
      # add everything with a new row
      df_zus <- rbind(df_zus, data.frame(FB = df_name,
                                         Konstrukt = str_extract(con_vor, "([A-Z]{3,5})(?=_)"),
                                         vorher = con_vor,
                                         nachher = con_nac,
                                         N_intra = sum(!is.na(mean_diff)),
                                         M_vor = mean(vorher, na.rm = T),
                                         SD_vor = sd(vorher, na.rm = T),
                                         M_nac = mean(nachher, na.rm = T),
                                         SD_nac = sd(nachher, na.rm = T),
                                         M_dff = mean(mean_diff, na.rm = T),
                                         SD_dff = sd(mean_diff, na.rm = T),
                                         p_test = wsr_p,
                                         d_value = d,
                                         pwr_test = diff_pwr,
                                         kendall = kendall_t[["tau"]]))
    }
  }, error = function(e) {
    # If an error occurs, assign NA to wsr_test and print error message
    #cat("Error occurred while running wilcoxsign_test on iteration ", i, ":\n", conditionMessage(e), "\n")
    error_message <- conditionMessage(e)
    cat(error_message)  # this can be deleted after debugging is completed
    
    if (any(grepl("pairwise differences equal zero", error_message))) {
      cat("Error: Specific Error 'all pairwise differences equal zero' occurred. Action taken: Theoretical Values assigned. \n")
      
      # Initialize a data frame to store the results for this data frame
      df_zus <- data.frame(FB = character(),
                           Konstrukt = character(),
                           vorher = character(),
                           nachher = character(),
                           N_intra = numeric(),
                           M_vor = numeric(),
                           SD_vor = numeric(),
                           M_nac = numeric(),
                           SD_nac = numeric(),
                           M_dff = numeric(),
                           SD_dff = numeric(),
                           p_test = numeric(),
                           d_value = numeric(),
                           pwr_test = numeric(),
                           kendall = numeric(),
                           stringsAsFactors = FALSE)
      
      # Loop through each variable pair
      for (j in 1:nrow(variable_pairs)) {
        con_vor <- variable_pairs$Item_vor[j]
        con_nac <- variable_pairs$Item_nac[j]
        
        vorher <- fb_df[, grepl(con_vor, colnames(fb_df))]
        nachher <- fb_df[, grepl(con_nac, colnames(fb_df))]
        
        # Calculate the difference
        mean_diff <- nachher - vorher
        
        kendall_t <- Kendall::Kendall(vorher, nachher)
        df_zus <- rbind(df_zus, data.frame(FB = df_name,
                                           Konstrukt = str_extract(con_vor, "([A-Z]{3,5})(?=_)"),
                                           vorher = con_vor,
                                           nachher = con_nac,
                                           N_intra = sum(!is.na(mean_diff)),
                                           M_vor = mean(vorher, na.rm = T),
                                           SD_vor = sd(vorher, na.rm = T),
                                           M_nac = mean(nachher, na.rm = T),
                                           SD_nac = sd(nachher, na.rm = T),
                                           M_dff = mean(mean_diff, na.rm = T),
                                           SD_dff = sd(mean_diff, na.rm = T),
                                           p_test = .9999999,
                                           d_value = 0,
                                           pwr_test = 0,
                                           kendall = kendall_t[["tau"]]))
      }
    } else if (any(grepl("subscript out of bounds", error_message))) {
      cat("Error: Specific Error 'subscript out of bounds' occurred. Action taken: ??? \n")
      
      # Initialize a data frame to store the results for this data frame
      df_zus <- data.frame(FB = character(),
                           Konstrukt = character(),
                           vorher = character(),
                           nachher = character(),
                           N_intra = numeric(),
                           M_vor = numeric(),
                           SD_vor = numeric(),
                           M_nac = numeric(),
                           SD_nac = numeric(),
                           M_dff = numeric(),
                           SD_dff = numeric(),
                           p_test = numeric(),
                           d_value = numeric(),
                           pwr_test = numeric(),
                           kendall = numeric(),
                           stringsAsFactors = FALSE)
      
      # Loop through each variable pair
      for (j in 1:nrow(variable_pairs)) {
        con_vor <- variable_pairs$Item_vor[j]
        con_nac <- variable_pairs$Item_nac[j]
        
        vorher <- fb_df[, grepl(con_vor, colnames(fb_df))]
        nachher <- fb_df[, grepl(con_nac, colnames(fb_df))]
        
        # Calculate the difference
        mean_diff <- nachher - vorher
        
        wsr_test <- coin::wilcoxsign_test(vorher ~ nachher)
        kendall_t <- Kendall::Kendall(vorher, nachher)
        
        wsr_p <- coin::pvalue(wsr_test)
        wsr_z <- abs(wsr_test@statistic@teststatistic)
        wsr_r <- wsr_z/sqrt(nrow(wsr_test@statistic@x))
        #h <- 4                                                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
        #d <- sqrt(h * wsr_r) / sqrt(1-wsr_r^2)                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
        #n <- sum(!is.na(fblist_num8[[i]][[3]]))
        #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
        #d <- wsr_r
        #fblist_num8[[i]]$SK.diff_eff <- d
        d <- (sqrt(4) * wsr_r)/(sqrt(1-wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner
        
        library(MKpower)      ### the power simulation works on a minimum of n >= 6, when n < 6 the working minimum gets applied
        pwr_m <- mean(mean_diff, na.rm = T)
        pwr_sd <- sd(mean_diff, na.rm = T)
        rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
        pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x)-1,
                                     n.max = nrow(wsr_test@statistic@x)+5, step.size = 1, iter = 1000, BREAK = FALSE)
        pwr_df <- as.data.frame(cbind(pwr[["n"]], pwr[["emp.power"]]))
        target_n <- nrow(wsr_test@statistic@x)
        closest_row <- which.min(abs(pwr_df$V1 - target_n))
        closest_value <- pwr_df[closest_row, "V2"]
        closest_value
        diff_pwr <- closest_value
        
        # add everything with a new row
        df_zus <- rbind(df_zus, data.frame(FB = df_name,
                                           Konstrukt = str_extract(con_vor, "([A-Z]{3,5})(?=_)"),
                                           vorher = con_vor,
                                           nachher = con_nac,
                                           N_intra = sum(!is.na(mean_diff)),
                                           M_vor = mean(vorher, na.rm = T),
                                           SD_vor = sd(vorher, na.rm = T),
                                           M_nac = mean(nachher, na.rm = T),
                                           SD_nac = sd(nachher, na.rm = T),
                                           M_dff = mean(mean_diff, na.rm = T),
                                           SD_dff = sd(mean_diff, na.rm = T),
                                           p_test = wsr_p,
                                           d_value = d,
                                           pwr_test = diff_pwr,
                                           kendall = kendall_t[["tau"]]))
      }
    }

  # Add df_new to stats_list with the unique name
  stats_list2[[df_name]] <<- as.data.frame(df_zus)
    
  })
}

### the result is a list of dfs with intra-individual statistics
## the results in the list can be used for the report



##### trouble shooting

# Create a data frame with 24 rows
a <- data.frame(
  vorher = rep(NA, 24),
  nachher = rep(NA, 24)
)

# Add non-NA values in two rows
a[1, c("vorher", "nachher")] <- c(5, 8)
a[2, c("vorher", "nachher")] <- c(NA, 1)

coin::wilcoxsign_test(a$vorher ~ a$nachher)
a <- Kendall::Kendall(a$vorher, a$nachher)

