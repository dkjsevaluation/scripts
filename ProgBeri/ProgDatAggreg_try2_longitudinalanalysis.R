
# Initialize an empty list to store the resulting data frames
stats_list <- list()

# Loop through each data frame in pair_list
for (i in 1:length(pair_list)) {
  
  # Get the name of the current data frame
  df_name <- names(pair_list)[i]
  
  # Find the corresponding data frame in fblist_num7
  fb_df <- fblist_num7[[df_name]]
  
  # Get the variable pairs from the current data frame
  variable_pairs <- pair_list[[df_name]]
  
  # Initialize a data frame to store the results for this data frame
  tryCatch({  
    df_zus <- data.frame(FB = character(),
                         Konstrukt = character(),
                         N_intra = numeric(),
                         M_vor = numeric(),
                         SD_vor = numeric(),
                         M_nac = numeric(),
                         SD_nac = numeric(),
                         M_dff = numeric(),
                         SD_dff = numeric(),
                         p_test = numeric(),
                         eff_size = numeric(),
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
      #n <- sum(!is.na(fblist_num7[[i]][[3]]))
      #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
      #d <- wsr_r
      #fblist_num7[[i]]$SK.diff_eff <- d
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
                                         kendall = kendall_t[["tau"]]
      ))
    }
  }, error = function(e) {
    # If an error occurs, assign NA to wsr_test and print error message
    #cat("Error occurred while running wilcoxsign_test on iteration ", i, ":\n", conditionMessage(e), "\n")
    error_message <- conditionMessage(e)
    cat(error_message)  # this can be deleted after debugging is completed
    
    if (any(grepl("pairwise differences equal zero", error_message))) {
      cat("Error: Specific Error 'all pairwise differences equal zero' occurred.Action taken: Theoretical Values assigned. \n")
      
      df_zus <- data.frame(FB = character(),
                           Konstrukt = character(),
                           N_intra = numeric(),
                           M_vor = numeric(),
                           SD_vor = numeric(),
                           M_nac = numeric(),
                           SD_nac = numeric(),
                           M_dff = numeric(),
                           SD_dff = numeric(),
                           p_test = numeric(),
                           eff_size = numeric(),
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
                                           kendall = kendall_t[["tau"]]
        ))
      }
    } else if (any(grepl("subscript out of bounds", error_message))) {
      cat("Error: Specific Error 'subscript out of bounds' occurred. Action taken: ??? \n")
      
      df_zus <- data.frame(FB = character(),
                           Konstrukt = character(),
                           N_intra = numeric(),
                           M_vor = numeric(),
                           SD_vor = numeric(),
                           M_nac = numeric(),
                           SD_nac = numeric(),
                           M_dff = numeric(),
                           SD_dff = numeric(),
                           p_test = numeric(),
                           eff_size = numeric(),
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
        #n <- sum(!is.na(fblist_num7[[i]][[3]]))
        #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
        #d <- wsr_r
        #fblist_num7[[i]]$SK.diff_eff <- d
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
                                           kendall = kendall_t[["tau"]]
        ))
      }
    }
    df_zus <- data.frame(FB = character(),
                         Konstrukt = character(),
                         N_intra = numeric(),
                         M_vor = numeric(),
                         SD_vor = numeric(),
                         M_nac = numeric(),
                         SD_nac = numeric(),
                         M_dff = numeric(),
                         SD_dff = numeric(),
                         p_test = numeric(),
                         eff_size = numeric(),
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
                                         p_test = NA,
                                         d_value = NA,
                                         pwr_test = NA,
                                         kendall = kendall_t[["tau"]]
      ))
    }
  })
  # Add df_new to stats_list with the unique name
    stats_list[[df_name]] <- df_zus
  
}

