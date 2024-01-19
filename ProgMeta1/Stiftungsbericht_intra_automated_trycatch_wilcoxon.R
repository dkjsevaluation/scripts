calculate_intra_statistics <- function(fbliste, construct) {

  con_vor <- paste0(construct, "_vor")
  con_nac <- paste0(construct, "_nac")

  gr_vor <- paste0(construct, ".*_vor")
  gr_nac <- paste0(construct, ".*_nac")

  # Create an empty data frame to store the results
  pair_registry_vor <- data.frame(FB = character(), vor = character(), stringsAsFactors = FALSE)

  # Loop through each data frame in the list
  for (i in seq_along(fbliste)) {
    # Get the name of the current data frame
    fb_name <- names(fbliste)[i]
    # Extract the variables that match the pattern using grep
    vor_vars <- grep(gr_vor, names(fbliste[[i]]), value = TRUE)
    # Add the results to the pair_registry data frame
    pair_registry_vor <- rbind(pair_registry_vor, data.frame(FB = fb_name, vor = vor_vars, stringsAsFactors = FALSE))
  }

  # View the final result
  pair_registry_vor

  # Create an empty data frame to store the results
  pair_registry_nac <- data.frame(FB = character(), nac = character(), stringsAsFactors = FALSE)

  # Loop through each data frame in the list
  for (i in seq_along(fbliste)) {
    # Get the name of the current data frame
    fb_name <- names(fbliste)[i]
    # Extract the variables that match the pattern using grep
    nac_vars <- grep(gr_nac, names(fbliste[[i]]), value = TRUE)
    # Check if there are any matches
    if (length(nac_vars) == 0) {
      # If there are no matches, add a new row with NAs
      pair_registry_nac <- rbind(pair_registry_nac, data.frame(FB = fb_name, nac = NA, stringsAsFactors = FALSE))
    } else {
      # If there are matches, add a new row for each match
      for (j in seq_along(nac_vars)) {
        pair_registry_nac <- rbind(pair_registry_nac, data.frame(FB = fb_name, nac = nac_vars[j], stringsAsFactors = FALSE))
      }
    }
  }

  # View the final result
  pair_registry_nac

  # merge
  pair_registry_complete <- pair_registry_vor
  pair_registry_complete$nac <- pair_registry_nac$nac

  pair_registry_complete



  ##### Second, reduce the dfs to these pairs and make a list with pairs

  # Create an empty nested list to store the extracted data frames
  extracted_data <- list()

  # Loop through each row of pair_registry_complete
  for (i in seq_along(pair_registry_complete$FB)) {
    # Get the name of the current data frame
    df_name <- pair_registry_complete$FB[i]

    # Get the relevant variable names
    var1 <- pair_registry_complete$vor[i]
    var2 <- pair_registry_complete$nac[i]

    # Skip rows with missing values in nac
    if (is.na(var2)) {
      next
    }

    # Extract the variables from the data frame in fblist_num5
    df <- fbliste[[df_name]]
    extracted <- df[, c(var1, var2)]

    # Set the column names of the extracted data frame
    colnames(extracted) <- c(var1, var2)

    # Create a new nested list if the df_name doesn't exist in extracted_data
    if (!(df_name %in% names(extracted_data))) {
      extracted_data[[df_name]] <- list()
    }

    # Append the extracted data frame to the nested list under the current df_name
    extracted_data[[df_name]][[i]] <- extracted
  }


  # Loop through each element in extracted_data and remove elements with type NULL
  for (i in seq_along(extracted_data)) {
    extracted_data[[i]] <- Filter(Negate(is.null), extracted_data[[i]])
  }

  # Rename data frames in the nested lists
  for (name in names(extracted_data)) {
    dfs <- extracted_data[[name]]
    for (i in seq_along(dfs)) {
      new_name <- paste0(name, "_df", i)
      extracted_data[[name]][[new_name]] <- dfs[[i]]
      names(extracted_data[[name]])[i] <- new_name
    }
  }

  for (i in seq_along(extracted_data)) {
    # Identify duplicate data frames within the current nested list
    duplicates <- duplicated(extracted_data[[i]])

    # Remove duplicates from the current nested list
    extracted_data[[i]] <- extracted_data[[i]][!duplicates]
  }

  # Check the resulting list of nested data frames

  extracted_data <- unlist(extracted_data, recursive = FALSE)
  names(extracted_data) <- gsub("^[^.]+\\.", "", names(extracted_data))

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




  for (i in 1:length(extracted_data)) {
    if (any(grepl(con_vor, colnames(extracted_data[[i]]))) & any(grepl(con_nac, colnames(extracted_data[[i]])))) {
      extracted_data[[i]]$SK.diff <- extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))] - extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))]
      ##if any row in extracted_data[[i]]$SK.diff > 0, else NA

      tryCatch({

      wsr_test <- coin::wilcoxsign_test(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))] ~ extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], data = extracted_data[[i]], zero.method = "Wilcoxon")
      #kendall_t <- psych::corr.test(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], method = "kendall")
      kendall_t <- Kendall::Kendall(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))])

      extracted_data[[i]]$SK.diff_p <- coin::pvalue(wsr_test)

      wsr_z <- abs(wsr_test@statistic@teststatistic)
      wsr_r <- wsr_z/sqrt(nrow(wsr_test@statistic@x))
      #h <- 4                                                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #d <- sqrt(h * wsr_r) / sqrt(1-wsr_r^2)                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #n <- sum(!is.na(extracted_data[[i]][[3]]))
      #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
      #d <- wsr_r
      #extracted_data[[i]]$SK.diff_eff <- d
      d <- (sqrt(4) * wsr_r)/(sqrt(1-wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner

      library(MKpower)
      rxy <- function(n) rnorm(n, mean = mean(extracted_data[[i]]$SK.diff, na.rm = T), sd(extracted_data[[i]]$SK.diff, na.rm = T))
      pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "paired", n.min = nrow(wsr_test@statistic@x),
                                   n.max = nrow(wsr_test@statistic@x)+2, step.size = 1, iter = 1000, BREAK = FALSE)
      extracted_data[[i]]$SK.diff_pwr <- pwr[["emp.power"]][[1]]

      df_zus <- rbind(df_zus, data.frame(FB = names(extracted_data)[i],
                                         Konstrukt = paste0(construct),
                                         N_intra = sum(!is.na(extracted_data[[i]][[3]])),
                                         M_vor = mean(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], na.rm = T),
                                         SD_vor = sd(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], na.rm = T),
                                         M_nac = mean(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                         SD_nac = sd(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                         M_dff = mean(extracted_data[[i]]$SK.diff, na.rm = T),
                                         SD_dff = sd(extracted_data[[i]]$SK.diff, na.rm = T),
                                         p_test = coin::pvalue(wsr_test),
                                         eff_size = d,
                                         pwr_test = pwr[["emp.power"]][[1]],
                                         kendall = kendall_t[["tau"]]
      ))}, error = function(e) {
        # If an error occurs, assign NA to wsr_test and print error message
        cat("Error occurred while running wilcoxsign_test on iteration ", i, ":\n", conditionMessage(e), "\n")
      })

      #rankcor <- cor.test(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))],extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], method = "spearman")
      #eff_size <- rankcor[["estimate"]][["rho"]]
      #kendall_t <- psych::corr.test(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], method = "kendall")
      kendall_t <- Kendall::Kendall(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))])
      df_zus <- rbind(df_zus, data.frame(FB = names(extracted_data)[i],
                                         Konstrukt = paste0(construct),
                                         N_intra = sum(!is.na(extracted_data[[i]][[3]])),
                                         M_vor = mean(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], na.rm = T),
                                         SD_vor = sd(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], na.rm = T),
                                         M_nac = mean(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                         SD_nac = sd(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                         M_dff = mean(extracted_data[[i]]$SK.diff, na.rm = T),
                                         SD_dff = sd(extracted_data[[i]]$SK.diff, na.rm = T),
                                         p_test = NA,
                                         eff_size = NA,
                                         pwr_test = NA,
                                         kendall = kendall_t[["tau"]]
                                         ))


    }
  }
  df_zus <- df_zus[!duplicated(df_zus[,c("FB", "Konstrukt", "N_intra", "M_vor", "SD_vor", "M_nac", "SD_nac", "M_dff", "SD_dff", "kendall")]),]
  return(df_zus)
}

##################
#### Option 1 ####
##################

wiss_intra_results_test <- calculate_intra_statistics(fblist_num5, "WISS")




##################
#### Option 2 ####
##################

wiss_intra_results2_test <- calculate_intra_statistics(fblist_num6, "WISS")





# eff for paired-t-test
r = t / sqrt(t^2 + df)

# eff if try catch NA
y <- cor.test(a, b, method = "spearman")




a <- c(1,2,1,2,1,2,1)
b <- c(1,2,1,2,2,2,2)
f <- as.data.frame(cbind(a,b))
c <- coin::wilcoxsign_test(a~b,f)
z <- abs(c@statistic@teststatistic)
r <- z/sqrt(7)

mean(a)
mean(b)
sd(a)
sd(b)
cor(a,b)
d <- 2 * asin(r)
d

