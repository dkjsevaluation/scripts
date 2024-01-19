
#########

fblist_num6 <- list()

for (i in seq_along(fblist_num4)) {
  df_list <- list()
  for (j in seq_along(fblist_num4[[i]])) {
    if ((is.data.frame(fblist_num4[[i]][[j]]) & (grepl("joined", names(fblist_num4[[i]])[j])) | grepl("_tr", names(fblist_num4[[i]])[j]))) {
      df_list[[names(fblist_num4[[i]])[j]]] <- fblist_num4[[i]][[j]]
    }
  }
  fblist_num6[[i]] <- df_list
}

fblist_num6 <- unlist(fblist_num6, recursive = FALSE)
View(fblist_num6)


#####

calculate_scales_bericht <- function(variable_name) {
for (i in seq_along(fblist_num6)) {
  variables <- grep(paste0("^", variable_name), names(fblist_num6[[i]]), value = TRUE)
  fblist_num6[[i]] <- fblist_num6[[i]] %>%
    mutate(!!paste0("SK.", variable_name) := rowMeans(select(., any_of(variables)), na.rm = TRUE)) %>%
    select(names(fblist_num6[[i]]), paste0("SK.", variable_name))
}
  return(fblist_num6)
}

fblist_num6 <- calculate_scales_bericht("WISS_vor")
fblist_num6 <- calculate_scales_bericht("WISS_nac")

fblist_num6 <- calculate_scales_bericht("ERFA_vor")
fblist_num6 <- calculate_scales_bericht("ERFA_nac")

fblist_num6 <- calculate_scales_bericht("FAEH_vor")
fblist_num6 <- calculate_scales_bericht("FAEH_nac")


for (i in seq_along(fblist_num6)) {
  # Select only the variables that start with "SK."
  fblist_num6[[i]] <- fblist_num6[[i]] %>% select(starts_with("SK.") | starts_with("CODE") )
}

# replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
library(dplyr)
for (i in seq_along(fblist_num6)) {
  fblist_num6[[i]] <- fblist_num6[[i]] %>%
  mutate_all(function(x) ifelse(is.nan(x), NA, x))
}

for (i in seq_along(fblist_num6)) {
  fblist_num6[[i]] <- fblist_num6[[i]] %>%
    select_if(~!all(is.na(.)))
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
                     d_test = numeric(),
                     pwr_test = numeric(),
                     kendall = numeric(),
                     stringsAsFactors = FALSE)



calculate_intra_statistics_scales <- function(construct) {

con_vor <- paste0("WISS", "_vor")
con_nac <- paste0("WISS", "_nac")

for (i in 1:length(fblist_num6)) {


  if (any(grepl(con_vor, colnames(fblist_num6[[i]]))) & any(grepl(con_nac, colnames(fblist_num6[[i]])))) {
    fblist_num6[[i]]$SK.WISSdiff <- fblist_num6[[i]][, grepl(con_nac, colnames(fblist_num6[[i]]))] - fblist_num6[[i]][, grepl(con_vor, colnames(fblist_num6[[i]]))]
    wsr_test <- coin::wilcoxsign_test(fblist_num6[[i]][, grepl(con_vor, colnames(fblist_num6[[i]]))] ~ fblist_num6[[i]][, grepl(con_nac, colnames(fblist_num6[[i]]))], data = fblist_num6[[i]], zero.method = "Wilcoxon")
    kendall_t <- psych::corr.test(fblist_num6[[i]][, grepl(con_vor, colnames(fblist_num6[[i]]))], fblist_num6[[i]][, grepl(con_nac, colnames(fblist_num6[[i]]))], method = "kendall")

    fblist_num6[[i]]$SK.ERFAdiff_p <- coin::pvalue(wsr_test)

    wsr_z <- wsr_test@statistic@teststatistic
    wsr_r <- abs(wsr_z/sqrt(nrow(wsr_test@statistic@x)))
    fblist_num6[[i]]$SK.WISSdiff_d <- sqrt(-4 * wsr_r^2 / (wsr_r^2 - 1))

    #library(MKpower)
   # rxy <- function(n) rnorm(n, mean = mean(fblist_num6[[i]]$SK.WISSdiff, na.rm = T), sd(fblist_num6[[i]]$SK.WISSdiff, na.rm = T))
    #pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "paired", n.min = nrow(wsr_test@statistic@x),
    #                             n.max = nrow(wsr_test@statistic@x)+2, step.size = 1, iter = 1000, BREAK = FALSE)
   # fblist_num6[[i]]$SK.WISSdiff_pwr <- pwr[["emp.power"]][[i]]

    df_zus <- rbind(df_zus, data.frame(FB = names(fblist_num6)[i],
                                       Konstrukt = paste0("WISS"),
                                       N_intra = sum(!is.na(fblist_num6[[i]][[3]])),
                                       M_vor = mean(fblist_num6[[i]][, grepl(con_vor, colnames(fblist_num6[[i]]))], na.rm = T),
                                       SD_vor = sd(fblist_num6[[i]][, grepl(con_vor, colnames(fblist_num6[[i]]))], na.rm = T),
                                       M_nac = mean(fblist_num6[[i]][, grepl(con_nac, colnames(fblist_num6[[i]]))], na.rm = T),
                                       SD_nac = sd(fblist_num6[[i]][, grepl(con_nac, colnames(fblist_num6[[i]]))], na.rm = T),
                                       M_dff = mean(fblist_num6[[i]]$SK.WISSdiff, na.rm = T),
                                       SD_dff = sd(fblist_num6[[i]]$SK.WISSdiff, na.rm = T),
                                       p_test = coin::pvalue(wsr_test),
                                       d_test = sqrt(-4 * wsr_r^2 / (wsr_r^2 - 1)),
                                      # pwr_test = pwr[["emp.power"]][[i]],
                                       kendall = kendall_t$r
    ))
  }
}
return(df_zus)
}

wiss_intra_statistics <- calculate_intra_statistics_scales("WISS")
erfa_intra_statistics <- calculate_intra_statistics_scales("ERFA")

wiss_intra_statistics <- wiss_intra_statistics %>%
  rename(`Programm-ID` = `FB`)

erfa_intra_statistics <- erfa_intra_statistics %>%
  rename(`Programm-ID` = `FB`)
