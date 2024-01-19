#### to do: integrate Vielfalt Daten... export lamapoll as xlsx, add data manually, import new xlsx

library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
library(patchwork)
library(stringr)
library(pwr)
library(dkjslama)
#########################################################################
############################## loop through variable names ##############
#########################################################################

###create empty list for fbdata
fblist <- list()

### get vector with fbnames
fbnames <- c("Evaluation_JbK_vorher", "Evaluation_Schulerfolgdigital_retro", "TechnovationGirls01", "TechnovationGirls02")

### get vector with varnames
varnames <- c("WISS_meth", "WISS", "NETZ", "XYZ.")

## get Lamapoll data for every fbname and put it into list fbdata with fbname as name of df
for (i in fbnames) {
  df <- as.data.frame(get.lamapoll.data(i, "we@dkjs.de", "wGl5v4fz"))
  df <- df %>%
    mutate(across(where(is.integer), as.numeric))
  fblist[[i]] <- df
}

### delete suffix .xxx in all variable names in all dfs
#fblist <- lapply(fblist, function(df) {
#  names(df) <- stringr::str_remove(names(df), "\\..*")
#  df
#})

### use only numeric variables
# to do CODE muss drin bleiben
fblist_num <- list()
for (j in 1:length(fblist)) {
  df_num <- fblist[[j]] %>% select(matches("CODE") | where(is.numeric))
  fblist_num[[length(fblist_num) + 1]] <- df_num
}
names(fblist_num) <- names(fblist)

### to do matching
##  ID an FBs dranheften bzw. die ersetzen // perspektivisch aus Excel
##  CODE Groß- und Kleinschreibung vereinheitlichen // fallausschlusskriterien z.B. wenn es mehrere gleiche gibt?
##  full join in neuen Datensatz mit Programm-ID //perspektivisch join cascade in den letzten MZP


####    if _vor und _nac are present, calculate difference variable WISS_diff = _nac - _vor
####    if _vor und _nac are present, calculate Wilcoxon-Tests, effect size and power. attatch results to dfs.
####    put everything in a df outside of the logic (df_zus)

df_zus <- data.frame(FB = character(),
                     N_Skala = numeric(),
                     M_WISS_vor = numeric(),
                     SD_WISS_vor = numeric(),
                     M_WISS_nac = numeric(),
                     SD_WISS_nac = numeric(),
                     M_WISS_dff = numeric(),
                     SD_WISS_dff = numeric(),
                     p_WISS_test = numeric(),
                     d_WISS_dff = numeric(),
                     pwr_WISS_dff = numeric(),
                     stringsAsFactors = FALSE)


for (i in 1:length(fblist_num)) {
  if (any(grepl("WISS_vor", colnames(fblist_num[[i]]))) & any(grepl("WISS_nac", colnames(fblist_num[[i]])))) {
    fblist_num[[i]]$SK.WISSdiff <- fblist_num[[i]][, grepl("WISS_nac", colnames(fblist_num[[i]]))] - fblist_num[[i]][, grepl("WISS_vor", colnames(fblist_num[[i]]))]
    wsr_test <- coin::wilcoxsign_test(fblist_num[[i]][, grepl("WISS_vor", colnames(fblist_num[[i]]))] ~ fblist_num[[i]][, grepl("WISS_nac", colnames(fblist_num[[i]]))], data = fblist_num[[i]], zero.method = "Wilcoxon")
    fblist_num[[i]]$SK.WISSdiff_p <- coin::pvalue(wsr_test)

    wsr_z <- wsr_test@statistic@teststatistic
    wsr_r <- abs(wsr_z/sqrt(nrow(wsr_test@statistic@x)))
    fblist_num[[i]]$SK.WISSdiff_d <- sqrt(-4 * wsr_r^2 / (wsr_r^2 - 1))

    library(MKpower)
    rxy <- function(n) rnorm(n, mean = mean(fblist_num[[i]]$SK.WISSdiff, na.rm = T), sd(fblist_num[[i]]$SK.WISSdiff, na.rm = T))
    pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "paired", n.min = nrow(wsr_test@statistic@x),
                                 n.max = nrow(wsr_test@statistic@x)+2, step.size = 1, iter = 1000, BREAK = FALSE)
    fblist_num[[i]]$SK.WISSdiff_pwr <- pwr[["emp.power"]][[1]]

    df_zus <- rbind(df_zus, data.frame(FB = names(fblist_num)[i],
                                       N_Skala = nrow(wsr_test@statistic@x),
                                       M_WISS_vor = mean(fblist_num[[i]][, grepl("WISS_vor", colnames(fblist_num[[i]]))], na.rm = T),
                                       SD_WISS_vor = sd(fblist_num[[i]][, grepl("WISS_vor", colnames(fblist_num[[i]]))], na.rm = T),
                                       M_WISS_nac = mean(fblist_num[[i]][, grepl("WISS_nac", colnames(fblist_num[[i]]))], na.rm = T),
                                       SD_WISS_nac = sd(fblist_num[[i]][, grepl("WISS_nac", colnames(fblist_num[[i]]))], na.rm = T),
                                       M_WISS_dff = mean(fblist_num[[i]]$SK.WISSdiff, na.rm = T),
                                       SD_WISS_dff = sd(fblist_num[[i]]$SK.WISSdiff, na.rm = T),
                                       p_WISS_test = coin::pvalue(wsr_test),
                                       d_WISS_dff = sqrt(-4 * wsr_r^2 / (wsr_r^2 - 1)),
                                       pwr_WISS_dff = pwr[["emp.power"]][[1]]
    ))
  }
}

#### make descriptive statistics overall?

## make table
df_zus %>% knitr::kable()


## make table html code
library(stargazer)
stargazer(df_zus, type = "html", summary=FALSE, rownames=FALSE)









##############################################################################
### continue here to do: calculate variable for reliability (s.u.)
## to do: separate for t0 and t1
for (j in 1:length(fblist_num)) {
  fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames){
    if (any(grepl(print(k), names(fblist_num[[j]]))) == TRUE) {
      fblist_num[[j]] <- fblist_num[[j]] %>% mutate(!!paste0("SK.", k) := rowMeans(select(.,contains(!!paste0(k))),na.rm = TRUE))
    }
  }
  fblist_num[[j]] <-  fblist_num[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}

##########

for (j in seq_along(fblist_num)) {
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames) {
    if (any(grepl(k, names(fblist_num[[j]])))) {
      # Check for variable with suffix _t0
      if (any(grepl(paste0(k, "_t0"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          rowwise() %>%
          mutate(!!paste0("SK.", k, "_t0") := mean(c_across(contains(paste0(k, "_t0"))), na.rm = TRUE)) %>%
          ungroup()
      }
      # Check for variable with suffix _t1
      if (any(grepl(paste0(k, "_t1"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          rowwise() %>%
          mutate(!!paste0("SK.", k, "_t1") := mean(c_across(contains(paste0(k, "_t1"))), na.rm = TRUE)) %>%
          ungroup()
      }
      # Create composite variable for base variable
      fblist_num[[j]] <- fblist_num[[j]] %>%
        rowwise() %>%
        mutate(!!paste0("SK.", k) := mean(c_across(contains(k)), na.rm = TRUE)) %>%
        ungroup()
    }
  }
}


#####

for (j in seq_along(fblist_num)) {
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames) {
    if (any(grepl(k, names(fblist_num[[j]])))) {
      # Check for variable with suffix _t0
      if (any(grepl(paste0(k, "_t0"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t0") := rowMeans(select(., contains(paste0(k, "_t0"))), na.rm = TRUE))
      }
      # Check for variable with suffix _t1
      if (any(grepl(paste0(k, "_t1"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t1") := rowMeans(select(., contains(paste0(k, "_t1"))), na.rm = TRUE))
      }
      # Create composite variable for base variable
      fblist_num[[j]] <- fblist_num[[j]] %>%
        mutate(!!paste0("SK.", k) := rowMeans(select(., contains(k) & !contains("_t0") & !contains("_t1")), na.rm = TRUE))
    }
  }
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}


#####
for (j in 1:length(fblist_num)) {
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames) {
    if (any(grepl(k, names(fblist_num[[j]])))) {
      # Check for variable with suffix _t0
      if (any(grepl(paste0(k, "_t0"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t0") := rowMeans(select(., contains(k, "_t0")), na.rm = TRUE))
      }
      # Check for variable with suffix _t1
      if (any(grepl(paste0(k, "_t1"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t1") := rowMeans(select(., contains(k, "_t1")), na.rm = TRUE))
      }
      # Create composite variable for base variable
      fblist_num[[j]] <- fblist_num[[j]] %>%
        mutate(!!paste0("SK.", k) := rowMeans(select(., contains(k)), na.rm = TRUE))
    }
  }
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}


####
for (j in seq_along(fblist_num)) {
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames) {
    if (any(grepl(k, names(fblist_num[[j]])))) {
      # Check for variable with suffix _t0
      if (any(grepl(paste0(k, "_t0"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t0") := rowMeans(select(., contains(paste0(k, "_t0"))), na.rm = TRUE))
      }
      # Check for variable with suffix _t1
      if (any(grepl(paste0(k, "_t1"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t1") := rowMeans(select(., contains(paste0(k, "_t1"))), na.rm = TRUE))
      }
      # Create composite variable for base variable
      fblist_num[[j]] <- fblist_num[[j]] %>%
        mutate(!!paste0("SK.", k) := rowMeans(select(., contains(k)), na.rm = TRUE))
    }
  }
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}
