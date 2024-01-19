library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
library(patchwork)
library(stringr)
library(pwr)
library(dkjslama)
library(readxl)
library(openxlsx)
library(BSDA)
library(DescTools)
library(coin)
library(MKpower)
library(stringr)

#load functions
source("Stiftungsbericht_functions.R")

#########################################################################
########################## get and prepare list of all FBs ##############
#########################################################################

###create empty list for fbdata
fblist <- list()

### get vector with fbnames
#Offene Gesellschaft ohne Vielfalt
#fbnames_all <- read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx")
fbnames_all <- read_xlsx("../../DKJSBericht/DKJSBericht_Liste.xlsx") #this excel is not in the project folder


#Zukunftskompetenzen
#colnamen <-as.character(read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", n_max = 1, col_names = F))
#fbnames_all <-read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", skip = 34, col_names = colnamen)

# Alle
#fbnames_all <-read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", skip = 1, col_names = colnamen)


#fbnames_all <- read.xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx")
#fbnames <- fbnames_all %>% filter(!str_detect(Kommentar, "noch nicht online"))

#Offene Gesellschaft ohne Vielfalt
fbnames_incl <- fbnames_all %>%
  filter(Handlungsfeld == "Offene Gesellschaft") %>%
  filter(include == 1) %>%
  filter(!FBname == "BerlinerFerienschulen_Nachher_Befragung_Traeger")


fbnames <- as.vector(fbnames_incl$FBname)

### get vector with varnames
#varnames <- c("WISS_meth", "WISS", "NETZ", "XYZ.")

## get Lamapoll data for every fbname and put it into
## list fbdata with fbname as name of df
for (i in fbnames) {
  df <- as.data.frame(get.lamapoll.data(i, "we@dkjs.de", "wGl5v4fz"))
  df <- df %>%
    mutate(across(where(is.integer), as.numeric))
  fblist[[i]] <- df
}

### get data from external evaluations
## get names of files into a vector
fbnames_external <- fbnames_all %>%
  filter(Handlungsfeld == "Offene Gesellschaft") %>%
  filter(extern == 1) %>%
  .$Link
## loop through files, import xls and put into fblist
for (i in seq_along(fbnames_external)) {
  #filename <- paste0("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Aufbereitet/", fbnames_external[i])
  filename <- paste0("../../../../../Automatisierte Auswertung_Daten/Aufbereitet/", fbnames_external[i])

  df <- as.data.frame(read_xlsx(filename))
  df <- df %>%
    mutate(across(where(is.integer), as.numeric))
  fblist <- c(fblist, setNames(list(df), fbnames_external[i]))  # Set names of list elements
}


### delete suffix .xxx in all variable names in all dfs
#fblist <- lapply(fblist, function(df) {
#  names(df) <- stringr::str_remove(names(df), "\\..*")
#  df
#})

### use only numeric variables
# done: CODE muss drin bleiben
fblist_num <- list()
for (j in 1:length(fblist)) {
  df_num <- fblist[[j]] %>% select(matches("CODE") | where(is.numeric))
  fblist_num[[length(fblist_num) + 1]] <- df_num
}
names(fblist_num) <- names(fblist)

#####
fbnames_external2 <- fbnames_all %>%
  filter(Handlungsfeld == "Offene Gesellschaft") %>%
  filter(extern == 1)
fbnames_incl <- rbind(fbnames_incl, fbnames_external2)
##  ID an FBs dranheften
fbnames_incl <- fbnames_incl %>% mutate(mzp = case_when(
  t0 == 1 ~ "t0",
  t1 == 1 ~ "t1",
  t2 == 1 ~ "t2",
  tr == 1 ~ "tr",
  TRUE ~ NA_character_
))

fbnames_incl <- fbnames_incl %>%
  mutate(meta_name = paste0(`Programm-ID`,
                            "_ID", `ID`,
                            "_", `mzp`,
                            "_", `FBname`))

##  CODE Groß- und Kleinschreibung vereinheitlichen
fblist_num2 <- lapply(fblist_num, function(df) {
  # identify the relevant variables
  var_names <- names(df)[grepl("^CODE", names(df))]

  # convert uppercase characters to lowercase in each relevant variable
  for (var_name in var_names) {
    df[[var_name]] <- tolower(df[[var_name]])
  }

  # return the modified data frame
  return(df)
})

#### Rename the list Items after meta_name

#fblist_num2_names <- str_extract_all(names(fblist_num2), "(?<=_)[^_]+$")[[1]]  ##### ERROR, not only use the [[1]] #####
fblist_num2_names <- str_extract_all(names(fblist_num2), "(?<=_)[^_]+$")
fbnames_incl_names <- str_extract(fbnames_incl$meta_name, "(?<=_)[^_]+$")
names(fblist_num2)[fblist_num2_names %in% fbnames_incl_names] <- fbnames_incl$meta_name

#######################################################################
########################## Group the FBs in nested lists ##############
#######################################################################

#### Group the list into Programm-IDS
# Make function that extracts the grouping key from a name of a df
get_group_key <- function(name) {
  substr(name, start = 1, stop = 7)  # Assumes the key is the first 7 characters
}

# Use lapply to loop over the list of dataframes and group them by the key
fblist_num2_grouped <- lapply(unique(sapply(names(fblist_num2), get_group_key)), function(key) {
  subset(fblist_num2, sapply(names(fblist_num2), get_group_key) == key)
})

# Rename the sublists with their grouping keys
names(fblist_num2_grouped) <- unique(sapply(names(fblist_num2), get_group_key))

#### Group the list into Programm-ID_ID groups
fblist_num3 <- split(fblist_num2,
                     sub("^(.*?)_(.*?)_(.*)$", "\\1_\\2",
                         sapply(names(fblist_num2),
                                function(x) unlist(strsplit(x, split = ".", fixed = TRUE))[1])))


######################################################################
########################## Match and Join t0 and t1 dfs ##############
######################################################################

### matching
##  // fallausschlusskriterien z.B. wenn es mehrere gleiche gibt?
##  full join in neuen Datensatz mit Programm-ID //perspektivisch join cascade in den letzten MZP

fblist_num4 <- fblist_num3

### recode CODE variables to be the same
for (i in seq_along(fblist_num4)) {
  for (j in seq_along(fblist_num4[[i]])) {
    if ("CODE_t0" %in% names(fblist_num4[[i]][[j]])) {
      names(fblist_num4[[i]][[j]])[grep("CODE_t0", names(fblist_num4[[i]][[j]]))] <- "CODE"
    }
    if ("CODE_t1" %in% names(fblist_num4[[i]][[j]])) {
      names(fblist_num4[[i]][[j]])[grep("CODE_t1", names(fblist_num4[[i]][[j]]))] <- "CODE"
    }
    if ("CODE_tr" %in% names(fblist_num4[[i]][[j]])) {
      names(fblist_num4[[i]][[j]])[grep("CODE_tr", names(fblist_num4[[i]][[j]]))] <- "CODE"
    }
  }
}

### join dfs t0 and t1
for (i in seq_along(fblist_num4)) {
  if (length(fblist_num4[[i]]) >= 2) {
    # join dfs
    df_joined <- fblist_num4[[i]][[1]] %>%
      full_join(fblist_num4[[i]][[2]], by = "CODE")

    # Count and print positive matches in joined df
    col_x <- grep("\\.x$", names(df_joined), value = TRUE)
    col_y <- grep("\\.y$", names(df_joined), value = TRUE)
    count <- sum(rowSums(!is.na(df_joined[col_x])) > 0 & rowSums(!is.na(df_joined[col_y])) > 0)
    print(paste("nr of positive matches in", names(fblist_num4)[[i]], "=", count))

    # attach joined dfs to list
    fblist_num4[[i]][[paste0(names(fblist_num4)[[i]], "_joined")]] <- df_joined
  }
}

######################################################################
########################## Make Descriptive Statistics ###############
######################################################################

#####################################
#### for inter-individual Analysis ##
#####################################




### create dfs _vor, _nac, _ver

df_WISS_vor <- describe_variables(fblist_num3, "WISS_vor")
#View(df_WISS_vor)
df_WISS_nac <- describe_variables(fblist_num3, "WISS_nac")
#View(df_WISS_nac)
df_WISS_ver <- describe_variables(fblist_num3, "WISS_ver")
#View(df_WISS_ver)


df_FAEH_vor <- describe_variables(fblist_num3, "FAEH_vor")
#View(df_FAEH_vor)
df_FAEH_nac <- describe_variables(fblist_num3, "FAEH_nac")
#View(df_FAEH_nac)
df_FAEH_ver <- describe_variables(fblist_num3, "FAEH_ver")
#View(df_FAEH_ver)

df_ERFA_vor <- describe_variables(fblist_num3, "ERFA_vor")
#View(df_ERFA_vor)
df_ERFA_nac <- describe_variables(fblist_num3, "ERFA_nac")
#View(df_ERFA_nac)
df_ERFA_ver <- describe_variables(fblist_num3, "ERFA_ver")
#View(df_ERFA_ver)

#### join dfs and perform unpaired t-tests

###### calculate inter_stats

wiss_inter_results <- inter_ind_stats(df_WISS_vor, df_WISS_nac)
faeh_inter_results <- inter_ind_stats(df_FAEH_vor, df_FAEH_nac)
erfa_inter_results <- inter_ind_stats(df_ERFA_vor, df_ERFA_nac)

## rowbind ERFA and FAEH into one df, because the difference is obsolete and FAEH is discarded
colnames(faeh_inter_results) <- colnames(erfa_inter_results)
erfa_faeh_inter_results <- rbind(faeh_inter_results, erfa_inter_results)

## get Programmname to attach onto the dfs
#Programmname <- as.data.frame(read_excel("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", sheet = 2))
Programmname <- as.data.frame(read_excel("../../DKJSBericht/DKJSBericht_Liste.xlsx", sheet = 2))



##### match Programmname
wiss_inter_results <- match_programnames(wiss_inter_results)  ##### Camino fliegt hier raus #####
erfa_faeh_inter_results <- match_programnames(erfa_faeh_inter_results)

##### save
results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/"
results_path <- "../../Daten/Ergebnisse_aggregiert_neu_Vielfalt/"

write.xlsx(wiss_inter_results, paste0(results_path, "wiss_trendanalyse_results", "_", Sys.Date(), "_.xlsx"))
write.xlsx(erfa_faeh_inter_results, paste0(results_path, "erfa_faeh_trendanalyse_results", "_", Sys.Date(), "_.xlsx"))


#####################################
#### for intra-individual Analysis ##
#####################################

##################
#### Option 1 ####
##################

fblist_num5 <- list()

for (i in seq_along(fblist_num4)) {
  df_list <- list()
  for (j in seq_along(fblist_num4[[i]])) {
    if (is.data.frame(fblist_num4[[i]][[j]]) & (grepl("joined", names(fblist_num4[[i]])[j]))) {
      df_list[[names(fblist_num4[[i]])[j]]] <- fblist_num4[[i]][[j]]
    }
  }
  fblist_num5[[i]] <- df_list
}

fblist_num5 <- unlist(fblist_num5, recursive = FALSE)

##################
#### Option 2 ####
##################

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


##################
#### Option 1 ####
##################


wiss_intra_results <- calculate_intra_statistics(fblist_num5, "WISS")
erfa_intra_results <- calculate_intra_statistics(fblist_num5, "ERFA")
faeh_intra_results <- calculate_intra_statistics(fblist_num5, "FAEH")

# match Programmname
wiss_intra_results <- match_programnames(wiss_intra_results)
erfa_intra_results <- match_programnames(erfa_intra_results)
faeh_intra_results <- match_programnames(faeh_intra_results)


results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/"
results_path <- "../../Daten/Ergebnisse_aggregiert_neu_Vielfalt/"
write.xlsx(wiss_intra_results, paste0(results_path, "wiss_laengsschnitt_results_o1", "_", Sys.Date(), "_.xlsx"))
write.xlsx(erfa_intra_results, paste0(results_path, "erfa_laengsschnitt_results_o1", "_", Sys.Date(), "_.xlsx"))
write.xlsx(faeh_intra_results, paste0(results_path, "faeh_laengsschnitt_results_o1", "_", Sys.Date(), "_.xlsx"))

## combine ERFA und FAEH
erfa_faeh_intra_results <- rbind.data.frame(erfa_intra_results, faeh_intra_results)
write.xlsx(erfa_faeh_intra_results, paste0(results_path, "erfa_faeh_laengsschnitt_results_o1", "_", Sys.Date(), "_.xlsx"))


##################
#### Option 2 ####
##################

wiss_intra_results2 <- calculate_intra_statistics(fblist_num6, "WISS")
faeh_intra_results2 <- calculate_intra_statistics(fblist_num6, "FAEH")
erfa_intra_results2 <- calculate_intra_statistics(fblist_num6, "ERFA")

# match Programmname
wiss_intra_results2 <- match_programnames(wiss_intra_results2)
erfa_intra_results2 <- match_programnames(erfa_intra_results2)
faeh_intra_results2 <- match_programnames(faeh_intra_results2)


wiss_intra_results2 <- wiss_intra_results2 %>%
  rename(`Programm-ID2` = `FB`)
erfa_intra_results2 <- erfa_intra_results2 %>%
  rename(`Programm-ID2` = `FB`)
faeh_intra_results2 <- faeh_intra_results2 %>%
  rename(`Programm-ID2` = `FB`)


## get Programmname to attach onto the dfs
library(readxl)
Programm <- as.data.frame(read_excel("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", sheet = 1))
Programm <- as.data.frame(read_excel("../../DKJSBericht/DKJSBericht_Liste.xlsx", sheet = 1))

Programm$Programm_ID2 <- paste0(Programm$`Programm-ID`, "_ID", Programm$ID)
Programm$Programmname2 <- paste0(Programm$Programmname, "_ID", Programm$ID)


# First, extract the prefix from FB into a new column called "prefix"
wiss_intra_results2$"Programm-ID2" <- sub("^(.*?_[^_]*?)_.*$", "\\1", wiss_intra_results2$`Programm-ID2`)
# extract the matching elements of Programmname based on Programm-ID2 into wiss_intra_statistics
wiss_intra_results2$Programm <- Programm$Programmname2[match(wiss_intra_results2$`Programm-ID2`, Programm$Programm_ID2)]

# First, extract the prefix from FB into a new column called "prefix"
erfa_intra_results2$"Programm-ID2" <- sub("^(.*?_[^_]*?)_.*$", "\\1", erfa_intra_results2$`Programm-ID2`)
# extract the matching elements of Programmname based on Programm-ID2 into wiss_intra_statistics
erfa_intra_results2$Programm <- Programm$Programmname2[match(erfa_intra_results2$`Programm-ID2`, Programm$Programm_ID2)]

# First, extract the prefix from FB into a new column called "prefix"
faeh_intra_results2$"Programm-ID2" <- sub("^(.*?_[^_]*?)_.*$", "\\1", faeh_intra_results2$`Programm-ID2`)
# extract the matching elements of Programmname based on Programm-ID2 into wiss_intra_statistics
faeh_intra_results2$Programm <- Programm$Programmname2[match(faeh_intra_results2$`Programm-ID2`, Programm$Programm_ID2)]

## combine ERFA und FAEH
erfa_faeh_intra_results2 <- rbind.data.frame(erfa_intra_results2, faeh_intra_results2)

#results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/"
write.xlsx(erfa_faeh_intra_results, paste0(results_path, "erfa_faeh_laengsschnitt_results_o2", "_", Sys.Date(), "_.xlsx"))

wiss_intra_results2$Konstrukt <- NULL
erfa_intra_results2$Konstrukt <- NULL
faeh_intra_results2$Konstrukt <- NULL

write.xlsx(wiss_intra_results2, paste0(results_path, "wiss_laengsschnitt_results_o2", "_", Sys.Date(), "_.xlsx"))
write.xlsx(erfa_intra_results2, paste0(results_path, "erfa_laengsschnitt_results_o2", "_", Sys.Date(), "_.xlsx"))
write.xlsx(faeh_intra_results2, paste0(results_path, "faeh_laengsschnitt_results_o2", "_", Sys.Date(), "_.xlsx"))


##################
#### Option 3 ####
##################


#########

fblist_num7 <- list()

for (i in seq_along(fblist_num4)) {
  df_list <- list()
  for (j in seq_along(fblist_num4[[i]])) {
    if ((is.data.frame(fblist_num4[[i]][[j]])
         & (grepl("joined", names(fblist_num4[[i]])[j]))
         | grepl("_tr", names(fblist_num4[[i]])[j]))) {
      df_list[[names(fblist_num4[[i]])[j]]] <- fblist_num4[[i]][[j]]
    }
  }
  fblist_num7[[i]] <- df_list
}

fblist_num7 <- unlist(fblist_num7, recursive = FALSE)
View(fblist_num7)


#####

calculate_scales_bericht <- function(variable_name) {
  for (i in seq_along(fblist_num7)) {
    variables <- grep(paste0("^", variable_name), names(fblist_num7[[i]]), value = TRUE)
    fblist_num7[[i]] <- fblist_num7[[i]] %>%
      mutate(!!paste0("SK.", variable_name) := rowMeans(select(., any_of(variables)), na.rm = TRUE)) %>%
      select(names(fblist_num7[[i]]), paste0("SK.", variable_name))
  }
  return(fblist_num7)
}

fblist_num7 <- calculate_scales_bericht("WISS_vor")
fblist_num7 <- calculate_scales_bericht("WISS_nac")

fblist_num7 <- calculate_scales_bericht("ERFA_vor")
fblist_num7 <- calculate_scales_bericht("ERFA_nac")

fblist_num7 <- calculate_scales_bericht("FAEH_vor")
fblist_num7 <- calculate_scales_bericht("FAEH_nac")


for (i in seq_along(fblist_num7)) {
  # Select only the variables that start with "SK."
  fblist_num7[[i]] <- fblist_num7[[i]] %>% select(starts_with("SK.") | starts_with("CODE") )
}

# replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
library(dplyr)
for (i in seq_along(fblist_num7)) {
  fblist_num7[[i]] <- fblist_num7[[i]] %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))
}


# select all but everything-NA variables
for (i in seq_along(fblist_num7)) {
  fblist_num7[[i]] <- fblist_num7[[i]] %>%
    select_if(~!all(is.na(.)))
}


wiss_intra_statistics <- calculate_intra_statistics_scales("WISS")
erfa_intra_statistics <- calculate_intra_statistics_scales("ERFA")
faeh_intra_statistics <- calculate_intra_statistics_scales("FAEH")


wiss_intra_statistics <- wiss_intra_statistics %>%
  rename(`Programm-ID2` = `FB`)
erfa_intra_statistics <- erfa_intra_statistics %>%
  rename(`Programm-ID2` = `FB`)
faeh_intra_statistics <- faeh_intra_statistics %>%
  rename(`Programm-ID2` = `FB`)

## get Programmname to attach onto the dfs
library(readxl)
Programm <- as.data.frame(read_excel("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", sheet = 1))
Programm$Programm_ID2 <- paste0(Programm$`Programm-ID`, "_ID", Programm$ID)
Programm$Programmname2 <- paste0(Programm$Programmname, "_ID", Programm$ID)


# First, extract the prefix from FB into a new column called "prefix"
wiss_intra_statistics$"Programm-ID2" <- sub("^(.*?_[^_]*?)_.*$", "\\1", wiss_intra_statistics$`Programm-ID2`)
# extract the matching elements of Programmname based on Programm-ID2 into wiss_intra_statistics
wiss_intra_statistics$Programm <- Programm$Programmname2[match(wiss_intra_statistics$`Programm-ID2`, Programm$Programm_ID2)]

# First, extract the prefix from FB into a new column called "prefix"
erfa_intra_statistics$"Programm-ID2" <- sub("^(.*?_[^_]*?)_.*$", "\\1", erfa_intra_statistics$`Programm-ID2`)
# extract the matching elements of Programmname based on Programm-ID2 into wiss_intra_statistics
erfa_intra_statistics$Programm <- Programm$Programmname2[match(erfa_intra_statistics$`Programm-ID2`, Programm$Programm_ID2)]

# First, extract the prefix from FB into a new column called "prefix"
faeh_intra_statistics$"Programm-ID2" <- sub("^(.*?_[^_]*?)_.*$", "\\1", faeh_intra_statistics$`Programm-ID2`)
# extract the matching elements of Programmname based on Programm-ID2 into wiss_intra_statistics
faeh_intra_statistics$Programm <- Programm$Programmname2[match(faeh_intra_statistics$`Programm-ID2`, Programm$Programm_ID2)]

## combine ERFA und FAEH
erfa_faeh_intra_statistics <- rbind.data.frame(erfa_intra_statistics,faeh_intra_statistics)

results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/"
write.xlsx(erfa_faeh_intra_statistics, paste0(results_path, "erfa_faeh_laengsschnitt_results_o3", "_", Sys.Date(), "_.xlsx"))


wiss_intra_statistics$Konstrukt <- NULL
erfa_intra_statistics$Konstrukt <- NULL
faeh_intra_statistics$Konstrukt <- NULL


results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/"
write.xlsx(wiss_intra_statistics, paste0(results_path, "wiss_laengsschnitt_results_o3", "_", Sys.Date(), "_.xlsx"))
write.xlsx(erfa_intra_statistics, paste0(results_path, "erfa_laengsschnitt_results_o3", "_", Sys.Date(), "_.xlsx"))
write.xlsx(faeh_intra_statistics, paste0(results_path, "faeh_laengsschnitt_results_o3", "_", Sys.Date(), "_.xlsx"))











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
