library(dplyr)
library(psych)
library(ggplot2)
#library(tidyr)
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


#########################################################################
########################## get and prepare list of all FBs ##############
#########################################################################

### !! There are two excels as base data. DKJSBericht_Liste.xlsx for the meta-analysis with just the _vor _nac variables.
### !! And Programmliste.xlsx for the fully recoded FBs. Choose accordingly.


###create empty list for fbdata
fblist <- list()

### get vector with fbnames
#fbnames_all <-read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste2.xlsx")
#fbnames_all <- read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx")


#Zukunftskompetenzen
colnamen <-as.character(read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Programmliste.xlsx", n_max = 1, col_names = F))
fbnames_all <-read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Programmliste.xlsx", col_names = colnamen)
fbnames_incl <- fbnames_all %>% filter(Handlungsfeld == "Zukunftskompetenzen") %>% filter(include == 1)


# Alle
#fbnames_all <-read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", skip = 1, col_names = colnamen)


#fbnames_all <- read.xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx")
#fbnames <- fbnames_all %>% filter(!str_detect(Kommentar, "noch nicht online"))

#Offene Gesellschaft ohne Vielfalt
#fbnames_incl <- fbnames_all %>% filter(Handlungsfeld == "Offene Gesellschaft") %>% filter(include == 1)
#fbnames_incl <- fbnames_incl %>% filter(!FBname == "BerlinerFerienschulen_Nachher_Befragung_Traeger")


fbnames <- as.vector(fbnames_incl$FBname)

### get vector with varnames
#varnames <- c("WISS_meth", "WISS", "NETZ", "XYZ.")

## get Lamapoll data for every fbname and put it into list fbdata with fbname as name of df
for (i in fbnames) {
  df <- as.data.frame(get.lamapoll.data(i, "we@dkjs.de", "wGl5v4fz"))
  df <- df %>%
    mutate(across(where(is.integer), as.numeric))
  fblist[[i]] <- df
}

### get data from external evaluations
## get names of files into a vector
fbnames_external <- fbnames_all %>% filter(Handlungsfeld == "Offene Gesellschaft") %>% filter(extern == 1) %>% .$Link
## loop through files, import xls and put into fblist
for (i in seq_along(fbnames_external)) {
  filename <- paste0("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Aufbereitet/", fbnames_external[i])
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

##### attach external evaluations
fbnames_external2 <- fbnames_all %>% filter(Handlungsfeld == "Offene Gesellschaft") %>% filter(extern == 1)
fbnames_incl <- rbind(fbnames_incl, fbnames_external2)

##  ID an FBs dranheften
fbnames_incl <- fbnames_incl %>% mutate(mzp = case_when(
  t0 == 1 ~ "t0",
  t1 == 1 ~ "t1",
  t2 == 1 ~ "t2",
  tr == 1 ~ "tr",
  TRUE ~ NA_character_
))

##
fbnames_incl <- fbnames_incl %>% mutate(meta_name = paste0(`Programm-ID`, "_ID", `ID`, "_", `mzp`, "_", `FBname`))

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
library(stringr)
fblist_num2_names <- str_extract_all(names(fblist_num2), "(?<=_)[^_]+$")[[1]]
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
library(stringr)
fblist_num3 <- split(fblist_num2, sub("^(.*?)_(.*?)_(.*)$", "\\1_\\2", sapply(names(fblist_num2), function(x) unlist(strsplit(x, split = ".", fixed = TRUE))[1])))


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

### join dfs t0 and t1 ..... retired, use code chunk below
#for (i in seq_along(fblist_num4)) {
#  if (length(fblist_num4[[i]]) >= 2) {
#    # join dfs
#    df_joined <- fblist_num4[[i]][[1]] %>%
#      full_join(fblist_num4[[i]][[2]], by = "CODE")
#
#    # Count and print positive matches in joined df
#    col_x <- grep("\\.x$", names(df_joined), value = TRUE)
#    col_y <- grep("\\.y$", names(df_joined), value = TRUE)
#    count <- sum(rowSums(!is.na(df_joined[col_x])) > 0 & rowSums(!is.na(df_joined[col_y])) > 0)
#      print(paste("nr of positive matches in", names(fblist_num4)[[i]], "=", count))
#
#    # attach joined dfs to list
#    fblist_num4[[i]][[paste0(names(fblist_num4)[[i]], "_joined")]] <- df_joined
#    }
#}


for (i in seq_along(fblist_num4)) {
  if (length(fblist_num4[[i]]) >= 2 && "CODE" %in% names(fblist_num4[[i]][[1]]) && "CODE" %in% names(fblist_num4[[i]][[2]])) {
    # join dfs
      df_joined <- fblist_num4[[i]][[1]] %>%
        full_join(fblist_num4[[i]][[2]], by = "CODE")

      # Count and print positive matches in the joined df
      col_x <- grep("\\.x$", names(df_joined), value = TRUE)
      col_y <- grep("\\.y$", names(df_joined), value = TRUE)
      count <- sum(rowSums(!is.na(df_joined[col_x])) > 0 & rowSums(!is.na(df_joined[col_y])) > 0)
      print(paste("nr of positive matches in", names(fblist_num4)[[i]], "=", count))

      # Attach joined dfs to the list
      joined_name <- paste0(names(fblist_num4)[[i]], "_joined")
      fblist_num4[[i]][[joined_name]] <- df_joined
    } else {
      print(paste("No matching df or variable 'CODE' not found in", names(fblist_num4)[[i]]))
    }
}




######################################################################
########################## Make Descriptive Statistics ###############
######################################################################

#####################################
#### for inter-individual Analysis ##
#####################################


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
inter_ind_stats <- function(df_vor, df_nac) {

  # Extract the first digit from Item_vor and Item_nac
  df_vor$digit <- str_extract(df_vor[,2], "\\d")
  df_nac$digit <- str_extract(df_nac[,2], "\\d")

  # Perform a full join based on FB and digit
  df_inter <- full_join(df_vor, df_nac, by = c("FB", "digit"))

  ###
  # unpaired t-tests _vor vs. _nac

  # filter out rows with missing values in either _vor or _nac
  df_inter_filtered <- df_inter[complete.cases(df_inter[, grepl("_vor|_nac", names(df_inter))]), ]

  # identify the columns containing "N", "M", and "SD"
  n_vor_col <- grep("N.*vor", names(df_inter_filtered))
  m_vor_col <- grep("M.*vor", names(df_inter_filtered))
  sd_vor_col <- grep("SD.*vor", names(df_inter_filtered))

  n_nac_col <- grep("N.*nac", names(df_inter_filtered))
  m_nac_col <- grep("M.*nac", names(df_inter_filtered))
  sd_nac_col <- grep("SD.*nac", names(df_inter_filtered))

  # loop over the rows of the data frame and perform the t-test
  t_test_results <- vector("list", nrow(df_inter_filtered))
  for (i in 1:nrow(df_inter_filtered)) {
    t_test_results[[i]] <- tsum.test(n.x =  df_inter_filtered[i, n_vor_col],
                                     n.y = df_inter_filtered[i, n_nac_col],
                                     mean.x = df_inter_filtered[i, m_vor_col],
                                     mean.y = df_inter_filtered[i, m_nac_col],
                                     s.x = df_inter_filtered[i, sd_vor_col],
                                     s.y = df_inter_filtered[i, sd_nac_col],
                                     var.equal = FALSE)
    # calculate M_diff and add to the original df
    df_inter_filtered[i, "M_diff"] <- df_inter_filtered[i, m_nac_col] - df_inter_filtered[i, m_vor_col]

    # attach the p-value to the original df
    df_inter_filtered[i, "p_wert"] <- t_test_results[[i]]["p.value"]

    # calculate effect size and attach to original df
    #df_inter_filtered[i, "d_wert"] <- abs(t_test_results[[1]][["statistic"]][["t"]]*sqrt((df_inter_filtered[i, n_vor_col] + df_inter_filtered[i, n_nac_col])/(df_inter_filtered[i, n_vor_col] * df_inter_filtered[i, n_nac_col])))

    # calcluate effect size d and pwr and add to the original df
    # for d use pooled SD from Cohen, Jacob (1988). Statistical Power Analysis for the Behavioral Sciences. Routledge. ISBN 978-1-134-74270-7
    # see also here for the formula: https://en.wikipedia.org/wiki/Effect_size#Cohen.27s_d
    m1 <- df_inter_filtered[i, m_vor_col]
    m2 <- df_inter_filtered[i, m_nac_col]
    sd1 <- df_inter_filtered[i, sd_vor_col]
    sd2 <- df_inter_filtered[i, sd_nac_col]
    n1 <- df_inter_filtered[i, n_vor_col]
    n2 <- df_inter_filtered[i, n_nac_col]

    lx <- n1- 1
    ly <- n2- 1
    md  <- abs(m1-m2)
    pooled_sd <- lx * sd1^2 + ly * sd2^2
    pooled_sd <- pooled_sd/(lx + ly)
    pooled_sd <- sqrt(pooled_sd)                     ## common sd computation
    d  <- md/pooled_sd                        ## cohen's d
    df_inter_filtered[i, "d_wert"] <-  d

    n <-  (df_inter_filtered[i, n_vor_col])
    a <- pwr.t.test(d = d, n = n, type = "two.sample", alternative = "two.sided")
    df_inter_filtered[i, "pwr_wert"] <- a$power

  }

  # join to original df
  df_inter2 <- df_inter %>% full_join(df_inter_filtered, by = c("FB", "digit"))

  ## to do: the next lines should not be necessary, but the full_join  weirdly duplicates columns
  # get column indices of variables ending with ".y" except p_wert
  cols_to_remove <- grep("\\.y$(?!.*p_wert)(?!.*d_wert)(?!.*pwr_wert)", names(df_inter2), perl = TRUE)
  # remove columns from data frame
  df_inter2 <- df_inter2[, -cols_to_remove]
  # remove .x from names
  names(df_inter2) <- sub("\\..*", "", names(df_inter2))

  # view the results
  df_inter2$digit <- NULL
  return(df_inter2)
}


###### calculate inter_stats

wiss_inter_results <- inter_ind_stats(df_WISS_vor, df_WISS_nac)
faeh_inter_results <- inter_ind_stats(df_FAEH_vor, df_FAEH_nac)
erfa_inter_results <- inter_ind_stats(df_ERFA_vor, df_ERFA_nac)

## rowbind ERFA and FAEH into one df, because the difference is obsolete and FAEH is discarded
colnames(faeh_inter_results) = colnames(erfa_inter_results)
erfa_faeh_inter_results <- rbind(faeh_inter_results, erfa_inter_results)

## get Programmname to attach onto the dfs
Programmname <- as.data.frame(read_excel("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx", sheet = 2))

# define matching function
match_programnames <- function(df_inter_results){
  # First, extract the prefix from FB into a new column called "prefix"
  df_inter_results$"Programm-ID" <- gsub("_.*", "", df_inter_results$FB)

  # Next, merge the two data frames based on the "Programm-ID" column
  merged_df <- merge(df_inter_results, Programmname, by = "Programm-ID")

return(merged_df)
}

##### match Programmname
wiss_inter_results <- match_programnames(wiss_inter_results)
erfa_faeh_inter_results <- match_programnames(erfa_faeh_inter_results)


##### save
results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/"

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

############## better way? seems very dependend on the specific dimensions of dfs and order of variables
##### probably not really NA robust and generalizable, but works so far, for WISS and ERFA!

calculate_intra_statistics <- function(fbliste, construct) {
  con_vor <- paste0(construct, "_vor")
  con_nac <- paste0(construct, "_nac")

  gr_vor <- paste0(construct, ".*_vor")
  gr_nac <- paste0(construct, ".*_nac")

  #If there are no variables with the construct, delete the df from the list and print a message
  #Loop through each index
  fbliste2 <- list ()
  for (i in seq_along(fbliste)) {
    if((any(grepl(gr_vor, names(fbliste[[i]]))) & any(grepl(gr_nac, names(fbliste[[i]]))))) {
      df_name <- names(fbliste)[i]
      fbliste2[[df_name]] <- fbliste[[i]]
    }
  }

  fbliste <- fbliste2

  # print message if list is now empty
  if (length(fbliste) == 0) {
    message("There are no dfs in the list with this construct name. Sorry.")
  }

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

    # Extract the variables from the data frame in fbliste
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
                       d_value = numeric(),
                       pwr_test = numeric(),
                       kendall = numeric(),
                       stringsAsFactors = FALSE)




  for (i in 1:length(extracted_data)) {
    if (any(grepl(con_vor, colnames(extracted_data[[i]]))) & any(grepl(con_nac, colnames(extracted_data[[i]])))) {
      nachher <- extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))]
      vorher <- extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))]

      extracted_data[[i]]$SK.diff <- nachher - vorher
      ##if any row in extracted_data[[i]]$SK.diff > 0, else NA

      df_zus <-  tryCatch({

        wsr_test <- coin::wilcoxsign_test(vorher ~ nachher)
        kendall_t <- Kendall::Kendall(vorher, nachher)

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

        library(MKpower)      ### the power simulation works on a minimum of n >= 6, when n < 6 the working minimum gets applied
        pwr_m <- mean(extracted_data[[i]]$SK.diff, na.rm = T)
        pwr_sd <- sd(extracted_data[[i]]$SK.diff, na.rm = T)
        rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
        pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x)-1,
                                     n.max = nrow(wsr_test@statistic@x)+5, step.size = 1, iter = 1000, BREAK = FALSE)
        pwr_df <- as.data.frame(cbind(pwr[["n"]], pwr[["emp.power"]]))
        target_n <- nrow(wsr_test@statistic@x)
        closest_row <- which.min(abs(pwr_df$V1 - target_n))
        closest_value <- pwr_df[closest_row, "V2"]
        closest_value
        extracted_data[[i]]$SK.diff_pwr <- closest_value

        df_zus <- rbind(df_zus, data.frame(FB = names(extracted_data)[i],
                                           Konstrukt = paste0(construct),
                                           N_intra = sum(!is.na(extracted_data[[i]][["SK.diff"]])),
                                           M_vor = mean(vorher, na.rm = T),
                                           SD_vor = sd(vorher, na.rm = T),
                                           M_nac = mean(nachher, na.rm = T),
                                           SD_nac = sd(nachher, na.rm = T),
                                           M_dff = mean(extracted_data[[i]]$SK.diff, na.rm = T),
                                           SD_dff = sd(extracted_data[[i]]$SK.diff, na.rm = T),
                                           p_test = extracted_data[[i]]$SK.diff_p[1],
                                           d_value = d,
                                           pwr_test = closest_value,
                                           kendall = kendall_t[["tau"]]
        ))
      }, error = function(e) {
        # If an error occurs, assign NA to wsr_test and print error message
        #cat("Error occurred while running wilcoxsign_test on iteration ", i, ":\n", conditionMessage(e), "\n")
        error_message <- conditionMessage(e)
        cat(error_message)  # this can be deleted after debugging is completed

        if (any(grepl("pairwise differences equal zero", error_message))) {
          cat("Error: Specific Error 'all pairwise differences equal zero' occurred. Action taken: Theoretical Values assigned. \n")
          kendall_t <- Kendall::Kendall(vorher, nachher)
          df_zus <- rbind(df_zus, data.frame(FB = names(extracted_data)[i],
                                             Konstrukt = paste0(construct),
                                             N_intra = sum(!is.na(extracted_data[[i]][["SK.diff"]])),
                                             M_vor = mean(vorher, na.rm = T),
                                             SD_vor = sd(vorher, na.rm = T),
                                             M_nac = mean(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                             SD_nac = sd(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                             M_dff = mean(extracted_data[[i]]$SK.diff, na.rm = T),
                                             SD_dff = sd(extracted_data[[i]]$SK.diff, na.rm = T),
                                             p_test = .9999999,
                                             d_value = 0,
                                             pwr_test = 0,
                                             kendall = kendall_t[["tau"]]
          ))
          return(df_zus)

        } else if (any(grepl("subscript out of bounds", error_message))) {
          cat("Error: Specific Error 'subscript out of bounds' occurred. Action taken: closest values from a power simulation \n")
          wsr_test <- coin::wilcoxsign_test(vorher ~ nachher)
          wsr_z <- abs(wsr_test@statistic@teststatistic)
          wsr_r <- wsr_z/sqrt(nrow(wsr_test@statistic@x))
          d <- (sqrt(4) * wsr_r)/(sqrt(1-wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner
          library(MKpower)
          pwr_m <- mean(extracted_data[[i]]$SK.diff, na.rm = T)
          pwr_sd <- sd(extracted_data[[i]]$SK.diff, na.rm = T)
          rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
          pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x)-1,
                                       n.max = nrow(wsr_test@statistic@x)+5, step.size = 1, iter = 1000, BREAK = FALSE)
          pwr_df <- as.data.frame(cbind(pwr[["n"]], pwr[["emp.power"]]))
          target_n <- nrow(wsr_test@statistic@x)
          closest_row <- which.min(abs(pwr_df$V1 - target_n))
          closest_value <- pwr_df[closest_row, "V2"]
          closest_value
          extracted_data[[i]]$SK.diff_pwr <- closest_value

          kendall_t <- Kendall::Kendall(vorher, nachher)
          df_zus <- rbind(df_zus, data.frame(FB = names(extracted_data)[i],
                                             Konstrukt = paste0(construct),
                                             N_intra = sum(!is.na(extracted_data[[i]][["SK.diff"]])),
                                             M_vor = mean(vorher, na.rm = T),
                                             SD_vor = sd(vorher, na.rm = T),
                                             M_nac = mean(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                             SD_nac = sd(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                             M_dff = mean(extracted_data[[i]]$SK.diff, na.rm = T),
                                             SD_dff = sd(extracted_data[[i]]$SK.diff, na.rm = T),
                                             p_test = coin::pvalue(wsr_test),
                                             d_value = d,
                                             pwr_test = closest_value,
                                             kendall = kendall_t[["tau"]]
          ))
          return(df_zus)
        }
        kendall_t <- Kendall::Kendall(extracted_data[[i]][, grepl(con_vor, colnames(extracted_data[[i]]))], extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))])
        df_zus <- rbind(df_zus, data.frame(FB = names(extracted_data)[i],
                                           Konstrukt = paste0(construct),
                                           N_intra = sum(!is.na(extracted_data[[i]][["SK.diff"]])),
                                           M_vor = mean(vorher, na.rm = T),
                                           SD_vor = sd(vorher, na.rm = T),
                                           M_nac = mean(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                           SD_nac = sd(extracted_data[[i]][, grepl(con_nac, colnames(extracted_data[[i]]))], na.rm = T),
                                           M_dff = mean(extracted_data[[i]]$SK.diff, na.rm = T),
                                           SD_dff = sd(extracted_data[[i]]$SK.diff, na.rm = T),
                                           p_test = NA,
                                           d_value = NA,
                                           pwr_test = NA,
                                           kendall = kendall_t[["tau"]]
        ))
        return(df_zus)
      })

    }
  }
  df_zus <- df_zus[!duplicated(df_zus[,c("FB", "Konstrukt", "N_intra", "M_vor", "SD_vor", "M_nac", "SD_nac", "M_dff", "SD_dff", "kendall")]),]
  return(df_zus)
}

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
write.xlsx(wiss_intra_results, paste0(results_path, "wiss_laengsschnitt_results_o1", "_", Sys.Date(), "_.xlsx"))
write.xlsx(erfa_intra_results, paste0(results_path, "erfa_laengsschnitt_results_o1", "_", Sys.Date(), "_.xlsx"))
write.xlsx(faeh_intra_results, paste0(results_path, "faeh_laengsschnitt_results_o1", "_", Sys.Date(), "_.xlsx"))

## combine ERFA und FAEH
erfa_faeh_intra_results <- rbind.data.frame(erfa_intra_results,faeh_intra_results)
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
erfa_faeh_intra_results2 <- rbind.data.frame(erfa_intra_results2,faeh_intra_results2)

results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/"
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
    if ((is.data.frame(fblist_num4[[i]][[j]]) & (grepl("joined", names(fblist_num4[[i]])[j])) | grepl("_tr", names(fblist_num4[[i]])[j]))) {
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


calculate_intra_statistics_scales <- function(construct) {

  con_vor <- paste0(construct, "_vor")
  con_nac <- paste0(construct, "_nac")

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

  for (i in 1:length(fblist_num7)) {
    if (any(grepl(con_vor, colnames(fblist_num7[[i]]))) & any(grepl(con_nac, colnames(fblist_num7[[i]])))) {
      nachher <- fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))]
      vorher <- fblist_num7[[i]][, grepl(con_vor, colnames(fblist_num7[[i]]))]
      #fblist_num7[[i]]$SK.diff <- fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))] - fblist_num7[[i]][, grepl(con_vor, colnames(fblist_num7[[i]]))]
      fblist_num7[[i]]$SK.diff <- nachher - vorher

      df_zus <-  tryCatch({

        wsr_test <- coin::wilcoxsign_test(vorher ~ nachher)
        kendall_t <- Kendall::Kendall(vorher, nachher)

        fblist_num7[[i]]$SK.diff_p <- coin::pvalue(wsr_test)

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
        pwr_m <- mean(fblist_num7[[i]]$SK.diff, na.rm = T)
        pwr_sd <- sd(fblist_num7[[i]]$SK.diff, na.rm = T)
        rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
        pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x)-1,
                                     n.max = nrow(wsr_test@statistic@x)+5, step.size = 1, iter = 1000, BREAK = FALSE)
        pwr_df <- as.data.frame(cbind(pwr[["n"]], pwr[["emp.power"]]))
        target_n <- nrow(wsr_test@statistic@x)
        closest_row <- which.min(abs(pwr_df$V1 - target_n))
        closest_value <- pwr_df[closest_row, "V2"]
        closest_value
        fblist_num7[[i]]$SK.diff_pwr <- closest_value

        df_zus <- rbind(df_zus, data.frame(FB = names(fblist_num7)[i],
                                           Konstrukt = paste0(construct),
                                           N_intra = sum(!is.na(fblist_num7[[i]][["SK.diff"]])),
                                           M_vor = mean(vorher, na.rm = T),
                                           SD_vor = sd(vorher, na.rm = T),
                                           M_nac = mean(nachher, na.rm = T),
                                           SD_nac = sd(nachher, na.rm = T),
                                           M_dff = mean(fblist_num7[[i]]$SK.diff, na.rm = T),
                                           SD_dff = sd(fblist_num7[[i]]$SK.diff, na.rm = T),
                                           p_test = fblist_num7[[i]]$SK.diff_p[1],
                                           d_value = d,
                                           pwr_test = closest_value,
                                           kendall = kendall_t[["tau"]]
        ))
      }, error = function(e) {
        # If an error occurs, assign NA to wsr_test and print error message
        #cat("Error occurred while running wilcoxsign_test on iteration ", i, ":\n", conditionMessage(e), "\n")
        error_message <- conditionMessage(e)
        cat(error_message)  # this can be deleted after debugging is completed

        if (any(grepl("pairwise differences equal zero", error_message))) {
          cat("Error: Specific Error 'all pairwise differences equal zero' occurred.Action taken: Theoretical Values assigned. \n")
          kendall_t <- Kendall::Kendall(vorher, nachher)
          df_zus <- rbind(df_zus, data.frame(FB = names(fblist_num7)[i],
                                             Konstrukt = paste0(construct),
                                             N_intra = sum(!is.na(fblist_num7[[i]][["SK.diff"]])),
                                             M_vor = mean(vorher, na.rm = T),
                                             SD_vor = sd(vorher, na.rm = T),
                                             M_nac = mean(fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))], na.rm = T),
                                             SD_nac = sd(fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))], na.rm = T),
                                             M_dff = mean(fblist_num7[[i]]$SK.diff, na.rm = T),
                                             SD_dff = sd(fblist_num7[[i]]$SK.diff, na.rm = T),
                                             p_test = .9999999,
                                             d_value = 0,
                                             pwr_test = 0,
                                             kendall = kendall_t[["tau"]]
          ))
          return(df_zus)

        } else if (any(grepl("subscript out of bounds", error_message))) {
          cat("Error: Specific Error 'subscript out of bounds' occurred. Action taken: ??? \n")
          wsr_test <- coin::wilcoxsign_test(vorher ~ nachher)
          wsr_z <- abs(wsr_test@statistic@teststatistic)
          wsr_r <- wsr_z/sqrt(nrow(wsr_test@statistic@x))
          d <- (sqrt(4) * wsr_r)/(sqrt(1-wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner
          library(MKpower)
          pwr_m <- mean(fblist_num7[[i]]$SK.diff, na.rm = T)
          pwr_sd <- sd(fblist_num7[[i]]$SK.diff, na.rm = T)
          rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
          pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x)-1,
                                       n.max = nrow(wsr_test@statistic@x)+5, step.size = 1, iter = 1000, BREAK = FALSE)
          pwr_df <- as.data.frame(cbind(pwr[["n"]], pwr[["emp.power"]]))
          target_n <- nrow(wsr_test@statistic@x)
          closest_row <- which.min(abs(pwr_df$V1 - target_n))
          closest_value <- pwr_df[closest_row, "V2"]
          closest_value
          fblist_num7[[i]]$SK.diff_pwr <- closest_value

          kendall_t <- Kendall::Kendall(vorher, nachher)
          df_zus <- rbind(df_zus, data.frame(FB = names(fblist_num7)[i],
                                             Konstrukt = paste0(construct),
                                             N_intra = sum(!is.na(fblist_num7[[i]][["SK.diff"]])),
                                             M_vor = mean(vorher, na.rm = T),
                                             SD_vor = sd(vorher, na.rm = T),
                                             M_nac = mean(fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))], na.rm = T),
                                             SD_nac = sd(fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))], na.rm = T),
                                             M_dff = mean(fblist_num7[[i]]$SK.diff, na.rm = T),
                                             SD_dff = sd(fblist_num7[[i]]$SK.diff, na.rm = T),
                                             p_test = coin::pvalue(wsr_test),
                                             d_value = d,
                                             pwr_test = closest_value,
                                             kendall = kendall_t[["tau"]]
          ))
          return(df_zus)
        }
        kendall_t <- Kendall::Kendall(fblist_num7[[i]][, grepl(con_vor, colnames(fblist_num7[[i]]))], fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))])
        df_zus <- rbind(df_zus, data.frame(FB = names(fblist_num7)[i],
                                           Konstrukt = paste0(construct),
                                           N_intra = sum(!is.na(fblist_num7[[i]][["SK.diff"]])),
                                           M_vor = mean(vorher, na.rm = T),
                                           SD_vor = sd(vorher, na.rm = T),
                                           M_nac = mean(fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))], na.rm = T),
                                           SD_nac = sd(fblist_num7[[i]][, grepl(con_nac, colnames(fblist_num7[[i]]))], na.rm = T),
                                           M_dff = mean(fblist_num7[[i]]$SK.diff, na.rm = T),
                                           SD_dff = sd(fblist_num7[[i]]$SK.diff, na.rm = T),
                                           p_test = NA,
                                           d_value = NA,
                                           pwr_test = NA,
                                           kendall = kendall_t[["tau"]]
        ))
        return(df_zus)
      })

    }
  }
  #df_zus <- df_zus[!duplicated(df_zus[,c("FB", "Konstrukt", "N_intra", "M_vor", "SD_vor", "M_nac", "SD_nac", "M_dff", "SD_dff", "kendall")]),]
  return(df_zus)
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
