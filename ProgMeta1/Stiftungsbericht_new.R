#### to do: integrate Vielfalt Daten... export lamapoll as xlsx, add data manually, import new xlsx

library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
library(patchwork)
library(stringr)
library(pwr)
library(dkjslama)
library(readxl)
library(BSDA)
library(coin)


#########################################################################
########################## get and prepare list of all FBs ##############
#########################################################################

###create empty list for fbdata
fblist <- list()

### get vector with fbnames
fbnames_all <-read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste2.xlsx")
#fbnames_all <- read.xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/DKJSBericht/DKJSBericht_Liste.xlsx")
#fbnames <- fbnames_all %>% filter(!str_detect(Kommentar, "noch nicht online"))
fbnames_incl <- fbnames_all %>% filter(include == 1)
fbnames_incl <- fbnames_incl %>% filter(!FBname == "BerlinerFerienschulen_Nachher_Befragung_Traeger")
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

#####
##  ID an FBs dranheften
fbnames_incl <- fbnames_incl %>% mutate(mzp = case_when(
  t0 == 1 ~ "t0",
  t1 == 1 ~ "t1",
  t2 == 1 ~ "t2",
  tr == 1 ~ "tr",
  TRUE ~ NA_character_
))

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
# Create a function that extracts the grouping key from a dataframe name
get_group_key <- function(name) {
  substr(name, start = 1, stop = 7)  # Assumes the key is the first 7 characters
}

# Use lapply to loop over the list of dataframes and group them by the key
fblist_num2_grouped <- lapply(unique(sapply(names(fblist_num2), get_group_key)), function(key) {
  subset(fblist_num2, sapply(names(fblist_num2), get_group_key) == key)
})

# Rename the sublists with their respective grouping keys
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

### join dfs t0 and t1
for (i in seq_along(fblist_num4)) {
  if (length(fblist_num4[[i]]) >= 2) {
    # join dfs
    df_joined <- fblist_num4[[i]][[1]] %>%
      full_join(fblist_num4[[i]][[2]], by = "CODE")

    # Count and print positive matches in joined dataframe
    col_x <- grep("\\.x$", names(df_joined), value = TRUE)
    col_y <- grep("\\.y$", names(df_joined), value = TRUE)
    count <- sum(rowSums(!is.na(df_joined[col_x])) > 0 & rowSums(!is.na(df_joined[col_y])) > 0)
      print(paste("nr of positive matches in", names(fblist_num4)[[i]], "=", count))

    # attach joined dfs to list
    fblist_num4[[i]][[paste0(names(fblist_num4)[[i]], "_joined")]] <- df_joined
    }
}
View(fblist_num4)
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

df_WISS_vor <- describe_variables(fblist_num4, "WISS_vor")
View(df_WISS_vor)
df_WISS_nac <- describe_variables(fblist_num4, "WISS_nac")
View(df_WISS_nac)
df_WISS_ver <- describe_variables(fblist_num4, "WISS_ver")
View(df_WISS_ver)


df_FAEH_vor <- describe_variables(fblist_num4, "FAEH_vor")
View(df_FAEH_vor)
df_FAEH_nac <- describe_variables(fblist_num4, "FAEH_nac")
View(df_FAEH_nac)
df_FAEH_ver <- describe_variables(fblist_num4, "FAEH_ver")
View(df_FAEH_ver)

df_ERFA_vor <- describe_variables(fblist_num4, "ERFA_vor")
View(df_ERFA_vor)
df_ERFA_nac <- describe_variables(fblist_num4, "ERFA_nac")
View(df_ERFA_nac)
df_ERFA_ver <- describe_variables(fblist_num4, "ERFA_ver")
View(df_ERFA_ver)

#### join dfs
### WISS
# Extract the first digit from Item_WISS_vor and Item_WISS_nac
df_WISS_vor$digit <- str_extract(df_WISS_vor[,2], "\\d")
df_WISS_nac$digit <- str_extract(df_WISS_nac[,2], "\\d")

# Perform a full join based on FB and digit
df_WISS_inter <- full_join(df_WISS_vor, df_WISS_nac, by = c("FB", "digit"))

###
# unpaired t-tests _vor vs. _nac

# create a new data frame to store the t-test results
#t_test_results <- data.frame(matrix(nrow = nrow(df_WISS_inter), ncol = 2))
#colnames(t_test_results) <- c("t_statistic", "p_value")

# filter out rows with missing values in either _vor or _nac
df_WISS_inter_filtered <- df_WISS_inter[complete.cases(df_WISS_inter[, grepl("_vor|_nac", names(df_WISS_inter))]), ]

# identify the columns containing "N", "M", and "SD"
n_vor_col <- grep("N.*vor", names(df_WISS_inter_filtered))
m_vor_col <- grep("M.*vor", names(df_WISS_inter_filtered))
sd_vor_col <- grep("SD.*vor", names(df_WISS_inter_filtered))

n_nac_col <- grep("N.*nac", names(df_WISS_inter_filtered))
m_nac_col <- grep("M.*nac", names(df_WISS_inter_filtered))
sd_nac_col <- grep("SD.*nac", names(df_WISS_inter_filtered))

# loop over the rows of the data frame and perform the t-test
t_test_results <- vector("list", nrow(df_WISS_inter_filtered))
for (i in 1:nrow(df_WISS_inter_filtered)) {
  t_test_results[[i]] <- tsum.test(n.x =  df_WISS_inter_filtered[i, n_vor_col],
                                   n.y = df_WISS_inter_filtered[i, n_nac_col],
                                   mean.x = df_WISS_inter_filtered[i, m_vor_col],
                                   mean.y = df_WISS_inter_filtered[i, m_nac_col],
                                   s.x = df_WISS_inter_filtered[i, sd_vor_col],
                                   s.y = df_WISS_inter_filtered[i, sd_nac_col],
                                   var.equal = FALSE)

  # attach the p-value to the original data frame
  df_WISS_inter_filtered[i, "p_wert"] <- t_test_results[[i]]["p.value"]

  # calculate effect size and attach to original data frame
  df_WISS_inter_filtered[i, "d_wert"] <- abs(t_test_results[[1]][["statistic"]][["t"]]*sqrt((df_WISS_inter_filtered[i, n_vor_col] + df_WISS_inter_filtered[i, n_nac_col])/(df_WISS_inter_filtered[i, n_vor_col] * df_WISS_inter_filtered[i, n_nac_col])))

  # calcluate pwr
  d <- (abs(t_test_results[[i]][["statistic"]][["t"]]*sqrt((df_WISS_inter_filtered[i, n_vor_col] + df_WISS_inter_filtered[i, n_nac_col])/(df_WISS_inter_filtered[i, n_vor_col] * df_WISS_inter_filtered[i, n_nac_col]))))
  n <-  (df_WISS_inter_filtered[i, n_vor_col])
  a <- pwr.t.test(d = d, n = n, type = "two.sample", alternative = "two.sided")
  df_WISS_inter_filtered[i, "pwr_wert"] <- a$power
}

d <- (abs(t_test_results[[1]][["statistic"]][["t"]]*sqrt((df_WISS_inter_filtered[1, n_vor_col] + df_WISS_inter_filtered[1, n_nac_col])/(df_WISS_inter_filtered[1, n_vor_col] * df_WISS_inter_filtered[1, n_nac_col]))))
n <-  (df_WISS_inter_filtered[1, n_vor_col])
a <- pwr.t.test(d = d, n = n, type = "two.sample", alternative = "two.sided")
a$power
# view the results
View(df_WISS_inter_filtered)

# join to original df
df_WISS_inter2 <- df_WISS_inter %>% full_join(df_WISS_inter_filtered, by = c("FB", "digit"))

## to do: the next lines should not be necessary, but the full_join  weirdly duplicates columns
# get column indices of variables ending with ".y" except p_wert
cols_to_remove <- grep("\\.y$(?!.*p_wert)(?!.*d_wert)(?!.*pwr_wert)", names(df_WISS_inter2), perl = TRUE)
# remove columns from data frame
df_WISS_inter2 <- df_WISS_inter2[, -cols_to_remove]
# remove .x from names
names(df_WISS_inter2) <- sub("\\..*", "", names(df_WISS_inter2))

# view the results
View(df_WISS_inter2)




#####################################
#### for intra-individual Analysis ##
#####################################


####    if _vor und _nac are present, calculate difference variable WISS_diff = _nac - _vor
####    if _vor und _nac are present, calculate Wilcoxon-Tests, effect size and power. attatch results to dfs.
####    put everything in a df outside of the logic (df_zus)



## to do: vor und nac sind auch in _tr FBs... die kommen dann hier aber nicht rein oder?
## ... methodisch gesehen müssten die extra. jetzt sind sie noch in der inter-individuellen statistik drin
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
View(fblist_num5)





##################### doesnt quite work ##################


# initialize an empty dataframe to store the pair registry information
pair_registry <- data.frame(FB = character(),
                            digit = character(),
                            vor = character(),
                            stringsAsFactors = FALSE)

df_list <- fblist_num5


# Create an empty dataframe to store the results
pair_registry <- data.frame(FB = character(),
                            digit = character(),
                            vor = character(),
                            nac = character(),
                            stringsAsFactors = FALSE)

# Loop through each data frame in the list
for (i in seq_along(fblist_num5)) {

  # Get the name of the current data frame
  fb_name <- names(fblist_num5)[i]

  # Get the column names that have "WISS" and either "_vor" or "_nac"
  wiss_cols <- grep("^WISS.*(vor|nac)[0-9]*", names(fblist_num5[[i]]), value = TRUE)

  # Loop through each of the WISS columns
  for (j in seq_along(wiss_cols)) {

    # Extract the digit following "_vor" or "_nac", if it exists
    digit_match <- regexpr("(?<=(_vor|_nac))\\d+", wiss_cols[j], perl = TRUE)
    if (digit_match > 0) {
      digit <- substr(wiss_cols[j], digit_match, digit_match)
    } else {
      digit <- NA_character_
    }

    # Create a new row for the pair_registry dataframe
    new_row <- data.frame(FB = fb_name,
                          digit = digit,
                          vor = ifelse(grepl("_vor", wiss_cols[j]), wiss_cols[j], NA_character_),
                          nac = ifelse(grepl("_nac", wiss_cols[j]), wiss_cols[j], NA_character_),
                          stringsAsFactors = FALSE)

    # Append the new row to the pair_registry dataframe
    pair_registry <- rbind(pair_registry, new_row)
  }
}

# View the resulting pair_registry dataframe
View(pair_registry)

#pair_registry_vor <- pair_registry[,c("FB", "digit", "vor")]
#pair_registry_nac <- pair_registry[,c("FB", "digit", "nac")]

#pair_registry_complete <- left_join(pair_registry_vor, pair_registry_nac, by = c("FB", "digit"))
#View(pair_registry_complete)



############## better way? seems very dependend on the specific dimensions of dfs and order of variables variables
##### probably not really NA robust and generalizable


###### First, get a registry of variables to pair for the difference tests

# Create an empty data frame to store the results
pair_registry_vor <- data.frame(FB = character(), vor = character(), stringsAsFactors = FALSE)

# Loop through each data frame in the list
for (i in seq_along(fblist_num5)) {
  # Get the name of the current data frame
  fb_name <- names(fblist_num5)[i]
  # Extract the variables that match the pattern using grep
  wiss_vor_vars <- grep("WISS.*_vor", names(fblist_num5[[i]]), value = TRUE)
  # Add the results to the pair_registry data frame
  pair_registry_vor <- rbind(pair_registry_vor, data.frame(FB = fb_name, vor = wiss_vor_vars, stringsAsFactors = FALSE))
}

# View the final result
pair_registry_vor

# Create an empty data frame to store the results
pair_registry_nac <- data.frame(FB = character(), nac = character(), stringsAsFactors = FALSE)

# Loop through each data frame in the list
for (i in seq_along(fblist_num5)) {
  # Get the name of the current data frame
  fb_name <- names(fblist_num5)[i]
  # Extract the variables that match the pattern using grep
  wiss_nac_vars <- grep("WISS.*_nac", names(fblist_num5[[i]]), value = TRUE)
  # Check if there are any matches
  if (length(wiss_nac_vars) == 0) {
    # If there are no matches, add a new row with NAs
    pair_registry_nac <- rbind(pair_registry_nac, data.frame(FB = fb_name, nac = NA, stringsAsFactors = FALSE))
  } else {
    # If there are matches, add a new row for each match
    for (j in seq_along(wiss_nac_vars)) {
      pair_registry_nac <- rbind(pair_registry_nac, data.frame(FB = fb_name, nac = wiss_nac_vars[j], stringsAsFactors = FALSE))
    }
  }
}

# View the final result
pair_registry_nac

pair_registry_complete <- pair_registry_vor
pair_registry_complete$nac <- pair_registry_nac$nac

View(pair_registry_complete)
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
  df <- fblist_num5[[df_name]]
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
View(extracted_data)

extracted_data <- unlist(extracted_data, recursive = FALSE)
names(extracted_data) <- gsub("^[^.]+\\.", "", names(extracted_data))

View(extracted_data)


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

for (i in 1:length(extracted_data)) {
  if (any(grepl("WISS_vor", colnames(extracted_data[[i]]))) & any(grepl("WISS_nac", colnames(extracted_data[[i]])))) {
    extracted_data[[i]]$SK.WISSdiff <- extracted_data[[i]][, grepl("WISS_nac", colnames(extracted_data[[i]]))] - extracted_data[[i]][, grepl("WISS_vor", colnames(extracted_data[[i]]))]
    wsr_test <- coin::wilcoxsign_test(extracted_data[[i]][, grepl("WISS_vor", colnames(extracted_data[[i]]))] ~ extracted_data[[i]][, grepl("WISS_nac", colnames(extracted_data[[i]]))], data = extracted_data[[i]], zero.method = "Wilcoxon")
    extracted_data[[i]]$SK.WISSdiff_p <- coin::pvalue(wsr_test)

    wsr_z <- wsr_test@statistic@teststatistic
    wsr_r <- abs(wsr_z/sqrt(nrow(wsr_test@statistic@x)))
    extracted_data[[i]]$SK.WISSdiff_d <- sqrt(-4 * wsr_r^2 / (wsr_r^2 - 1))

    library(MKpower)
    rxy <- function(n) rnorm(n, mean = mean(extracted_data[[i]]$SK.WISSdiff, na.rm = T), sd(extracted_data[[i]]$SK.WISSdiff, na.rm = T))
    pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "paired", n.min = nrow(wsr_test@statistic@x),
                                 n.max = nrow(wsr_test@statistic@x)+2, step.size = 1, iter = 1000, BREAK = FALSE)
    extracted_data[[i]]$SK.WISSdiff_pwr <- pwr[["emp.power"]][[1]]

    df_zus <- rbind(df_zus, data.frame(FB = names(extracted_data)[i],
                                       N_Skala = sum(!is.na(extracted_data[[i]][[3]])),
                                       M_WISS_vor = mean(extracted_data[[i]][, grepl("WISS_vor", colnames(extracted_data[[i]]))], na.rm = T),
                                       SD_WISS_vor = sd(extracted_data[[i]][, grepl("WISS_vor", colnames(extracted_data[[i]]))], na.rm = T),
                                       M_WISS_nac = mean(extracted_data[[i]][, grepl("WISS_nac", colnames(extracted_data[[i]]))], na.rm = T),
                                       SD_WISS_nac = sd(extracted_data[[i]][, grepl("WISS_nac", colnames(extracted_data[[i]]))], na.rm = T),
                                       M_WISS_dff = mean(extracted_data[[i]]$SK.WISSdiff, na.rm = T),
                                       SD_WISS_dff = sd(extracted_data[[i]]$SK.WISSdiff, na.rm = T),
                                       p_WISS_test = coin::pvalue(wsr_test),
                                       d_WISS_dff = sqrt(-4 * wsr_r^2 / (wsr_r^2 - 1)),
                                       pwr_WISS_dff = pwr[["emp.power"]][[1]]
    ))
  }
}

View(df_zus)









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
