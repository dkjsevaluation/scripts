library(dplyr)
library(psych)
library(ggplot2)
#library(tidyr)
library(patchwork)
library(pwr)
library(dkjslama)
library(readxl)
library(openxlsx)
library(BSDA)
library(DescTools)
library(coin)
library(MKpower)
library(splithalfr)
library(stringr)

#### TODO für Programmberichte ####
# in fblist_num7 sind die vor nac Skalen,
# aber nicht mehr die vollständigen Rohdaten,
# ggf. neu matchen oder Syntax umorganisieren.



#########################################################################
########################## get and prepare list of all FBs ##############
#########################################################################

### !! There are two excels as base data. DKJSBericht_Liste.xlsx for the meta-analysis with just the _vor _nac variables.
### !! And Programmliste.xlsx for the fully recoded FBs. Choose accordingly.


###create empty list for fbdata
fblist <- list()

#Zukunftskompetenzen
colnamen <- as.character(read_xlsx("../../Programmliste.xlsx", n_max = 1, col_names = FALSE))

fbnames_all <- read_xlsx("../../Programmliste.xlsx", col_names = colnamen)
fbnames_incl <- fbnames_all %>%
  filter(Handlungsfeld == "Zukunftskompetenzen") %>%
  filter(include == 1)

fbnames <- fbnames_incl$FBname

#create empty list
fblist <- list()

## get Lamapoll data for every fbname and put it into list fbdata with fbname as name of df
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

##### attach external evaluations
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

##
fbnames_incl <- fbnames_incl %>%
  mutate(meta_name = paste0(`Programm-ID`, "_ID", `ID`, "_", `mzp`, "_", `FBname`))

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
fblist_num3 <- split(fblist_num2, sub("^(.*?)_(.*?)_(.*)$", "\\1_\\2",
                                      sapply(names(fblist_num2),
                                             function(x) unlist(strsplit(x, split = ".", fixed = TRUE))[1])))

##############

#fblist_num4 <- fblist_num3

fblist_num4 <- fblist_num2

fblist2_num4_test <- fblist_num4[sapply(fblist_num4, function(x) ncol(x) >= 2)]


fblist2_num <- fblist2_num4_test

##################################
############ Scale Building ######
##################################

#### this requires building a filter system that applies different ways of calculating scales and reliability
#### depending on the first two prefixes in the variable name as well as the "_s1" part of the variable name.
#### lets build modules based on Tabelle Variablenbenennung!
#### in the end/at the beginning, loop through all modules for every df in fblist2_num

#### Module 1 for WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM; Module 2 for ERWA; ...TN .... ZUFR

############### Module 1

###########################
#### Sort Items into scales
#### done: extend for ".KJ" at the end of a variable name
#### done: check for different "_s0", which should be identical for all items in a scale
#### to do: build different scales for ROLLE!!!! only needed for reliability calculation and report?
####  either: make a function with the role as input that translates it to regex
####  or: detect ROLLE occurrences and use it to build all scale variants?
####  neither! attach scale to original df and use dyplr::group_by(Rolle), scrap the rolle-dependent reliability.
#### to do: sum scores for mehrfachantwortensets (_s...)

# get the scale types and differentiate mean scales and sum scales
# load scale types
scale_types <- read_xlsx("../../Skalenbildung_EvaFB.xlsx")
sum_scales <- scale_types %>%
  filter(str_detect(Berechnung, "Summe")) %>%
  filter(!is.na(Skala))
# clean from empty spaces and add underscore
sum_scale_suffixes <- str_replace_all(sum_scales$Skala, "\\s+", "")
sum_scale_suffixes <- str_replace_all(sum_scale_suffixes, "^", "_")

# test: sum_scale_suffixes <- c("_s14", "_s2")

# make an empty list to store scale-sorted data
fblist2_num2 <- vector("list", length = length(fblist2_num))

# loop over the list with fb data
for (k in 1:length(fblist2_num)) {
  # Extract the part of the variable names without "_i1" etc.
  variable_names <- str_replace(names(fblist2_num[[k]]), "_i\\d+", "")
  variable_names <- str_replace(variable_names, "_\\.", ".")

  # Get only the unique values
  unique_variable_names <- unique(variable_names)

  # extract variable names for scales
  scalevars <- unique_variable_names[grep("^(WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM)_", unique_variable_names)]

  # Create empty list with names from scalevars
  scalevar_dfs <- vector("list", length = length(scalevars))
  names(scalevar_dfs) <- scalevars

  # Loop over each element in scalevars
  for (i in 1:length(scalevars)) {
    scalevarname <- scalevars[i]
    scalevarprefix <- sub("_.*", "", scalevarname)
    scalevarsuffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)

    matching_vars <- grep(paste0("^", scalevarprefix, ".*_", scalevarsuffix, "$"),
                          names(fblist2_num[[k]]), value = TRUE)

    if (length(matching_vars) > 0) {
      scalevar_dfs[[i]] <- fblist2_num[[k]][, matching_vars, drop = FALSE]

      # extract suffix from scalevarname
      suffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)

      # filter variables in the current scalevar_dfs
      scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^.*_", suffix, "$"),
                                                     names(scalevar_dfs[[i]])), drop = FALSE]

      # extract prefix from scalevarname
      prefix <- sub("(.*?_.*?_).*", "\\1", scalevarname)

      # filter variables in the current scalevar_dfs to keep only variables matching the prefix
      scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^", prefix, ".*"), names(scalevar_dfs[[i]])), drop = FALSE]
    }
  }
  # Select only those list elements that are data frames with at least two columns
  #scalevar_dfs2 <- scalevar_dfs[sapply(scalevar_dfs, is.list)]   # retired, not needed anymore
  scalevar_dfs <- scalevar_dfs[sapply(scalevar_dfs, function(x) ncol(x) >= 2)]

  ### trennen für summenskalen und mittelwerte
  scalevars_dfs1 <- list()
  scalevars_dfs2 <- list()

  # Loop through the names of data frames in scalevar_dfs
  for (df_name in names(scalevar_dfs)) {
    if (any(str_detect(df_name, paste0(".*(", paste(sum_scale_suffixes, collapse = "|"), ")\\D")))) {
      # If true, assign the data frame to scalevars_dfs1
      scalevars_dfs1[[df_name]] <- scalevar_dfs[[df_name]]
    } else {
      # If false, assign the data frame to scalevars_dfs2
      scalevars_dfs2[[df_name]] <- scalevar_dfs[[df_name]]
    }
  }

  ### für summenskalen

  for (i in seq_along(scalevars_dfs1)) {

    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))

    # check if the data frame has more than two columns
    if (ncol(scalevars_dfs1[[i]]) > 2) {

      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))

      # calculate the row means for the data frame and add as new column
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs1)[i]) := rowSums(.[, 1:ncol(.)], na.rm = TRUE))

      # calculate reliability and attach as new column
      alpha <- tryCatch(
        psych::alpha(scalevars_dfs1[[i]], check.keys = TRUE),
        error = function(e) NULL
      )

      if (!is.null(alpha)) {
        scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs1)[i]) := alpha %>%
                   {. ->> tmp} %>%
                   '[['(1) %>%
                   pull(std.alpha) %>%
                   .[1])
      } else {
        scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs1)[i]) := NA)
        next # skip to next iteration of for loop
      }
    }
    # check if the data frame has two columns
    if (ncol(scalevars_dfs1[[i]]) < 3) {

      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))

      # calculate the row means for the data frame and add as new column
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs1)[i]) := rowSums(.[, 1:ncol(.)], na.rm = TRUE))

      # calculate reliability and attach as new column
      scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
        mutate(
          !!paste0("REL.", names(scalevars_dfs1)[i]) := tryCatch(
            spearman_brown(na.omit(.[, 1]), na.omit(.[, 2])),
            error = function(e) NA
          )
        )

      # scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
      #  mutate(!!paste0("REL.", names(scalevars_dfs1)[i]) := spearman_brown(na.omit(.[,1]), na.omit(.[,2])))
    }
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs1[[i]] <- scalevars_dfs1[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    scalevars_dfs1 <<- scalevars_dfs1
  }

  ### für mittelwertskalen

  for (i in seq_along(scalevars_dfs2)) {

    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))

    # check if the data frame has more than two columns
    if (ncol(scalevars_dfs2[[i]]) > 2) {

      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))

      # calculate the row means for the data frame and add as new column
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs2)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))

      # calculate reliability and attach as new column
      alpha <- tryCatch(
        psych::alpha(scalevars_dfs2[[i]], check.keys = TRUE),
        error = function(e) NULL
      )

      if (!is.null(alpha)) {
        scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs2)[i]) := alpha %>%
                   {. ->> tmp} %>%
                   '[['(1) %>%
                   pull(std.alpha) %>%
                   .[1])
      } else {
        scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
          mutate(!!paste0("REL.", names(scalevars_dfs2)[i]) := NA)
        next # skip to next iteration of for loop
      }
    }
    # check if the data frame has two columns
    if (ncol(scalevars_dfs2[[i]]) < 3) {

      # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate_all(function(x) ifelse(is.nan(x), NA, x))

      # calculate the row means for the data frame and add as new column
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate(!!paste0("SK.", names(scalevars_dfs2)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))

      # calculate reliability and attach as new column
      scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
        mutate(
          !!paste0("REL.", names(scalevars_dfs2)[i]) := tryCatch(
            spearman_brown(na.omit(.[, 1]), na.omit(.[, 2])),
            error = function(e) NA
          )
        )

      # scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
      #  mutate(!!paste0("REL.", names(scalevars_dfs2)[i]) := spearman_brown(na.omit(.[,1]), na.omit(.[,2])))
    }
    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevars_dfs2[[i]] <- scalevars_dfs2[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))
    scalevars_dfs2 <<- scalevars_dfs2
  }

  ### coalesce scalevars_dfs1 und 2

  scalevar_dfs <- c(scalevars_dfs1, scalevars_dfs2)

  ### copy to prepared list fblist2_num2
  fblist2_num2[[k]] <- scalevar_dfs
  # Assign names to the list elements
  names(fblist2_num2)[k] <- paste0(names(fblist2_num[k]))
}

# replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
# Function to replace NaN values with NA
replace_nan_with_na <- function(df) {
  if (inherits(df, "data.frame")) {
    df[] <- lapply(df, function(x) ifelse(is.nan(x), NA, x))
  }
  return(df)
}

# Iterate over each data frame in the list
for (i in seq_along(fblist2_num2)) {
  fblist2_num2[[i]] <- replace_nan_with_na(fblist2_num2[[i]])
}


### nur berechnete SK und REL behalten und an original dfs ranpappen

combined_dfs_list <- list()

# Iterate through the list elements in fblist2_num2
for (list_name in names(fblist2_num2)) {
  list_element <- fblist2_num2[[list_name]]
  combined_df <- NULL

  # Iterate through the data frames within each list element
  for (df in list_element) {
    # Check if combined_df is NULL (first data frame)
    if (is.null(combined_df)) {
      combined_df <- df
    } else {
      # Use cbind to combine the data frames horizontally
      combined_df <- cbind(combined_df, df)
    }
  }

  # Add the combined data frame to the named list
  combined_dfs_list[[list_name]] <- combined_df
}

# Loop through each data frame in the list
for (i in 1:length(combined_dfs_list)) {
  df <- combined_dfs_list[[i]]

  # Use grep to select columns that match the pattern
  selected_columns <- grep("^SK\\.|^REL\\.", names(df), value = TRUE)

  # Subset the data frame to keep only the selected columns
  df <- df[, selected_columns]

  # Update the data frame in the list
  combined_dfs_list[[i]] <- df
}

# Initialize an empty list to store the combined data frames
combined_dfs_list2 <- list()

num_data_frames <- length(fblist2_num)

# Iterate through the data frames and combine them
for (i in 1:num_data_frames) {
  # Combine data frames based on their indexes
  df1_combined <- cbind(fblist2_num[[i]], combined_dfs_list[[i]])

  # Add the combined data frames to list_c
  combined_dfs_list2[[paste0("combined_", i)]] <- df1_combined
}

names(combined_dfs_list2) <- names(fblist2_num)

#### now matching

#######################################################################
########################## Group the FBs in nested lists ##############
#######################################################################

#### Group the list into Programm-IDS
# Make function that extracts the grouping key from a name of a df
get_group_key <- function(name) {
  substr(name, start = 1, stop = 7)  # Assumes the key is the first 7 characters
}

# Use lapply to loop over the list of dataframes and group them by the key
fblist_num2_grouped <- lapply(unique(sapply(names(combined_dfs_list2), get_group_key)), function(key) {
  subset(combined_dfs_list2, sapply(names(combined_dfs_list2), get_group_key) == key)
})

# Rename the sublists with their grouping keys
names(fblist_num2_grouped) <- unique(sapply(names(combined_dfs_list2), get_group_key))

#### Group the list into Programm-ID_ID groups
combined_dfs_list3 <- split(combined_dfs_list2,
                            sub("^(.*?)_(.*?)_(.*)$", "\\1_\\2",
                                sapply(names(combined_dfs_list2),
                                       function(x) unlist(strsplit(x, split = ".", fixed = TRUE))[1])))

######################################################################
########################## Match and Join t0 and t1 dfs ##############
######################################################################


### recode CODE variables to be the same
for (i in seq_along(combined_dfs_list3)) {
  for (j in seq_along(combined_dfs_list3[[i]])) {
    if ("CODE_t0" %in% names(combined_dfs_list3[[i]][[j]])) {
      names(combined_dfs_list3[[i]][[j]])[grep("CODE_t0", names(combined_dfs_list3[[i]][[j]]))] <- "CODE"
    }
    if ("CODE_t1" %in% names(combined_dfs_list3[[i]][[j]])) {
      names(combined_dfs_list3[[i]][[j]])[grep("CODE_t1", names(combined_dfs_list3[[i]][[j]]))] <- "CODE"
    }
    if ("CODE_tr" %in% names(combined_dfs_list3[[i]][[j]])) {
      names(combined_dfs_list3[[i]][[j]])[grep("CODE_tr", names(combined_dfs_list3[[i]][[j]]))] <- "CODE"
    }
  }
}


for (i in seq_along(combined_dfs_list3)) {
  if (length(combined_dfs_list3[[i]]) >= 2 &&
      "CODE" %in% names(combined_dfs_list3[[i]][[1]]) &&
      "CODE" %in% names(combined_dfs_list3[[i]][[2]])) {
    # join dfs
    df_joined <- combined_dfs_list3[[i]][[1]] %>%
      full_join(combined_dfs_list3[[i]][[2]], by = "CODE")

    # Count and print positive matches in the joined df
    col_x <- grep("\\.x$", names(df_joined), value = TRUE)
    col_y <- grep("\\.y$", names(df_joined), value = TRUE)
    count <- sum(rowSums(!is.na(df_joined[col_x])) > 0 & rowSums(!is.na(df_joined[col_y])) > 0)
    print(paste("nr of positive matches in", names(combined_dfs_list3)[[i]], "=", count))

    # Attach joined dfs to the list
    joined_name <- paste0(names(combined_dfs_list3)[[i]], "_joined")
    combined_dfs_list3[[i]][[joined_name]] <- df_joined
  } else {
    print(paste("No matching df or variable 'CODE' not found in", names(combined_dfs_list3)[[i]]))
  }
}

fblist_num4 <- combined_dfs_list3 #sehr gut für die Programmbericht nutzbar

### result is one list of lists with dfs

## flatten the list into an unnested list
#fblist2_num8 <- list()

#for (nested_list in combined_dfs_list3) {
#  for (i in 1:length(nested_list)) {
#    fblist2_num4 <- c(fblist2_num4,nested_list[i])
#  }
#}

##### result is a list of dfs with all scales and their reliability from module 1
## to do: module for zufr, nps etc.
### then statistics

##############################################
##### intra individual statistics ############
##############################################


### This needs two modules. one for "joined" dfs and on for "tr" dfs
### "joined" dfs work with e.g. t0 t1 or t0 t3 regex:"_t[0-9a-zA-Z]"
### "tr" dfs work with _vor and _nac

###############
#### for tr ###
###############

# select tr FBs like in Stiftungsbericht_new_2

fblist_num7 <- list()

for (i in seq_along(fblist_num4)) {
  df_list <- list()
  for (j in seq_along(fblist_num4[[i]])) {
    if ((is.data.frame(fblist_num4[[i]][[j]])) & grepl("_tr", names(fblist_num4[[i]][j]))) {
      df_list[[names(fblist_num4[[i]])[j]]] <- fblist_num4[[i]][[j]]
    }
  }
  fblist_num7[[i]] <- df_list
}

fblist_num7 <- unlist(fblist_num7, recursive = FALSE)
View(fblist_num7)

# calculate scales like in Stiftungsbericht_new_2

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

fblist_num7 <- calculate_scales_bericht("HAND_vor")
fblist_num7 <- calculate_scales_bericht("HAND_nac")

fblist_num7 <- calculate_scales_bericht("NETZ_vor")
fblist_num7 <- calculate_scales_bericht("NETZ_nac")

fblist_num7 <- calculate_scales_bericht("TEIL_vor")
fblist_num7 <- calculate_scales_bericht("TEIL_nac")

fblist_num7 <- calculate_scales_bericht("AFFM_vor")
fblist_num7 <- calculate_scales_bericht("AFFM_nac")

# select only variables starting with "^(WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM)_" or SK.

for (i in seq_along(fblist_num7)) {
  # Select only the needed variables
  fblist_num7[[i]] <- fblist_num7[[i]] %>% select(starts_with("SK.") |
                                                    starts_with("CODE") |
                                                    starts_with("WISS_") |
                                                    starts_with("ERFA_") |
                                                    starts_with("HAND_") |
                                                    starts_with("FAEH_") |
                                                    starts_with("NETZ_") |
                                                    starts_with("TEIL_") |
                                                    starts_with("AFFM_"))
}



# replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
for (i in seq_along(fblist_num7)) {
  fblist_num7[[i]] <- fblist_num7[[i]] %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))
}


# select all but everything-NA variables
for (i in seq_along(fblist_num7)) {
  fblist_num7[[i]] <- fblist_num7[[i]] %>%
    select_if(~!all(is.na(.)))
}

# calculate intra stats like in Stiftungsbericht_new_2
# modify: make a list with matching _vor und _nac variablen
# look at inter-individual statistics in Stiftungsbericht_new_2 for that line 230
# lets try without the describe part, just contains _vor and _nac
# done! now just put it in a for loop and put the results into a list of dfs with the ID as dfname
# done!


pair_list <- list()

for (i in 1:length(fblist_num7)) {
  df_test <- fblist_num7[[i]]
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
  df_pairs2 <- df_pairs %>% select(Item_vor, Item_nac)

  # Create a unique name for the data frame based on the df_test
  result_name <- paste0("result_", length(pair_list) + 1)

  # Add the df_pairs2 data frame to the pair_list with the unique name
  pair_list[[result_name]] <- df_pairs2

  # rename the df
  df_name <- names(fblist_num7[i])
  names(pair_list)[names(pair_list) == result_name] <- df_name
}

##################


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
      wsr_r <- wsr_z / sqrt(nrow(wsr_test@statistic@x))
      #h <- 4                                                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #d <- sqrt(h * wsr_r) / sqrt(1-wsr_r^2)                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #n <- sum(!is.na(fblist_num7[[i]][[3]]))
      #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
      #d <- wsr_r
      #fblist_num7[[i]]$SK.diff_eff <- d
      d <- (sqrt(4) * wsr_r) / (sqrt(1 - wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner

      library(MKpower)      ### the power simulation works on a minimum of n >= 6, when n < 6 the working minimum gets applied
      pwr_m <- mean(mean_diff, na.rm = TRUE)
      pwr_sd <- sd(mean_diff, na.rm = TRUE)
      rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
      pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x) - 1,
                                   n.max = nrow(wsr_test@statistic@x) + 5, step.size = 1, iter = 1000, BREAK = FALSE)
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
                                         M_vor = mean(vorher, na.rm = TRUE),
                                         SD_vor = sd(vorher, na.rm = TRUE),
                                         M_nac = mean(nachher, na.rm = TRUE),
                                         SD_nac = sd(nachher, na.rm = TRUE),
                                         M_dff = mean(mean_diff, na.rm = TRUE),
                                         SD_dff = sd(mean_diff, na.rm = TRUE),
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
                                           M_vor = mean(vorher, na.rm = TRUE),
                                           SD_vor = sd(vorher, na.rm = TRUE),
                                           M_nac = mean(nachher, na.rm = TRUE),
                                           SD_nac = sd(nachher, na.rm = TRUE),
                                           M_dff = mean(mean_diff, na.rm = TRUE),
                                           SD_dff = sd(mean_diff, na.rm = TRUE),
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
        wsr_r <- wsr_z / sqrt(nrow(wsr_test@statistic@x))
        #h <- 4                                                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
        #d <- sqrt(h * wsr_r) / sqrt(1-wsr_r^2)                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
        #n <- sum(!is.na(fblist_num7[[i]][[3]]))
        #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
        #d <- wsr_r
        #fblist_num7[[i]]$SK.diff_eff <- d
        d <- (sqrt(4) * wsr_r) / (sqrt(1 - wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner

        library(MKpower)      ### the power simulation works on a minimum of n >= 6, when n < 6 the working minimum gets applied
        pwr_m <- mean(mean_diff, na.rm = TRUE)
        pwr_sd <- sd(mean_diff, na.rm = TRUE)
        rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
        pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x) - 1,
                                     n.max = nrow(wsr_test@statistic@x) + 5, step.size = 1, iter = 1000, BREAK = FALSE)
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
                                           M_vor = mean(vorher, na.rm = TRUE),
                                           SD_vor = sd(vorher, na.rm = TRUE),
                                           M_nac = mean(nachher, na.rm = TRUE),
                                           SD_nac = sd(nachher, na.rm = TRUE),
                                           M_dff = mean(mean_diff, na.rm = TRUE),
                                           SD_dff = sd(mean_diff, na.rm = TRUE),
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
                                         M_vor = mean(vorher, na.rm = TRUE),
                                         SD_vor = sd(vorher, na.rm = TRUE),
                                         M_nac = mean(nachher, na.rm = TRUE),
                                         SD_nac = sd(nachher, na.rm = TRUE),
                                         M_dff = mean(mean_diff, na.rm = TRUE),
                                         SD_dff = sd(mean_diff, na.rm = TRUE),
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

### the result is a list of dfs with intra-individual statistics
## the results in the list can be used for the report


#################
### for joined ##
#################


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
  df_select_vor <- df_test  %>%
    select(matches("_t0")) %>%
    select(-contains("_vor") & -contains("_nac") & -contains("_ver"))  
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
  df_pairs2 <- df_pairs %>% select(Item_vor, Item_nac)

  # Create a unique name for the data frame based on the df_test
  result_name <- paste0("result_", length(pair_list2) + 1)

  # Add the df_pairs2 data frame to the pair_list with the unique name
  pair_list2[[result_name]] <- df_pairs2

  # rename the df
  df_name <- names(fblist_num8[i])
  names(pair_list2)[names(pair_list2) == result_name] <- df_name
}

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
  df_pairs2 <- df_pairs %>% select(Item_vor, Item_nac)

  # Create a unique name for the data frame based on the df_test
  result_name <- paste0("result_", length(pair_list3) + 1)

  # Add the df_pairs2 data frame to the pair_list with the unique name
  pair_list3[[result_name]] <- df_pairs2

  # rename the df
  df_name <- names(fblist_num8[i])
  names(pair_list3)[names(pair_list3) == result_name] <- df_name
}

## 'coalesce' both pair lists

shared_ID <- intersect(names(pair_list2), names(pair_list3))

# Append rows for common names
for (name in shared_ID) {
  pair_list3[[name]] <- rbind(pair_list3[[name]], pair_list2[[name]])
}


##### calculate intra individual stats t0 t1 with pair_list3 and fblist_num8 #######

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
      wsr_r <- wsr_z / sqrt(nrow(wsr_test@statistic@x))
      #h <- 4                                                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #d <- sqrt(h * wsr_r) / sqrt(1-wsr_r^2)                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
      #n <- sum(!is.na(fblist_num8[[i]][[3]]))
      #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
      #d <- wsr_r
      #fblist_num8[[i]]$SK.diff_eff <- d
      d <- (sqrt(4) * wsr_r) / (sqrt(1 - wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner

      library(MKpower)      ### the power simulation works on a minimum of n >= 6, when n < 6 the working minimum gets applied
      pwr_m <- mean(mean_diff, na.rm = TRUE)
      pwr_sd <- sd(mean_diff, na.rm = TRUE)
      rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
      pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x) - 1,
                                   n.max = nrow(wsr_test@statistic@x) + 5, step.size = 1, iter = 1000, BREAK = FALSE)
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
                                         M_vor = mean(vorher, na.rm = TRUE),
                                         SD_vor = sd(vorher, na.rm = TRUE),
                                         M_nac = mean(nachher, na.rm = TRUE),
                                         SD_nac = sd(nachher, na.rm = TRUE),
                                         M_dff = mean(mean_diff, na.rm = TRUE),
                                         SD_dff = sd(mean_diff, na.rm = TRUE),
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
                                           M_vor = mean(vorher, na.rm = TRUE),
                                           SD_vor = sd(vorher, na.rm = TRUE),
                                           M_nac = mean(nachher, na.rm = TRUE),
                                           SD_nac = sd(nachher, na.rm = TRUE),
                                           M_dff = mean(mean_diff, na.rm = TRUE),
                                           SD_dff = sd(mean_diff, na.rm = TRUE),
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
        wsr_r <- wsr_z / sqrt(nrow(wsr_test@statistic@x))
        #h <- 4                                                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
        #d <- sqrt(h * wsr_r) / sqrt(1-wsr_r^2)                    # Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. Journal of educational psychology, 74(2), 166.
        #n <- sum(!is.na(fblist_num8[[i]][[3]]))
        #d <- wsr_r * sqrt((n - 2) / (1 - wsr_r^2))                 # Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates.
        #d <- wsr_r
        #fblist_num8[[i]]$SK.diff_eff <- d
        d <- (sqrt(4) * wsr_r) / (sqrt(1 - wsr_r^2))  # cohen's d formula from Rosenthal (1982,1994) wie im psychometrica rechner

        library(MKpower)      ### the power simulation works on a minimum of n >= 6, when n < 6 the working minimum gets applied
        pwr_m <- mean(mean_diff, na.rm = TRUE)
        pwr_sd <- sd(mean_diff, na.rm = TRUE)
        rxy <- function(n) rnorm(n, mean = pwr_m, pwr_sd)
        pwr <- sim.ssize.wilcox.test(rx = rxy, mu = 0, type = "one.sample", n.min = nrow(wsr_test@statistic@x) - 1,
                                     n.max = nrow(wsr_test@statistic@x) + 5, step.size = 1, iter = 1000, BREAK = FALSE)
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
                                           M_vor = mean(vorher, na.rm = TRUE),
                                           SD_vor = sd(vorher, na.rm = TRUE),
                                           M_nac = mean(nachher, na.rm = TRUE),
                                           SD_nac = sd(nachher, na.rm = TRUE),
                                           M_dff = mean(mean_diff, na.rm = TRUE),
                                           SD_dff = sd(mean_diff, na.rm = TRUE),
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


##############################################################


### bind result lists from tr and t0,t1 together
stats_joined_intraindividual <- c(stats_list, stats_list2)
View(stats_joined_intraindividual)

##############################################################

######################################
########## Additional Statistics #####
######################################

### Zufriedenheit

### NPS


######################################
########## Reporting #################
######################################

### descriptives and plots for all variables and scales





########################
##### The bin ##########
########################


df_test <- fblist_num7[["533-508_ID23_tr_Evaluation_Lernferien_SuS_retro"]]
df_test <- fblist_num7[["853-771_ID22_tr_Evaluation_Zukunftskompetenzen_Kinder"]]