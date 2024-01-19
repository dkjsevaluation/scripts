####################OLD VERSION##############################

# Extract the first part of the variable names before the second underscore
variable_names <- str_extract(names(fblist_num[[2]]), "^([^_]+_[^_]+)+_")
print(variable_names)

# Get only the unique values
unique_variable_names <- unique(variable_names)
print(unique_variable_names)

# extract variable names for scales
scalevars <- unique_variable_names[grep("^(WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM)_", unique_variable_names)]
print(scalevars)

# Create empty list with names from scalevars
scalevar_dfs <- vector("list", length = length(scalevars))
names(scalevar_dfs) <- scalevars

# Loop over each element in scalevars
for (i in seq_along(scalevars)) {

  # Find all variables in fblist_num[[2]] that match the current scalevar
  matching_vars <- grep(paste0("^", scalevars[i]), names(fblist_num[[2]]), value = TRUE)

  # Extract the matching variables and store them in the corresponding list element
  scalevar_dfs[[scalevars[i]]] <- fblist_num[[2]][, matching_vars]

}

####################NEW VERSION################################

library(stringr)
# Generate some random data with missing values
set.seed(123)
data <- data.frame(
  HAND_begl3_i4_s4_tr = sample(c(1:10, NA), 5, replace = TRUE),
  ZUFR_sonstige_s6_tr.Zufriedenheit = sample(c(1:10, NA), 5, replace = TRUE),
  HAND_begl1_i13_s5_tr.LK = sample(c(1:10, NA), 5, replace = TRUE),
  GESCHLECHT_tr = sample(c(1:2, NA), 5, replace = TRUE),
  HAND_begl1_i17_s5_tr.KJ = sample(c(1:10, NA), 5, replace = TRUE),
  HAND_begl1_i17_s5_tr.SB = sample(c(1:10, NA), 5, replace = TRUE),
  HAND_begl1_i17_s5_tr.LK = sample(c(1:10, NA), 5, replace = TRUE),
  WISS_inha5_i7_s1_tr.C8 = sample(c(1:10, NA), 5, replace = TRUE),
  WISS_inha5_i8_s1_tr.C8 = sample(c(1:10, NA), 5, replace = TRUE),
  WISS_inha5_i9_s1_tr.C8 = sample(c(1:10, NA), 5, replace = TRUE),
  WISS_inha1_i14_s1_tr.C13 = sample(c(1:10, NA), 5, replace = TRUE),
  WISS_inha1_i15_s1_tr.C13 = sample(c(1:10, NA), 5, replace = TRUE),
  WISS_inha1_i14_s10_tr.C13 = sample(c(1:10, NA), 5, replace = TRUE),
  WISS_inha1_i15_s10_tr.C13 = sample(c(1:10, NA), 5, replace = TRUE)
)


# Extract the part of the variable names without "_i1" etc.
variable_names <- str_replace(names(fblist_num[[2]]), "_i\\d+", "")
variable_names <- str_replace(variable_names, "_\\.", ".")
print(variable_names)

# Get only the unique values
unique_variable_names <- unique(variable_names)
print(unique_variable_names)

# extract variable names for scales
scalevars <- unique_variable_names[grep("^(WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM)_", unique_variable_names)]
print(scalevars)

# Create empty list with names from scalevars
scalevar_dfs <- vector("list", length = length(scalevars))
names(scalevar_dfs) <- scalevars

# Loop over each element in scalevars
for (i in 1:length(scalevars)) {
  scalevarname <- scalevars[i]
  scalevarprefix <- sub("_.*", "", scalevarname)
  scalevarsuffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)

  matching_vars <- grep(paste0("^", scalevarprefix, ".*_", scalevarsuffix, "$"), names(fblist_num[[2]]), value = TRUE)

  if (length(matching_vars) > 0) {
    scalevar_dfs[[i]] <- fblist_num[[2]][, matching_vars, drop = FALSE]

    # extract suffix from scalevarname
    suffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)

    # filter variables in the current scalevar_dfs
    scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^.*_", suffix, "$"), names(scalevar_dfs[[i]])), drop = FALSE]

    # extract prefix from scalevarname
    prefix <- sub("(.*?_.*?_).*", "\\1", scalevarname)

    # filter variables in the current scalevar_dfs to keep only variables matching the prefix
    scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^", prefix, ".*"), names(scalevar_dfs[[i]])), drop = FALSE]

  }
}


#### to do prefix


############# to do: does matching with an OR clause....


#######
# Loop over each element in scalevars
for (i in 1:length(scalevars)) {
  scalevarname <- scalevars[i]
  scalevarprefix <- sub("_.*", "", scalevarname)
  scalevarsuffix <- sub(".*_(.*)", "\\1", scalevarname)

  matching_vars <- grep(paste0("^", scalevarprefix, ".*_", scalevarsuffix, "$"), names(fblist_num[[2]]), value = TRUE)

  if (length(matching_vars) > 0) {
    scalevar_dfs[[i]] <- fblist_num[[2]][, matching_vars, drop = FALSE]
  }
}

#######
scalevarname <- names(scalevar_dfs)[26]
print(scalevarname)
scalevarsuffix <- gsub("^.*(_s\\d+)", "\\1", scalevarname)
print(scalevarsuffix)
df <- scalevar_dfs[[26]]
for (j in seq_along(df)) {
  if (!grepl(paste0("_", scalevarsuffix, "$"), names(df)[j])) {
    df[[j]] <- NULL
  }
}


scalevarname <- names(scalevar_dfs)[26]
print(scalevarname)
# Escape special characters in scalevarsuffix
scalevarsuffix2 <- gsub("([\\.\\[\\{\\(\\*\\+\\?\\^\\$\\\\])", "\\\\\\1", scalevarsuffix)
print(scalevarsuffix2)
# Construct regular expressions to match suffix and full string
suffix_pattern <- paste0("_s\\d+_tr$")
string_pattern <- paste0(".*", scalevarsuffix2, "$")
# Check if string matches both patterns
!grepl(suffix_pattern, "HAND_meth_i16_s24_tr") & grepl(string_pattern, "HAND_meth_i16_s24_tr")


df <- scalevar_dfs[[26]]
for (j in seq_along(df)) {
  if (!grepl(suffix_pattern, names(df)[j]) & grepl(string_pattern, names(df)[j])) {
    df[[j]] <- NULL
  }
}
names(df)




####
library(stringr)
scalevarname <- names(scalevar_dfs)[26]
print(scalevarname)
scalevarsuffix <- str_extract(scalevarname, "_s\\d+")
print(scalevarsuffix)
df <- scalevar_dfs[[26]]
names(df)
for (j in seq_along(df)) {
  if (!grepl(paste0(".*_", scalevarsuffix, "$"), names(df)[j])) {
    df[[j]] <- NULL
  }
}
names(df)



scalevarname <- names(scalevar_dfs)[i]
scalevarsuffix <- gsub("^.*(_s\\d+)", "\\1", scalevarname)
df <- scalevar_dfs[[i]]
for (j in seq_along(df)) {
  if (!grepl(scalevarsuffix, names(df)[j])) {
    df[[j]] <- NULL
  }
}
scalevar_dfs[[i]] <- df


  for (i in seq_along(scalevar_dfs)) {
    scalevarname <- names(scalevar_dfs)[i]
    scalevarsuffix <- gsub("^.*(_s\\d+)", "\\1", scalevarname)
    df <- scalevar_dfs[[i]]
    for (j in seq_along(df)) {
      if (!grepl(scalevarsuffix, names(df)[j])) {
        df[[j]] <- NULL
      }
    }
    scalevar_dfs[[i]] <- df
  }




#######################
# loop through each dataframe in scalevar_df
for (i in seq_along(scalevar_df)) {
  # extract suffix from dataframe name
  scalevarname <- names(scalevar_df)[i]
  suffix <- gsub("^.*(_s\\d+)", "\\1", scalevarname)

  # check if there are any variables in the dataframe that match the pattern
  matching_vars <- grep(suffix, names(scalevar_df[[i]]), value = TRUE)
  if (length(matching_vars) == 0) {
    # if no matching variables, skip to next dataframe
    next
  }

  # loop through each variable in the dataframe
  for (j in seq_along(scalevar_df[[i]])) {
    # check if variable name contains the suffix
    if (!grepl(suffix, names(scalevar_df[[i]])[j])) {
      # if not, delete the variable
      scalevar_df[[i]][[j]] <- NULL
    }
  }
}





##########
# Loop over each element in scalevars
for (i in 1:length(scalevar_dfs)) {
  scalevarname <- names(scalevar_dfs)[i]
  scalevarsuffix <- gsub("^.*(_s\\d+)", "\\1", scalevarname)

  matching_vars <- grep(paste0("^", scalevarprefix, ".*_", scalevarsuffix, "$"), names(fblist_num[[2]]), value = TRUE)

  if (length(matching_vars) > 0) {
    scalevar_dfs[[i]] <- fblist_num[[2]][, matching_vars, drop = FALSE]
  }
}

for (i in 1:length(scalevar_dfs)) {
  # Extract the suffix from the name of the data frame
  suffix <- sub("^.*_s", "", names(i)[1])
  # Loop through the variables in the data frame
  for (var in names(i)) {
    # Check if the variable name contains the suffix
    if (!grepl(suffix, var)) {
      # If not, delete the variable
      i[[var]] <- NULL
    }
  }
}

# Extract suffix from the name of the list element
suffix <- sub(".*_.*_", "", names(scalevar_dfs[[i]]))

# Loop over each variable in the list element
for (j in names(scalevar_dfs[[i]])) {
  varprefix <- sub("_.*", "", j)
  if (varprefix != scalevarprefix) {
    scalevar_dfs[[i]] <- scalevar_dfs[[i]][, -which(names(scalevar_dfs[[i]]) == j)]
  } else {
    varsuffix <- sub(".*_.*_", "", j)
    if (varsuffix != suffix) {
      scalevar_dfs[[i]] <- scalevar_dfs[[i]][, -which(names(scalevar_dfs[[i]]) == j)]
    }
  }
}
