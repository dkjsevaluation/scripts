#create list to fill in
fblist_num2 <- vector("list", length = length(fblist_num))

# loop over the list with fb data
for (k in 1:length(fblist_num)) {
# Extract the part of the variable names without "_i1" etc.
variable_names <- str_replace(names(fblist_num[[k]]), "_i\\d+", "")
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

  matching_vars <- grep(paste0("^", scalevarprefix, ".*_", scalevarsuffix, "$"), names(fblist_num[[k]]), value = TRUE)

  if (length(matching_vars) > 0) {
    scalevar_dfs[[i]] <- fblist_num[[k]][, matching_vars, drop = FALSE]

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

# Assign names to the list elements
names(fblist_num2)[k] <- paste0(names(fblist_num[k]))

# Select only those list elements that are data frames with at least two columns
#scalevar_dfs2 <- scalevar_dfs[sapply(scalevar_dfs, is.list)]   # retired
fblist_num2[[k]] <- scalevar_dfs[sapply(scalevar_dfs, function(x) ncol(x) >= 2)]

}

