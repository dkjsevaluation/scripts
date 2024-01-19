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


# Check the resulting list of nested data frames
View(extracted_data)


for (i in seq_along(extracted_data)) {
  # Identify duplicate data frames within the current nested list
  duplicates <- duplicated(extracted_data[[i]])

  # Remove duplicates from the current nested list
  extracted_data[[i]] <- extracted_data[[i]][!duplicates]
}





# Extract data frames with unique names
unique_dfs <- list()
names_list <- list()
for (i in seq_along(extracted_data)) {
  for (j in seq_along(extracted_data[[i]])) {
    name <- deparse(substitute(extracted_data[[i]][[j]]))
    if (!(name %in% names_list)) {
      unique_dfs[[name]] <- extracted_data[[i]][[j]]
      names_list <- c(names_list, name)
    }
  }
}






# Loop through each element in extracted_data and assign new names
for (i in seq_along(extracted_data)) {
  # Get the current df_name
  df_name <- names(extracted_data)[i]

  # Add an index number to the df_name
  new_name <- paste(df_name, "_1", sep = "")

  # Assign the new name to the nested list
  names(extracted_data)[i] <- new_name
}
