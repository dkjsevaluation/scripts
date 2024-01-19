a <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_vor3_s9_t0.C5"]]
b <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_nac3_s9_t1.C5"]]
c <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_nac3_s9_t1.C5"]] - fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_vor3_s9_t0.C5"]]
sd(c, na.rm = TRUE)
a_complete <- na.omit(a)
b_complete <- na.omit(b)
sd(a_complete)
sd(b_complete)

summary(fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_nac2_s9_t1.C5"]])

summary(b)

Hmisc::rcorr(df_test$a, df_test$b, type = "spearman")
f <- psych::corr.test(df_test$a,df_test$b, method = "kendall")

cor.test(a,b)
cor.test(a, b, method = "kendall", na.rm = T)


df_test[10,1] <- 7.00001
a <- cor.test(df_test$a, df_test$b, method = "kendall")


# create the data frame
df_test <- data.frame(a = c(NA, 5, 5, NA, 7),
                      b = c(NA, 5, 5, NA, 7))

# create the data frame
df_test <- data.frame(a = c(NA, 5, 5, NA, 7),
                      b = c(NA, 5, 5, NA, 7))

# create the data frame
df_test <- data.frame(a = c(NA, 5, 5, NA, 7),
                      b = c(NA, 5, 5, NA, 7))

a <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_vor3_s9_t0.C5"]]
b <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_nac3_s9_t1.C5"]]
df_test <- as.data.frame(cbind(a,b))

# get the indices of rows with non-NA values in both variables
pairwise_indices <- which(!is.na(df_test$a) & !is.na(df_test$b))

# loop through the pairwise indices
for (i in pairwise_indices) {
  # check if the standard deviation of either variable is zero
  if (sd(df_test[i, c("a", "b")][!is.na(df_test[i, c("a", "b")])], na.rm = TRUE) == 0) {
    # generate two random but non-equal numbers in the range of the indices
    selected_indices <- sample(c("a", "b"), size = 1)
    var_to_update <- selected_indices[1]
    other_var <- setdiff(c("a", "b"), var_to_update)
    # add 0.0001 to the value at the selected index
    df_test[i, var_to_update] <- df_test[i, var_to_update] + 0.00000001
  }
}


a <- cor.test(df_test$a, df_test$b, method = "kendall")
a[["estimate"]][["tau"]]
a[["p.value"]]




a <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_vor3_s9_t0.C5"]]
b <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_nac3_s9_t1.C5"]]
df_test <- as.data.frame(cbind(a,b))


# get the indices of rows with non-NA values in both variables
pairwise_indices <- which(!is.na(df_test$a) & !is.na(df_test$b))

# loop through the pairwise indices
for (i in pairwise_indices) {
  # check if the standard deviation of either variable is zero
  if (sd(df_test[i, c("a", "b")][!is.na(df_test[i, c("a", "b")])], na.rm = TRUE) == 0) {
    # generate two random but non-equal numbers in the range of the pairwise indices
    selected_indices <- sample(pairwise_indices, size = 2, replace = FALSE)
    var_to_update <- sample(c("a", "b"), size = 1)
    # determine which variable to update
    if (var_to_update == "a") {
      df_test[selected_indices[1], "a"] <- df_test[selected_indices[1], "a"] + 0.0001
      df_test[selected_indices[2], "a"] <- df_test[selected_indices[2], "a"] + 0.0001
    } else {
      df_test[selected_indices[1], "b"] <- df_test[selected_indices[1], "b"] + 0.0001
      df_test[selected_indices[2], "b"] <- df_test[selected_indices[2], "b"] + 0.0001
    }
  }
}



a <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_vor2_s9_t0.C5"]]
b <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_nac2_s9_t1.C5"]]
df_test <- as.data.frame(cbind(a,b))

# get the indices of rows with non-NA values in both variables
pairwise_indices <- which(!is.na(df_test$a) & !is.na(df_test$b))

# loop through the pairwise indices
for (i in pairwise_indices) {
  # check if the standard deviation of either variable is zero
  if (sd(df_test[i, c("a", "b")][!is.na(df_test[i, c("a", "b")])], na.rm = TRUE) == 0) {
    print("this one:", i)
    # generate two random but non-equal numbers in the range of the pairwise indices
    selected_indices <- sample(pairwise_indices, size = 2, replace = FALSE)

    # randomly pick one of the numbers in selected_indices
    index_to_update <- sample(selected_indices, size = 1)

    # add 0.0001 to the value at the selected index in both variables
    df_test[index_to_update, "a"] <- df_test[index_to_update, "a"] + 0.0001
    df_test[index_to_update, "b"] <- df_test[index_to_update, "b"] + 0.0001
  }
}






a <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_vor3_s9_t0.C5"]]
b <- fblist_num4[["099-325_ID37"]][["099-325_ID37_joined"]][["WISS_nac3_s9_t1.C5"]]
df_test <- as.data.frame(cbind(a,b))

# get the indices of rows with non-NA values in both variables
pairwise_indices <- which(!is.na(df_test$a) & !is.na(df_test$b))
# randomly pick one of the numbers in pairwise_indices
index_to_update <- sample(pairwise_indices, size = 1)

if (sd(df_test[c(pairwise_indices), 1]) == 0 || sd(df_test[c(pairwise_indices), 2]) == 0 || sd((df_test[c(pairwise_indices), 2] - df_test[c(pairwise_indices), 1])) == 0) {
  print("yes")
  # add 0.0001 to the value at the selected index in both variables
  df_test[index_to_update, 1] <- df_test[index_to_update, 1] + 0.0001
  df_test[index_to_update, 2] <- df_test[index_to_update, 2] + 0.0001
  }

k <- Kendall::Kendall(df_test$a, df_test$b)
k

psych::corr.test(df_test$a, df_test$b, method = "kendall")

coin::wilcoxsign_test(df_test$a ~ df_test$b, data = df_test, zero.method = "Wilcoxon")


tryCatch(
  {
    coin::wilcoxsign_test(df_test$a ~ df_test$b, data = df_test, zero.method = "Wilcoxon")
  },
  error = function(e) {
    error_message <- e
    if (error_message[["message"]]  == "all pairwise differences equal zero") {
      print("replace p_test, eff_size and pwr_test with 0")
    } else {
    print("replace p_test, eff_size and pwr_test with 0")
    }
  }
)


