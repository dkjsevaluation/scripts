a <- fblist_num5[["517-334_ID28_joined"]][["WISS_vor_s9_t0.Wissen_t1"]]
b <- fblist_num5[["517-334_ID28_joined"]][["WISS_nac_s9_t1.Wissen_t1"]]

wsr_test <- coin::wilcoxsign_test(a~b)
t <- psych::corr.test(a,b, method = 'kendall')
t$r

am <- mean((a), na.rm = T)
asd <- sd((a), na.rm = T)
bm <- mean((b), na.rm = T)
bsd <- sd((b), na.rm = T)


c <- mean((a-b), na.rm = T)
d <- sd((a-b), na.rm = T)

library(MKpower)
rxy <- function(n) rnorm(n, mean = c, sd = d)
pwr <- sim.ssize.wilcox.test(rx = rxy, n.min = 1, n.max = 100, step.size = 1, iter = 1000, type = "one.sample")
pwr_df <- as.data.frame(cbind(pwr[["n"]], pwr[["emp.power"]]))
target_row <- 9
closest_row <- which.min(abs(pwr_df$V1 - target_row))
closest_value <- pwr_df[closest_row, "V2"]
closest_value

pwr[["emp.power"]]


extracted_data[[i]]$SK.diff_pwr <- pwr[["emp.power"]][[1]]



wsr_z <- abs(wsr_test@statistic@teststatistic)
wsr_r <- wsr_z/sqrt(nrow(wsr_test@statistic@x))
d <- (sqrt(4) * wsr_r)/(sqrt(1-wsr_r^2))
d


a <- fblist_num5[["099-325_ID37_joined"]][["WISS_nac3_s9_t1.C5"]]
b <- fblist_num5[["099-325_ID37_joined"]][["WISS_vor3_s9_t0.C5"]]


a <- fblist_num5[["099-325_ID37_joined"]][["WISS_nac4_s9_t1.V8"]]
b <- fblist_num5[["099-325_ID37_joined"]][["WISS_vor4_s9_t0.v8"]]


c <- Kendall::Kendall(a,b)
c[["tau"]]

library(DescTools)
c <- KendallTauB(a,b)
c
#######
library(exactRankTests)
wilcox.exact(a, b, paired = TRUE, alternative = "two.sided")

wilcox.test(a, b, paired = TRUE, alternative = "two.sided")
coin::wilcoxsign_test(a~b)

######

dftest <- as.data.frame(cbind(a,b))
dftest$ID <- paste0("ID_", rownames(dftest))
dftest_long <- tidyr::pivot_longer(dftest,
                                   cols=c('a', 'b'),
                                   names_to='group',
                                   values_to='value')
library(dplyr)
dftest_long_na <- dftest_long %>%  filter(!is.na(value))

coin::independence_test(data = dftest_long[,c(2:3)], value ~ group, distribution = "exact")
wilcox.test(data = dftest_long, value ~ group, na.action = "na.omit")


coin::independence_test(data = asat, asat ~ group, distribution = "exact")


#####

View(dftesto)
wsr_testo <- coin::wilcoxsign_test(dftesto$WISS_nac_s9_t1.Wissen_t1~dftesto$WISS_vor_s9_t0.Wissen_t1)
pvalue(wsr_testo)
wsr_z_testo <- abs(wsr_testo@statistic@teststatistic)
wsr_z_testo
wsr_r_testo <- wsr_z_testo/sqrt(nrow(wsr_testo@statistic@x))
wsr_r_testo
d_testo <- (sqrt(4) * wsr_r_testo)/(sqrt(1-wsr_r_testo^2))
d_testo

##### Fixing a problem in which N_intra was incorrect
### Problem was that the code addressed the wrong column in one place
## now fixed
library(dplyr)
df123<-fblist_num4[["383-709_ID20"]][["383-709_ID20_joined"]]
df123<-fblist_num7[["383-709_ID20_joined"]]

sum(!is.na(fblist_num7[[3]][["SK.WISS_nac"]]))

df1234 <- df123 %>% select(contains(c("WISS", "CODE")))
psych::describe(df1234)
vorher <- df1234$SK.WISS_vor
nachher <- df1234$SK.WISS_nac
df1234$SK.diff <- nachher - vorher
sum(!is.na(df1234$SK.diff))
wsr_test <- coin::wilcoxsign_test(vorher ~ nachher)


fblist_num7[["383-709_ID20_joined"]]
a <- calculate_scales_bericht("WISS_vor")
a <- calculate_scales_bericht("WISS_nac")

##### problem: intra-ind. analysis in Option2 doesn't work for ERFA when Vielfalt is included
### isolate Vielfalt
element_to_keep <- "383-709_ID20_joined"
fblist_num6_new <- fblist_num6[grepl(element_to_keep, names(fblist_num6))]
erfa_intra_results2 <- calculate_intra_statistics(fblist_num6_new, "ERFA")
View(erfa_intra_results2)
# works, so the problem is not Vielfalt data
## Option 1 also works, so the problem must be with joined | tr
## maybe some other tr data is the problem?
## iterate all tr dfs to isolate the data for which it's not working
fblist_num6[["362-075_ID1_tr_Ferienschule_Befragung_Traeger"]]
fblist_num6[["409-824_ID13_tr_Evaluation_Netzwerke_fuer_Demokratie_retro"]]

# Delete the elements from the list by name
fblist_num6_new <- fblist_num6
fblist_num6_new[["362-075_ID1_tr_Ferienschule_Befragung_Traeger"]] <- NULL
erfa_intra_results2 <- calculate_intra_statistics(fblist_num6_new, "ERFA")
# doesn't work
fblist_num6_new <- fblist_num6
fblist_num6_new[["409-824_ID13_tr_Evaluation_Netzwerke_fuer_Demokratie_retro"]] <- NULL
erfa_intra_results2 <- calculate_intra_statistics(fblist_num6_new, "ERFA")
# works, so the problem is in fblist_num6[["409-824_ID13_tr_Evaluation_Netzwerke_fuer_Demokratie_retro"]]
fblist_num6_new <- fblist_num6
element_to_keep <- "409-824_ID13_tr_Evaluation_Netzwerke_fuer_Demokratie_retro"
fblist_num6_new <- fblist_num6[grepl(element_to_keep, names(fblist_num6))]

# test parts of failing function
con_vor <- paste0("ERFA", "_vor")
con_nac <- paste0("ERFA", "_nac")

gr_vor <- paste0("ERFA", ".*_vor")
gr_nac <- paste0("ERFA", ".*_nac")

# Create an empty data frame to store the results
pair_registry_vor <- data.frame(FB = character(), vor = character(), stringsAsFactors = FALSE)

# Loop through each data frame in the list
for (i in seq_along(fblist_num6_new)) {
  # Get the name of the current data frame
  fb_name <- names(fblist_num6_new)[i]
  # Extract the variables that match the pattern using grep
  vor_vars <- grep(gr_vor, names(fblist_num6_new[[i]]), value = TRUE)
  # Add the results to the pair_registry data frame
  pair_registry_vor <- rbind(pair_registry_vor, data.frame(FB = fb_name, vor = vor_vars, stringsAsFactors = FALSE))
}

### scheitert, weil im FB nicht ERFA, sondern FAEH drin ist. Entsprechend kommt Error raus.
## Lösungen: entweder ein Check vorher machen und dfs ohne ERFA/ FAEH jeweils löschen
## Lösungen: trycatch() basiertes 'übergehen'
## erste Lösung weniger komplex und fehleranfällig -> machen
# scan the dfs in the list vor ERFA in the variable names. If not - delete list element
## Lösungen: vorher FAEH in ERFA umcodieren
## das ist eigentlich besser, weil man dann Syntax reduzieren kann
## Doch Lösung eins besser, weil es dann flexibler auch für andere Konstrukte läuft
## das sollte man aber in die Funktion einbauen, damit es dann unabhängig vom Konstrukt läuft
## das machen

# If there are no variables with the construct, delete the df from the list and print a Warning
# Assuming 'df_list' is your list of data frames
# Assuming 'variable_name_part' is the specific variable name part you want to check

# Create an empty list to store the updated data frames
updated_df_list <- list()
variable_name_part <- "ERFA"
# Loop through each data frame in the original list
for (i in seq_along(fblist_num6_new)) {
  # Check if the specific variable name part exists in the data frame
  if (any(grepl(variable_name_part, names(fblist_num6_new[[i]])))) {
    # If the variable name part exists, add the data frame to the updated list
    updated_df_list[[length(updated_df_list) + 1]] <- fblist_num6_new[[i]]
  } else {
    # If the variable name part doesn't exist, print a warning
    warning(paste0(variable_name_part, " not found in df ", names(fblist_num6_new[i])))
  }
}
# Replace the original list with the updated list
df_list <- updated_df_list

variable_name_part <- "ERFA"

# Loop through each data frame in the original list in reverse order
for (i in seq_along(fblist_num6_new)) {
  # Check if the specific variable name part exists in the data frame
  if (!any(grepl(variable_name_part, names(fblist_num6_new[[i]])))) {
    # If the variable name part doesn't exist, remove the data frame from the list
    fblist_num6_new <- fblist_num6_new[-i]
    warning(paste0(variable_name_part, " not found in df ", names(fblist_num6_new[i])))
  }
}


# Assuming 'fblist_num6_new' is your list of data frames
# Assuming 'variable_name_part' is the specific variable name part you want to check
fblist_num6_new <- fblist_num6
names(fblist_num6_new)
variable_name_part <- "FAEH"

# Assuming 'fblist_num6_new' is your list of data frames
# Assuming 'variable_name_part' is the specific variable name part you want to check

# Loop through each index in reverse order
for (i in rev(seq_along(fblist_num6_new))) {
  # Check if the specific variable name part exists in the data frame
  if (!any(grepl(variable_name_part, names(fblist_num6_new[[i]])))) {
    # If the variable name part doesn't exist, remove the data frame from the list
    df_name <- names(fblist_num6_new)[i]  # Get the name of the current data frame
    fblist_num6_new <- fblist_num6_new[-i]
    warning(paste0(variable_name_part, " not found in df ", df_name))
  }
}

construct <- "FAEH"
con_vor <- paste0(construct, "_vor")
con_nac <- paste0(construct, "_nac")

gr_vor <- paste0(construct, ".*_vor")
gr_nac <- paste0(construct, ".*_nac")

# If there are no variables with the construct, delete the df from the list and print a Warning
# Loop through each index
variable_name_part <- construct
for (i in rev(seq_along(fbliste))) {
  # Check if the specific variable name part exists in the data frame
  if (!any(grepl(variable_name_part, names(fbliste[[i]])))) {
    # If the variable name part doesn't exist, remove the data frame from the list
    df_name <- names(fbliste)[i]  # Get the name of the current data frame
    fbliste <- fbliste[-i]
    warning(paste0(variable_name_part, " not found in df ", df_name))
  }
}




fbliste <- fblist_num6
names(fbliste)
construct <- "FAEH"

con_vor <- paste0(construct, "_vor")
con_nac <- paste0(construct, "_nac")

gr_vor <- paste0(construct, ".*_vor")
gr_nac <- paste0(construct, ".*_nac")

# If there are no variables with the construct, delete the df from the list and print a message
# Loop through each index
variable_name_part <- construct
for (i in seq_along(fbliste)) {
  # Check if both gr_vor and gr_nac patterns exist in the data frame
  if (!any(grepl(gr_vor, names(fbliste[[i]]))) && !any(grepl(gr_nac, names(fbliste[[i]])))) {
    # If both patterns don't exist, remove the data frame from the list
    df_name <- names(fbliste)[i]  # Get the name of the current data frame
    fbliste <- fbliste[-i]
    message(paste0(variable_name_part, " not found in df ", df_name))
  }
}



fbliste <- fblist_num6
names(fbliste)
construct <- "Yo"
con_vor <- paste0(construct, "_vor")
con_nac <- paste0(construct, "_nac")

gr_vor <- paste0(construct, ".*_vor")
gr_nac <- paste0(construct, ".*_nac")

# If there are no variables with the construct, delete the df from the list and print a message
# Loop through each index
variable_name_part <- construct
for (i in rev(seq_along(fbliste))) {
  # Check if both gr_vor and gr_nac patterns exist in the data frame
  if (!any(grepl(gr_vor, names(fbliste[[i]]))) | !any(grepl(gr_nac, names(fbliste[[i]])))) {
    # If either pattern doesn't exist, remove the data frame from the list
    df_name <- names(fbliste)[i]  # Get the name of the current data frame
    fbliste <- fbliste[-i]
    message(paste0(variable_name_part, " with *_vor as well as *_nac in variables not found in df ", df_name))
  }
}

names(fbliste)
if (length(fbliste) == 0) {
  message("There are no dfs in the list with this construct name. Sorry.")
}
