
########################
####### vorher #########
########################

freq_inter_collected <- function(df_name, listname) {

  df_var <- as.data.frame(cbind(df_name[,1], df_name[,2]))
  colnames(df_var) <- c("V1", "V2")
  # Remove the "_desc" suffix from the V2 column
  df_var[, "V2"] <- gsub("_desc$", "", df_var[, "V2"])

  # Create an empty list to hold the extracted variables
  vars_list <- list()

  # Loop through each row in df_var
  for (i in 1:nrow(df_var)) {
    # Get the name from column V1
    name <- df_var[i, "V1"]
    # Find the corresponding nested list in fblist_num3
    sublist <- listname[[name]]
    # Check if the sublist exists
    if (is.null(sublist)) {
      message(paste0("No sublist found for ", name))
      next
    }
    # Get the variable name from column V2
    V2 <- df_var[i, "V2"]
    # Check if V2 exists in the nested list
    if (!(V2 %in% names(sublist[[1]]))) {
      message(paste0("No variable named ", V2, " in sublist ", name))
      next
    }
    # Loop through each data frame in the sublist
    for (j in 1:length(sublist)) {
      # Check if the data frame contains the variable from column V2
      if (V2 %in% names(sublist[[j]])) {
        # Extract the variable and add it to the list with a name based on V2 and V1
        var <- sublist[[j]][[V2]]
        new_length <- 3500
        num_nas <- new_length - length(var)
        length(var) <- new_length
        var[(length(var) - num_nas + 1):length(var)] <- NA

        var_name <- paste0(df_var[i, "V1"], "_", V2, "_", j)
        vars_list[[var_name]] <- var
      }
    }
  }
return(vars_list)
}

all_categs <- 0:9

freq_wiss_vor <-  freq_inter_collected(df_WISS_vor, fblist_num3)
df_freq_wiss_vor <- as.data.frame(freq_wiss_vor)
df_freq_wiss_vor <- lapply(df_freq_wiss_vor, factor, levels = all_categs)
#hist(unlist(df_freq_wiss_vor), main = paste0("Häufigkeitstabelle für Wissen vorher"))
freq_wiss_vor_table <- table(unlist(df_freq_wiss_vor))
relfreq_wiss_vor_table <- prop.table(table(unlist(df_freq_wiss_vor)))


freq_erfa_vor <-  freq_inter_collected(df_ERFA_vor, fblist_num3)
df_freq_erfa_vor <- as.data.frame(freq_erfa_vor)
df_freq_erfa_vor <- lapply(df_freq_erfa_vor, factor, levels = all_categs)
#hist(unlist(df_freq_erfa_vor), main = paste0("Häufigkeitstabelle für Erfahrung vorher"))
freq_erfa_vor_table <-table(unlist(df_freq_erfa_vor))
relfreq_erfa_vor_table <- prop.table(table(unlist(df_freq_erfa_vor)))


freq_faeh_vor <-  freq_inter_collected(df_FAEH_vor, fblist_num3)
df_freq_faeh_vor <- as.data.frame(freq_faeh_vor)
df_freq_faeh_vor <- lapply(df_freq_faeh_vor, factor, levels = all_categs)
#hist(unlist(df_freq_faeh_vor), main = paste0("Häufigkeitstabelle für Erfahrung vorher"))
freq_faeh_vor_table <- table(unlist(df_freq_faeh_vor))
relfreq_faeh_vor_table <- prop.table(table(unlist(df_freq_faeh_vor)))




#########################
####### nachher #########
#########################


freq_inter_collected2 <- function(df_name, listname) {

  df_var <- as.data.frame(cbind(df_name[,1], df_name[,2]))
  colnames(df_var) <- c("V1", "V2")
  # Remove the "_desc" suffix from the V2 column
  df_var[, "V2"] <- gsub("_desc$", "", df_var[, "V2"])


  # Create an empty list to hold the extracted variables
  vars_list <- list()

  for (i in 1:nrow(df_var)) {
    # Get the name from column V1
    name <- df_var[i, 1]
    print(name)
    # Find the corresponding nested list in fblist_num3
    sublist <- fblist_num3[[name]]
    # Check if the sublist exists

    if (is.null(sublist)) {
      message(paste0("No sublist found for ", name))
    }

    # Get the variable name from column V2
    V2 <- df_var[i, 2]
    if (is.na(V2) || V2 == "") {
      message(paste0("Skipping empty V2 value for ", name))
      next
    }

    print(V2)
    # Loop through each data frame in the sublist
    for (j in 1:length(sublist)) {
      # Check if the data frame contains the variable from column V2
      if (V2 %in% names(sublist[[j]])) {
        # Extract the variable and add it to the list with a name based on V2 and V1
        var <- sublist[[j]][[V2]]
        new_length <- 3500
        num_nas <- new_length - length(var)
        length(var) <- new_length
        var[(length(var) - num_nas + 1):length(var)] <- NA
        var_name <- paste0(df_var[i, "V1"], "_", V2, "_", j)
        vars_list[[var_name]] <- var
        V2 <- NULL
      }
    }
  }
  return(vars_list)
}

freq_wiss_nac <-  freq_inter_collected(df_WISS_nac, fblist_num3)
df_freq_wiss_nac <- as.data.frame(freq_wiss_nac)
df_freq_wiss_nac <- lapply(df_freq_wiss_nac, factor, levels = all_categs)
#hist(unlist(df_freq_wiss_nac), main = paste0("Relative Häufigkeiten für Wissen nachher"))
freq_wiss_nac_table <- table(unlist(df_freq_wiss_nac))
relfreq_wiss_nac_table <- prop.table(table(unlist(df_freq_wiss_nac)))


freq_erfa_nac <-  freq_inter_collected(df_ERFA_nac, fblist_num3)
df_freq_erfa_nac <- as.data.frame(freq_erfa_nac)
df_freq_erfa_nac <- lapply(df_freq_erfa_nac, factor, levels = all_categs)
#hist(unlist(df_freq_erfa_nac), main = paste0("Relative Häufigkeiten für Erfahrung nachher"))
freq_erfa_nac_table <- table(unlist(df_freq_erfa_nac))
relfreq_erfa_nac_table <- prop.table(table(unlist(df_freq_erfa_nac)))


freq_faeh_nac <-  freq_inter_collected(df_FAEH_nac, fblist_num3)
df_freq_faeh_nac <- as.data.frame(freq_faeh_nac)
df_freq_faeh_nac <- lapply(df_freq_faeh_nac, factor, levels = all_categs)
#hist(unlist(df_freq_faeh_nac), main = paste0("Relative Häufigkeiten für Fähigkeiten nachher"))
freq_faeh_nac_table <-table(unlist(df_freq_faeh_nac))
relfreq_faeh_nac_table <- prop.table(table(unlist(df_freq_faeh_nac)))



library(ggplot2)
# create a data frame with the frequency tables
df <- data.frame(Konstrukt = factor(rep(c("Wissen vorher", "Wissen nachher"), each = length(relfreq_wiss_vor_table)),
                                    levels = c("Wissen vorher", "Wissen nachher")),
                 Wert = rep(1:length(relfreq_wiss_vor_table), times = 2),
                 relfreq = c(relfreq_wiss_vor_table, relfreq_wiss_nac_table))
results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/freq_tables/"
write.xlsx(df, paste0(results_path, "rel_haeufigkeit_wiss_vor_nac", "_", Sys.Date(), "_.xlsx"))


# create the ggplot object
p <- ggplot(df, aes(x = Wert - 1, y = relfreq, fill = Konstrukt)) +
  geom_bar(position = position_dodge2(width = 0.8, preserve = "single"), stat = "identity", width = 0.8) +
  labs(title = "Selbsteingeschätztes Wissen im Vorher-Nachher Vergleich",
       x = "Angekreuzte Werte",
       y = "Prozentuale Häufigkeit") +
  scale_fill_manual(values = c("grey","#009DB5")) +
  scale_x_discrete(limits = c(0:9), expand = c(0,0))+
  theme_classic()

# add the percentage labels to the bars
p <- p + geom_text(aes(x = Wert - 1.03,
                       y = relfreq + 0.03,
                       label = paste0(round(relfreq * 100, digits = 0), "%")),
                        size = 3,
                        angle = 90,
                        position = position_dodge(width = 0.8))

# customize the y-axis labels to show percentages
p <- p + scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5))

# customize title and axis titles
p <- p + theme(axis.title.x = element_text(hjust=0.5, margin = margin(t = 10)),
               axis.title.y = element_text(hjust=0.5, margin = margin(r = 10))) +
          theme(plot.title = element_text(hjust = 0))

# display the plot
p






library(ggplot2)
# create a data frame with the frequency tables
df <- data.frame(Konstrukt = factor(rep(c("Erfahrung vorher", "Erfahrung nachher"), each = length(relfreq_erfa_vor_table)),
                                    levels = c("Erfahrung vorher", "Erfahrung nachher")),
                 Wert = rep(1:length(relfreq_erfa_vor_table), times = 2),
                 relfreq = c(relfreq_erfa_vor_table, relfreq_erfa_nac_table))
results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/freq_tables/"
write.xlsx(df, paste0(results_path, "rel_haeufigkeit_erfah_vor_nac", "_", Sys.Date(), "_.xlsx"))


# create the ggplot object
p <- ggplot(df, aes(x = Wert - 1, y = relfreq, fill = Konstrukt)) +
  geom_bar(position = position_dodge2(width = 0.8, preserve = "single"), stat = "identity", width = 0.8) +
  labs(title = "Selbsteingeschätzte Erfahrung im Vorher-Nachher Vergleich",
       x = "Angekreuzte Werte",
       y = "Prozentuale Häufigkeit") +
  scale_fill_manual(values = c("grey","#009DB5")) +
  scale_x_discrete(limits = c(0:9), expand = c(0,0))+
  theme_classic()

# add the percentage labels to the bars
p <- p + geom_text(aes(x = Wert - 1.03,
                       y = relfreq + 0.03,
                       label = paste0(round(relfreq * 100, digits = 0), "%")),
                   size = 3,
                   angle = 90,
                   position = position_dodge(width = 0.8))

# customize the y-axis labels to show percentages
p <- p + scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5))

# customize title and axis titles
p <- p + theme(axis.title.x = element_text(hjust=0.5, margin = margin(t = 10)),
               axis.title.y = element_text(hjust=0.5, margin = margin(r = 10))) +
  theme(plot.title = element_text(hjust = 0))

# display the plot
p



df1 <- as.data.frame(freq_erfa_vor_table)
df2 <- as.data.frame(freq_faeh_vor_table)
names(df2) <- names(df1)
merged_df_vor <- merge(df1, df2, by = "Var1")
merged_df_vor$freq <- merged_df_vor$Freq.x + merged_df_vor$Freq.y
merged_df_vor$Freq.x <- NULL
merged_df_vor$Freq.y <- NULL

df1 <- as.data.frame(freq_erfa_nac_table)
df2 <- as.data.frame(freq_faeh_nac_table)
names(df2) <- names(df1)
merged_df_nac <- merge(df1, df2, by = "Var1")
merged_df_nac$freq <- merged_df_nac$Freq.x + merged_df_nac$Freq.y
merged_df_nac$Freq.x <- NULL
merged_df_nac$Freq.y <- NULL

freq_faeh_erfa_nac_table <- xtabs(freq ~ Var1, merged_df_nac)
relfreq_faeh_erfa_nac_table <- prop.table(xtabs(freq ~ Var1, merged_df_nac))

freq_faeh_erfa_vor_table <- xtabs(freq ~ Var1, merged_df_vor)
relfreq_faeh_erfa_vor_table <- prop.table(xtabs(freq ~ Var1, merged_df_vor))



library(ggplot2)
# create a data frame with the frequency tables
df <- data.frame(Konstrukt = factor(rep(c("Fähigkeit vorher", "Fähigkeit nachher"), each = length(relfreq_faeh_erfa_vor_table)),
                                    levels = c("Fähigkeit vorher", "Fähigkeit nachher")),
                 Wert = rep(1:length(relfreq_faeh_erfa_vor_table), times = 2),
                 relfreq = c(relfreq_faeh_erfa_vor_table, relfreq_faeh_erfa_nac_table))
results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/freq_tables/"
write.xlsx(df, paste0(results_path, "rel_haeufigkeit_faeh_erfa_vor_nac", "_", Sys.Date(), "_.xlsx"))


# create the ggplot object
p <- ggplot(df, aes(x = Wert - 1, y = relfreq, fill = Konstrukt)) +
  geom_bar(position = position_dodge2(width = 0.8, preserve = "single"), stat = "identity", width = 0.8) +
  labs(title = "Selbsteingeschätzte Fähigkeiten im Vorher-Nachher Vergleich",
       x = "Angekreuzte Werte",
       y = "Prozentuale Häufigkeit") +
  scale_fill_manual(values = c("grey","#009DB5")) +
  scale_x_discrete(limits = c(0:9), expand = c(0,0))+
  theme_classic()

# add the percentage labels to the bars
p <- p + geom_text(aes(x = Wert - 1.03,
                       y = relfreq + 0.03,
                       label = paste0(round(relfreq * 100, digits = 0), "%")),
                   size = 3,
                   angle = 90,
                   position = position_dodge(width = 0.8))

# customize the y-axis labels to show percentages
p <- p + scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5))

# customize title and axis titles
p <- p + theme(axis.title.x = element_text(hjust=0.5, margin = margin(t = 10)),
               axis.title.y = element_text(hjust=0.5, margin = margin(r = 10))) +
  theme(plot.title = element_text(hjust = 0))

# display the plot
p















freq_values_vor <- round(relfreq_wiss_vor_table * 100, digits = 2)
freq_values_nac <- round(relfreq_wiss_nac_table * 100, digits = 2)
max_freq_value <- max(max(freq_values_vor), max(freq_values_nac))
library(scales)
library(ggplot2)
# Create a barplot
a <- barplot(rbind(relfreq_wiss_vor_table,relfreq_wiss_nac_table), beside = TRUE, horiz = FALSE,
             main = "Relative Häufigkeiten für Fähigkeit",
             xlab = "Angekreuzte Werte", ylab = "Häufigkeit",
             xlim = c(0,9), ylim = c(0, 0.5),
             width = 0.2,
             legend.text = c("Wissen vorher", "Wissen nachher"),
             col = c("grey", "#009DB5"))

# Add text labels to the barplot
text(x = a[c(1,3,5,7,9,11,13,15,17,19)] - 0.03,
     y = relfreq_wiss_vor_table + 0.03,
     labels = paste0(round(relfreq_wiss_vor_table * 100, digits = 2), "%"),
     col = "black",
     cex = 0.6,
     srt = 90)

text(x = a[c(2,4,6,8,10,12,14,16,18,20)] - 0.02,
     y = relfreq_wiss_nac_table + 0.03,
     labels = paste0(round(relfreq_wiss_nac_table * 100, digits = 2), "%"),
     col = "black",
     cex = 0.6,
     srt = 90)











a <- barplot(rbind(relfreq_wiss_vor_table,relfreq_wiss_nac_table), beside = TRUE, horiz = FALSE,
        main = "Relative Häufigkeiten für Fähigkeit",
        xlab = "Gültige Werte", ylab = "Häufigkeit",
        xlim = c(0,9), ylim = c(0,0.5),
        width = 0.2,
        legend.text = TRUE)
text(x = a - 0.1, y = relfreq_wiss_vor_table + 0.03, labels = relfreq_wiss_vor_table, col = "black", cex = 1)
text(x = a + 0.1, y = relfreq_wiss_nac_table + 0.03, labels = relfreq_wiss_nac_table, col = "black", cex = 1)


par(mfrow = c(3:2))

barplot(relfreq_wiss_vor_table, beside = TRUE, horiz = FALSE,
        main = "Relative Häufigkeiten für Wissen vorher",
        xlab = "Gültige Werte", ylab = "Häufigkeit",
        xlim = c(0,9), ylim = c(0,0.4),
        width = 0.5)
barplot(relfreq_wiss_nac_table, beside = TRUE, horiz = FALSE,
             main = "Relative Häufigkeiten für Wissen nachher",
             xlab = "Gültige Werte", ylab = "Häufigkeit",
             xlim = c(0,9), ylim = c(0,0.4),
             width = 0.5)


barplot(relfreq_erfa_vor_table, beside = TRUE, horiz = FALSE,
        main = "Relative Häufigkeiten für Erfahrung vorher",
        xlab = "Gültige Werte", ylab = "Häufigkeit",
        xlim = c(0,9), ylim = c(0,0.4),
        width = 0.5)
barplot(relfreq_erfa_nac_table, beside = TRUE, horiz = FALSE,
        main = "Relative Häufigkeiten für Erfahrung nachher",
        xlab = "Gültige Werte", ylab = "Häufigkeit",
        xlim = c(0,9), ylim = c(0,0.4),
        width = 0.5)


barplot(relfreq_faeh_vor_table, beside = TRUE, horiz = FALSE,
        main = "Relative Häufigkeiten für Fähigkeit vorher",
        xlab = "Gültige Werte", ylab = "Häufigkeit",
        xlim = c(0,9), ylim = c(0,0.4),
        width = 0.5)
barplot(relfreq_faeh_nac_table, beside = TRUE, horiz = FALSE,
        main = "Relative Häufigkeiten für Fähigkeit nachher",
        xlab = "Gültige Werte", ylab = "Häufigkeit",
        xlim = c(0,9), ylim = c(0,0.4),
        width = 0.5)



# create a vector of values from 0 to 9
all_values <- 0:9

# set the names of the categories that have frequencies
freq_values <- c(4, 5, 6, 7, 8)
names(freq_values) <- freq_values

# combine the frequency data with the missing categories
all_freq <- c(freq_values, rep(0, length(all_values) - length(freq_values)))

# create the barplot
barplot(all_freq, beside = TRUE, horiz = FALSE,
        main = "Relative Häufigkeiten für Fähigkeit nachher",
        xlab = "Gültige Werte", ylab = "Häufigkeit",
        xlim = c(0, 9), ylim = c(0, 0.4),
        width = 0.5)




freq_wiss_vor_table
freq_wiss_nac_table
freq_erfa_vor_table
freq_erfa_nac_table
freq_faeh_vor_table
freq_faeh_nac_table

