####

##### Boxplots für Variablen einer Skala plus Skalenmittelwert innerhalb einer Stichprobe erstellen

#df.1 <-df %>% select(contains("FAEH") & contains("C2") | contains("CODE"), -contains("rel"))
#df.1_long <- df.1 %>% pivot_longer(cols = c(names(df.1[,1:9])), names_to = "variables", values_to = "values")
#df.1_long.f <- transform(df.1_long, facet = ifelse(variables %in% c("SK.FAEH.C2"), 2, 1))

df.1 <-df %>% select(contains("FAEH") & contains("C2") | contains("CODE"), -contains("rel"), -contains("SK"))
df.1_long <- df.1 %>% pivot_longer(cols = c(names(df.1[,1:8])), names_to = "variables", values_to = "values")

df.2 <-df %>% select(contains("FAEH") & contains("C2") & contains("SK")| contains("CODE"), -contains("rel"))
df.2_long <- df.2 %>% pivot_longer(cols = c(colnames(df.2[1])), names_to = "variables", values_to = "values")
df.2_long.1 <- df.2_long %>% mutate(reliabilitaet = df$SK.FAEH.C2.rel)
#df.2_long.1 <- df.2_long %>% mutate(rel = 0.1)   # zum Check für die Farbe des Skalenboxplots


plot.scale <- function(df, variables, values) {
  ggplot(data = df, aes(x = variables, y = values)) +
    scale_x_discrete(limits = rev(c(unlist(c(unique(df[,2])))))) +
    scale_y_continuous(limits = c(0,3), breaks = seq(0,3,1)) +
    geom_boxplot(outlier.alpha = 0.1) +
    geom_jitter(height = 0.1, alpha = 0.1) +
    stat_boxplot(geom = 'errorbar', width = 0.25) +
    stat_summary(fun = "mean", color = "gray", geom = "line", aes(group = 1)) +
    stat_summary(fun = "mean", color = "black", shape = "|") +
    coord_flip() +
    theme_light() +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title = element_blank())
}

p1 <- plot.scale(df.1_long) + theme(axis.text.x = element_blank())
#p2 <- plot.scale(df.2_long.1) + geom_boxplot(aes(fill = rel))
p2 <- plot.scale(df.2_long.1) + geom_boxplot(aes(fill = reliabilitaet), alpha = 0.3) +
  scale_fill_gradientn(c("Reliabilität"), colors = c("#DB432599", "#02d42599"), labels=c("Min",0.6,"Max"), breaks=c(0,0.6,1), limits=c(0,1)) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_text(vjust = 0.85, hjust = 1.75)) +
  guides(fill = guide_colorbar(override.aes = list(alpha = 0.01), title.position = "left"))

cowplot::plot_grid(p1,p2, align = "v", nrow = 2, rel_heights = c(6/8,2/8))
