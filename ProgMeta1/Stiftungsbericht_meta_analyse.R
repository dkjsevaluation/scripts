##### Meta-Analyse #####


#################################################
######## intra-individuelle Daten Option 3 ######
#################################################

wiss_intra_statistics$order <- c(1:nrow(wiss_intra_statistics))
erfa_faeh_intra_statistics$order <- c(1:nrow(erfa_faeh_intra_statistics))

##########################
######### WISS ###########
##########################


###### calculate within standardized mean difference and SE as effect size

library(metafor)
library(metaviz)

df.wiss.intra <- escalc(measure = "SMCR",
                         m1i = M_nac, m2i = M_vor,
                         sd1i = SD_nac, sd2i = SD_vor,
                        ni = N_intra, ri = kendall,
                         data = wiss_intra_statistics)

#df.wiss.intra <- escalc(measure = "MC",
#                        m1i = M_nac, m2i = M_vor,
#                        sd1i = SD_nac, sd2i = SD_vor,
#                        ni = N_intra, ri = kendall,
#                        data = wiss_intra_statistics)

#df.wiss.intra <- escalc(measure = "SMD",
#                        m1i = M_nac, m2i = M_vor,
#                        sd1i = SD_nac, sd2i = SD_vor,
#                        n1i = N_intra, n2i = N_intra,
#                        data = wiss_intra_statistics)

#erfa_intra_statistics$d1<- rbind(1.58,1.66,1.81,1.46,1.73,0.71,0.44)
#erfa_intra_statistics$d1<- rbind(1.59,1.15,0.46,1.46,1.74,0.46)

df.wiss.intra <- escalc(measure = "SMD",
                        m1i = M_nac, m2i = M_vor,
                        sd1i = SD_nac, sd2i = SD_vor,
                        n1i = N_intra, n2i = N_intra,
                        di = d_value,
                        data = wiss_intra_statistics)



####replace NA with column averages
df.wiss.intra <- as.data.frame(lapply(df.wiss.intra, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}))


###### Set up basic models

#Basic FE Model

#model.fe.wiss.intra <- rma(yi, vi, data=df.wiss.intra, method = "FE")
#model.fe.wiss.intra

##Basic RE Model

model.re.wiss.intra <- rma(yi, vi, data=df.wiss.intra, method = "REML")
model.re.wiss.intra


##sensitivity Analysis

##Outliers
# calculate influence diagnostics
inf.wiss.intra <- influence(model.re.wiss.intra)
inf.wiss.intra
results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/meta_analyse/"
write.xlsx(print(inf.wiss.intra), paste0(results_path, "wiss_outlier", "_", Sys.Date(), ".xlsx"))


# plot the influence diagnostics
plot(inf.wiss.intra)

##exclude outliers and compare models with and without
df.wiss.intra.noutliers <- subset(df.wiss.intra,
                                  (!df.wiss.intra$Programm.ID2 == "676-412_ID10")
                                    )

model.re.wiss.intra.noutliers <- rma(yi, vi, data=df.wiss.intra.noutliers, method = "REML")
model.re.wiss.intra.noutliers

dat.comp <- data.frame(estimate = c(coef(model.re.wiss.intra.noutliers), coef(model.re.wiss.intra)), stderror = c(model.re.wiss.intra.noutliers$se, model.re.wiss.intra$se),
                        outliers = c("without","with"), tau2 = round(c(model.re.wiss.intra.noutliers$tau2, model.re.wiss.intra$tau2),3))
dat.comp

rma(estimate, sei=stderror, mods = ~ outliers, method="FE", data=dat.comp, digits=3)

##Eggers regression test (significance indicates asymmetry)
regtest(model.re.wiss.intra, model = "lm")

##Beggs rank-correlation test
ranktest(model.re.wiss.intra)

# contour-enhanced funnel plot centered at 0 (see Peters et al., 2008)
funnel(model.re.wiss.intra, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),
       refline=0, atransf=exp, at=log(c(.10, .25, .5, 1, 2, 4, 10)))

viz_funnel(model.re.wiss.intra, method = "REML",
           x_trans_function = exp, xlab = "Within Standardized Mean Difference with small N correction",
           contours = TRUE, sig_contours = FALSE, contours_col = "Blues",
           egger = TRUE, trim_and_fill = TRUE)

# trim and fill
taf.wiss.intra <- trimfill(model.re.wiss.intra)
funnel(taf.wiss.intra)

#sunset plot for power
viz_sunset(model.re.wiss.intra,
           xlab = "Standardized Mean Difference (SMCR)", x_breaks = c(-0.5, 0, 0.5, 1, 2),
           contours = TRUE,
           power_contours =  "continuous")


#calculate individual power
s_power <- function(se, true_effect, sig_level) {

  (1 - stats::pnorm(stats::qnorm(1 - sig_level/2) *
                      se, abs(true_effect), se)) +
    stats::pnorm(stats::qnorm(sig_level/2) *
                   se, abs(true_effect), se)
}

funnel.re.wiss.intra <- funnel(model.re.wiss.intra, main="Standard Error")

df.wiss.intra$power <- s_power(se = funnel.re.wiss.intra$y,
                                true_effect = 1.1987,  #insert overall effect from model
                                sig_level = 0.05)
df.wiss.intra$power <- ifelse(round(df.wiss.intra$power, digits = 2) == 1, 0.99, df.wiss.intra$power)

# make forrest plot
# extract study weights
weights.model.re.wiss.intra <- cbind(paste0(formatC(weights(model.re.wiss.intra), format="f", digits=1, width=4), "%"))
df.wiss.intra$weight <- weights.model.re.wiss.intra

forest.rma(model.re.wiss.intra,
           xlim=c(-3,11.5), ylim=c(-0.5, 13),
           header= c("Programm", "Effektgröße [95% CI]"),
           xlab = "Standardized Mean Difference (SMCR)",
           slab = c("ID1","ID20","ID42","ID13","ID3","ID4", "ID10", "ID5","ID19","ID8"),
           at=c(-2, 0, 2, 4, 6), digits=c(2L),
           ilab= cbind(df.wiss.intra$N_intra, round(df.wiss.intra$power, digits = 2), df.wiss.intra$weight),
           ilab.xpos = c(5.8, 6.8, 8),
           cex = 0.7,
            order = wiss_intra_statistics$order,
           #order = "obs",
           addfit = F,
)


abline(h=0.5)
addpoly(model.re.wiss.intra, row=0, cex = 0.7)
text(5.8, 12, "N", cex = 0.7, font = 2)
text(6.8, 12, "Power", cex = 0.7, font = 2)
text(8, 12, "Gewicht", cex = 0.7, font = 2)
segments(coef(model.re.wiss.intra), -0.1, coef(model.re.wiss.intra), (model.re.wiss.intra$k)+0.5, lty="dotted")


### add text with Q-value, dfs, p-value, and I^2 statistic
text(4, 0, pos=4, cex=0.7, bquote(paste("(Q = ",
                                              .(formatC(model.re.wiss.intra$QE, digits=2, format="f")), ", df = ", .(model.re.wiss.intra$k - model.re.wiss.intra$p),
                                              ", p = ", .(formatC(model.re.wiss.intra$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                              .(formatC(model.re.wiss.intra$I2, digits=1, format="f")), "%)")))


##########################
######### ERFA ###########
##########################

###### calculate within standardized mean difference and SE as effect size

library(metafor)
library(metaviz)

df.erfa.intra <- escalc(measure = "SMCR",
                        m1i = M_nac, m2i = M_vor,
                        sd1i = SD_nac, sd2i = SD_vor,
                        ni = N_intra, ri = kendall,
                        data = erfa_faeh_intra_statistics)

#df.erfa.intra <- escalc(measure = "MC",
#                        m1i = M_nac, m2i = M_vor,
#                        sd1i = SD_nac, sd2i = SD_vor,
#                        ni = N_intra, ri = kendall,
#                        data = erfa_intra_statistics)

#df.erfa.intra <- escalc(measure = "SMD",
#                        m1i = M_nac, m2i = M_vor,
#                        sd1i = SD_nac, sd2i = SD_vor,
#                        n1i = N_intra, n2i = N_intra,
#                        data = erfa_intra_statistics)

#erfa_intra_statistics$d1<- rbind(1.58,1.66,1.81,1.46,1.73,0.71,0.44)
#erfa_intra_statistics$d1<- rbind(1.59,1.15,0.46,1.46,1.74,0.46)

df.erfa.intra <- escalc(measure = "SMD",
                        m1i = M_nac, m2i = M_vor,
                        sd1i = SD_nac, sd2i = SD_vor,
                        n1i = N_intra, n2i = N_intra,
                        di = d_value,
                        data = erfa_faeh_intra_statistics)



####replace NA with column averages
df.erfa.intra <- as.data.frame(lapply(df.erfa.intra, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}))


###### Set up basic models

#Basic FE Model

#model.fe.erfa.intra <- rma(yi, vi, data=df.erfa.intra, method = "FE")
#model.fe.erfa.intra

##Basic RE Model

model.re.erfa.intra <- rma(yi, vi, data=df.erfa.intra, method = "REML")
model.re.erfa.intra


##sensitivity Analysis

##Outliers
# calculate influence diagnostics
inf.erfa.intra <- influence(model.re.erfa.intra)
inf.erfa.intra
results_path <- "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/meta_analyse/"
write.xlsx(print(inf.erfa.intra), paste0(results_path, "erfa_outlier", "_", Sys.Date(), ".xlsx"))


# plot the influence diagnostics
plot(inf.erfa.intra)

##exclude outliers and compare models with and without
df.erfa.intra.noutliers <- subset(df.erfa.intra,
                                  (!df.erfa.intra$Programm.ID2 == "676-412_ID10")
)

model.re.erfa.intra.noutliers <- rma(yi, vi, data=df.erfa.intra.noutliers, method = "REML")
model.re.erfa.intra.noutliers

dat.comp <- data.frame(estimate = c(coef(model.re.erfa.intra.noutliers), coef(model.re.erfa.intra)), stderror = c(model.re.erfa.intra.noutliers$se, model.re.erfa.intra$se),
                       outliers = c("without","with"), tau2 = round(c(model.re.erfa.intra.noutliers$tau2, model.re.erfa.intra$tau2),3))
dat.comp

rma(estimate, sei=stderror, mods = ~ outliers, method="FE", data=dat.comp, digits=3)

##Eggers regression test (significance indicates asymmetry)
regtest(model.re.erfa.intra, model = "lm")

##Beggs rank-correlation test
ranktest(model.re.erfa.intra)

# contour-enhanced funnel plot centered at 0 (see Peters et al., 2008)
funnel(model.re.erfa.intra, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),
       refline=0, atransf=exp, at=log(c(.10, .25, .5, 1, 2, 4, 10)))

viz_funnel(model.re.erfa.intra, method = "REML",
           x_trans_function = exp, xlab = "Within Standardized Mean Difference with small N correction",
           contours = TRUE, sig_contours = FALSE, contours_col = "Blues",
           egger = TRUE, trim_and_fill = TRUE)

# trim and fill
taf.erfa.intra <- trimfill(model.re.erfa.intra)
funnel(taf.erfa.intra)

#sunset plot for power
viz_sunset(model.re.erfa.intra,
           xlab = "Standardized Mean Difference (SMCR)", x_breaks = c(-0.5, 0, 0.5, 1, 2),
           contours = TRUE,
           power_contours =  "continuous")


#calculate individual power
s_power <- function(se, true_effect, sig_level) {

  (1 - stats::pnorm(stats::qnorm(1 - sig_level/2) *
                      se, abs(true_effect), se)) +
    stats::pnorm(stats::qnorm(sig_level/2) *
                   se, abs(true_effect), se)
}

funnel.re.erfa.intra <- funnel(model.re.erfa.intra, main="Standard Error")

df.erfa.intra$power <- s_power(se = funnel.re.erfa.intra$y,
                               true_effect = 0.8971,  #insert overall effect from model
                               sig_level = 0.05)
df.erfa.intra$power <- ifelse(round(df.erfa.intra$power, digits = 2) == 1, 0.99, df.erfa.intra$power)

# make forrest plot
# extract study weights
weights.model.re.erfa.intra <- cbind(paste0(formatC(weights(model.re.erfa.intra), format="f", digits=1, width=4), "%"))
df.erfa.intra$weight <- weights.model.re.erfa.intra

forest.rma(model.re.erfa.intra,
           xlim=c(-3,11.5), ylim=c(-0.5, 12),
           header= c("Programm", "Effektgröße [95% CI]"),
           xlab = "Standardized Mean Difference (SMCR)",
           slab = c("ID1","ID20","ID3","ID4", "ID10", "ID5", "ID8", "ID42", "ID13"),
           at=c(-2, 0, 2, 4, 6), digits=c(2L),
           ilab= cbind(df.erfa.intra$N_intra, round(df.erfa.intra$power, digits = 2), df.erfa.intra$weight),
           ilab.xpos = c(5.8, 6.8, 8),
           cex = 0.7,
           order = erfa_faeh_intra_statistics$order,
           #order = "obs",
           addfit = F,
)


abline(h=0.5)
addpoly(model.re.erfa.intra, row=0, cex = 0.7)
text(5.8, 11, "N", cex = 0.7, font = 2)
text(6.8, 11, "Power", cex = 0.7, font = 2)
text(8, 11, "Gewicht", cex = 0.7, font = 2)
segments(coef(model.re.erfa.intra), -0.1, coef(model.re.erfa.intra), (model.re.erfa.intra$k)+0.5, lty="dotted")


### add text with Q-value, dfs, p-value, and I^2 statistic
text(4, 0, pos=4, cex=0.7, bquote(paste("(Q = ",
                                        .(formatC(model.re.erfa.intra$QE, digits=2, format="f")), ", df = ", .(model.re.erfa.intra$k - model.re.erfa.intra$p),
                                        ", p = ", .(formatC(model.re.erfa.intra$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                        .(formatC(model.re.erfa.intra$I2, digits=1, format="f")), "%)")))



################################
##### Moderation Analysis ######
################################


#perform overall model with seperate heterogeneity estimation in each subgroup
#model.re.wiss.intra.het <- rma.mv(yi, vi, mods = ~  Type-1, random = ~ Type | trial, struct="DIAG", data=df.wiss.intra, digits=3, method = "REML")

#pairwise comparison
#anova(model.re.wiss.intra.het, L=rbind(c(-1,1)))

openxlsx::write.xlsx(print(inf.wiss.intra), "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Auswertung - Wirkung & Entwicklung/Daten/Ergebnisse_aggregiert_neu_Vielfalt/meta_analyse/outlier_wiss.xlsx")
