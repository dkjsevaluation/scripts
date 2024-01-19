
#Datensimulation AUF!leben Einstellung Datenanalyse
## Projekt 1
#### Datensatz per Kennwerte und Korrelationsmatrix simulieren
mu1 <- c(3.8, 0.8,  3.0, 3.1, 2.9, 3.0, 2.8, 2.8, 2.9, 1.4, 1.4, 1.6)
sd1 <- c(0.3, 0.4, 0.3, 0.3, 0.35, 0.35, 0.35, 0.35, 0.3525, 0.15, 0.15, 0.15)

corMat1 <- matrix(c(1	,0	,0.1	,0.1	,0.1	,0.1	,0.1	,0.1	,0.1	,0.5	,0.6	,0.5
                   ,0	,1	,0.4	,0.4	,0.4	,0.5	,0.1	,0.1	,0.1	,0.1	,0.1	,0.1
                   ,0.1	,0.4	,1	,0.4	,0.5	,0.4	,0.2	,0.2	,0.2	,0.4	,0.4	,0.4
                   ,0.1	,0.4	,0.4	,1	,0.9	,0.8	,0.3	,0.3	,0.3	,0.7	,0.7	,0.7
                   ,0.1	,0.4	,0.5	,0.9	,1	,0.8	,0.3	,0.3	,0.3	,0.7	,0.7	,0.7
                   ,0.1	,0.5	,0.4	,0.8	,0.8	,1	,0.3	,0.3	,0.3	,0.7	,0.7	,0.7
                   ,0.1	,0.1	,0.2	,0.3	,0.3	,0.3	,1	,0.8	,0.9	,0.3	,0.3	,0.3
                   ,0.1	,0.1	,0.2	,0.3	,0.3	,0.3	,0.8	,1	,0.9	,0.3	,0.3	,0.3
                   ,0.1	,0.1	,0.2	,0.3	,0.3	,0.3	,0.9	,0.9	,1	,0.3	,0.3	,0.3
                   ,0.5	,0.1	,0.4	,0.7	,0.7	,0.7	,0.3	,0.3	,0.3	,1	,0.7	,0.8
                   ,0.6	,0.1	,0.4	,0.7	,0.7	,0.7	,0.3	,0.3	,0.3	,0.7	,1	,0.7
                   ,0.5	,0.1	,0.4	,0.7	,0.7	,0.7	,0.3	,0.3	,0.3	,0.8	,0.7	,1
),
                 ncol = 12)

covMat1 <- sd1 %*% t(sd1) * corMat1
covMat1

library(lqmm)
covMat1 <- make.positive.definite(covMat1, tol = 0.003)

library(tmvtnorm)
set.seed(12345)
df <- as.data.frame(round(rtmvnorm(13, mean = mu1, sigma = covMat1, lower = rep(0, 12), upper = rep(5, 12)), digits = 0))
names(df) <- c("alter",	"geschlecht",	"zufriedenheit",	"psych1",	"psych2",	"psych3",	"phys1",	"phys2",	"phys3",	"selbst1",	"selbst2",	"selbst3")

df[,c(4:12)] <- df[,c(4:12)]-2
df$projekt <- rep(1, 13)
df$förderkategorie <- rep("ALLTAG PLUS", 13)
df$kontext <- rep("kita", 13)

summary(df)

#########################
## Projekt 2
#### Datensatz per Kennwerte und Korrelationsmatrix simulieren
mu2 <- c(2, 0.8,  3.2, 3.2, 2.8, 3.2, 2.1, 1.9, 1.9, 3.2, 3.4, 3.3)
sd2 <- c(1.3, 0.4, 1.2, 0.35, 0.35, 0.3, 0.3, 0.35, 0.3, 0.45, 0.45, 0.5)

corMat2 <- matrix(c(1	,0	,0.1	,0.1	,0.1	,0.1	,0.2	,0.2	,0.2	,0.5	,0.5	,0.5
                   ,0	,1	,0.4	,0.4	,0.4	,0.4	,0.1	,0.1	,0.1	,0.7	,0.7	,0.7
                   ,0.1	,0.4	,1	,0.4	,0.5	,0.4	,0.4	,0.5	,0.4	,0.4	,0.4	,0.4
                   ,0.1	,0.4	,0.4	,1	,0.9	,0.9	,0.6	,0.6	,0.6	,0.5	,0.5	,0.5
                   ,0.1	,0.4	,0.5	,0.9	,1	,0.9	,0.6	,0.6	,0.6	,0.5	,0.5	,0.5
                   ,0.1	,0.4	,0.4	,0.9	,0.9	,1	,0.5	,0.6	,0.6	,0.5	,0.5	,0.5
                   ,0.2	,0.1	,0.4	,0.6	,0.6	,0.5	,1	,0.8	,0.8	,0.2	,0.2	,0.2
                   ,0.2	,0.1	,0.5	,0.6	,0.6	,0.6	,0.8	,1	,0.8	,0.2	,0.2	,0.2
                   ,0.2	,0.1	,0.4	,0.6	,0.6	,0.6	,0.8	,0.8	,1	,0.2	,0.2	,0.2
                   ,0.5	,0.7	,0.4	,0.5	,0.5	,0.5	,0.2	,0.2	,0.2	,1	,0.7	,0.7
                   ,0.5	,0.7	,0.4	,0.5	,0.5	,0.5	,0.2	,0.2	,0.2	,0.7	,1	,0.7
                   ,0.5	,0.7	,0.4	,0.5	,0.5	,0.5	,0.2	,0.2	,0.2	,0.7	,0.7	,1
                   
),
ncol = 12)

covMat2 <- sd2 %*% t(sd2) * corMat2
covMat2

library(lqmm)
covMat2 <- make.positive.definite(covMat2, tol = 0.003)

library(tmvtnorm)
set.seed(12345)
df2 <- as.data.frame(round(rtmvnorm(26, mean = mu2, sigma = covMat2, lower = rep(0, 12), upper = rep(5, 12)), digits = 0))
names(df2) <- c("alter",	"geschlecht",	"zufriedenheit",	"psych1",	"psych2",	"psych3",	"phys1",	"phys2",	"phys3",	"emsoz1",	"emsoz2",	"emsoz3")

df2$alter <- df2$alter+10
df2[,c(4:12)] <- df2[,c(4:12)]-2
df2$projekt <- rep(2, 26)
df2$förderkategorie <- rep("ALLTAG PLUS", 26)
df2$kontext <- rep("schule", 26)

summary(df2)

#########################
## Projekt 3
#### Datensatz per Kennwerte und Korrelationsmatrix simulieren
mu3 <- c(2, 0.8,  3.2, 3.2, 2.8, 3.2, 2.1, 1.9, 1.9, 3.2, 3.4, 3.3)
sd3 <- c(1.3, 0.4, 1.2, 0.35, 0.35, 0.3, 0.3, 0.35, 0.3, 0.45, 0.45, 0.5)

corMat3 <- matrix(c(1	,0	,0.1	,0.1	,0.1	,0.1	,0.3	,0.3	,0.3	,0.5	,0.5	,0.5
                    ,0	,1	,0.4	,0.4	,0.4	,0.4	,0.7	,0.7	,0.7	,0.1	,0.1	,0.1
                    ,0.1	,0.4	,1	,0.4	,0.5	,0.4	,0.4	,0.5	,0.4	,0.4	,0.4	,0.4
                    ,0.1	,0.4	,0.4	,1	,0.9	,0.9	,0.5	,0.5	,0.5	,0.7	,0.7	,0.7
                    ,0.1	,0.4	,0.5	,0.9	,1	,0.9	,0.5	,0.5	,0.5	,0.7	,0.7	,0.7
                    ,0.1	,0.4	,0.4	,0.9	,0.9	,1	,0.5	,0.5	,0.5	,0.7	,0.7	,0.7
                    ,0.3	,0.7	,0.4	,0.5	,0.5	,0.5	,1	,0.8	,0.8	,0.2	,0.2	,0.2
                    ,0.3	,0.7	,0.5	,0.5	,0.5	,0.5	,0.8	,1	,0.8	,0.2	,0.2	,0.2
                    ,0.3	,0.7	,0.4	,0.5	,0.5	,0.5	,0.8	,0.8	,1	,0.2	,0.2	,0.2
                    ,0.5	,0.1	,0.4	,0.7	,0.7	,0.7	,0.2	,0.2	,0.2	,1	,0.7	,0.7
                    ,0.5	,0.1	,0.4	,0.7	,0.7	,0.7	,0.2	,0.2	,0.2	,0.7	,1	,0.7
                    ,0.5	,0.1	,0.4	,0.7	,0.7	,0.7	,0.2	,0.2	,0.2	,0.7	,0.7	,1
                    
                   
                   
),
ncol = 12)

covMat3 <- sd3 %*% t(sd3) * corMat3
covMat3

library(lqmm)
covMat3 <- make.positive.definite(covMat3, tol = 0.003)

library(tmvtnorm)
set.seed(12345)
df3 <- as.data.frame(round(rtmvnorm(69, mean = mu3, sigma = covMat3, lower = rep(0, 12), upper = rep(5, 12)), digits = 0))
names(df3) <- c("alter",	"geschlecht",	"zufriedenheit",	"psych1",	"psych2",	"psych3",	"emsoz1",	"emsoz2",	"emsoz3", "alltag1",	"alltag2",	"alltag3")

df3$alter <- df3$alter+14
df3[,c(4:12)] <- df3[,c(4:12)]-2
df3$projekt <- rep(3, 69)
df3$förderkategorie <- rep("MENTORING", 69)
df3$kontext <- rep("kita", 69)

##in ausgewählten Variablen im Datensatz insgesamt 3% durch " " ersetzen
set.seed(420)
df3[,c(1:12)] <- as.data.frame(lapply(df3[,c(1:12)], function(x) {
  x[sample(seq_along(x), 0.03 * length(x))] <- " "
  x}))


summary(df3)


###exportieren
library(writexl)
write_xlsx(df, "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/aufleben_sim/df1.xlsx")
write_xlsx(df2, "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/aufleben_sim/df2.xlsx")
write_xlsx(df3, "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/aufleben_sim/df3.xlsx")

###importieren
library(readxl)
df <- read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/aufleben_sim/df1.xlsx")
df2 <- read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/aufleben_sim/df2.xlsx")
df3 <- read_xlsx("C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/aufleben_sim/df3.xlsx")

df3[1:13] <- lapply(df3[1:13], as.numeric)



####mergen
dfmerge <- dplyr::full_join(df,df2)
dfmerge <- dplyr::full_join(dfmerge, df3)

write_xlsx(dfmerge, "C:/Users/Alexander Wedel/OneDrive - Deutsche Kinder- und Jugendstiftung/Alex/R/aufleben_sim/dfmerge.xlsx")


###test
shapiro.test(dfmerge$sk_alltag)
hist(dfmerge$sk_selbst)


dfmerge$sk_emsoz <- rowMeans(cbind(dfmerge$emsoz1, dfmerge$emsoz2, dfmerge$emsoz3), na.rm = FALSE)
dfmerge$sk_psych <- rowMeans(cbind(dfmerge$psych1, dfmerge$psych2, dfmerge$psych3), na.rm = FALSE)
dfmerge$sk_phys <- rowMeans(cbind(dfmerge$phys1, dfmerge$phys2, dfmerge$phys3), na.rm = FALSE)
dfmerge$sk_selbst <- rowMeans(cbind(dfmerge$selbst1, dfmerge$selbst2, dfmerge$selbst3), na.rm = FALSE)
dfmerge$sk_alltag <- rowMeans(cbind(dfmerge$alltag1, dfmerge$alltag2, dfmerge$alltag3), na.rm = FALSE)

PerformanceAnalytics::chart.Correlation(dfmerge[,c(1:3, 22:25)], histogram = TRUE)
t.test(dfmerge$sk_phys~dfmerge$geschlecht)

#Standardisieren der Variablen nötig?
#df2 <- as.data.frame(scale(df))
