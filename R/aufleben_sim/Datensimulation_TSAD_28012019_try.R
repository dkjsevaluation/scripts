##Simulating Data##
#clear objects in r environment
rm(list = ls())

#######################################################################
##schulen mit weniger mobbing und höherer Erkennungsrate s1 s2 s3 s4###
#######################################################################
##Aggressionsdaten Selbst- und Fremdeinschätzung##
####create dataset from correlation matrix
mu <- c(0.15, 0.15, 0.2, 0.2, 0.1, 0.1, 0.25, 0.25, 3.1, 2.0)
stddev <- c(0.02, 0.02, 0.05, 0.05, 0.02, 0.02, 0.05, 0.05, 0.05, 0.05)

corMat <- matrix(c(1	,0.5	,0.5	,0.2	,0.7	,0.3	,0.2	,0.2, -0.3, 0.3, 
                   0.5	,1	,0.2	,0.2	,0.3	,0.7	,0.2	,0.2, 0.1, 0.1,
                   0.5	,0.2	,1	,0.3	,0.2	,0.2	,0.7	,0.3, -0.3, 0.3,
                   0.2	,0.2	,0.3	,1	,0.2	,0.2	,0.3	,0.7, 0.1, 0.1,
                   0.7	,0.3	,0.2	,0.2	,1	,0.3	,0.3	,0.2, 0.1, 0.3,
                   0.3	,0.7	,0.2	,0.2	,0.3	,1	,0.2	,0.2, 0.1, 0.1,
                   0.2	,0.2	,0.7	,0.3	,0.3	,0.2	,1	,0.3, 0.1, 0.1,
                   0.2	,0.2	,0.3	,0.7	,0.2	,0.2	,0.3,	1, 0.1, 0.1,
                   -0.3 ,0.1  ,-0.3 ,0.1  ,0.1  ,0.1  ,0.1 ,0.1 ,1, 0.1,
                   0.3 ,0.1  ,0.3 ,0.1  ,0.3  ,0.1  ,0.1 ,0.1 , 0.1, 1),
                 ncol = 10)

covMat <- stddev %*% t(stddev) * corMat
covMat

library(MASS)
set.seed(123)
agminus <- mvrnorm(n = 1500, mu = mu, Sigma = covMat, empirical = TRUE)
colMeans(agminus)
cor(agminus)

AGminus<- data.frame(agminus)
names(AGminus) <- c("cb", "cv", "tb", "tv", "Tcb", "Tcv", "Ttb", "Ttv", "emoreg", "mnutz")

###Soziodemografika erstsellen und an AGminus binden
AGminus$sex <- c(rbinom(1500, 1, 0.5))
#check result
table(AGminus$sex)

AGminus$migr <- c(rbinom(1500, 1, 0.3))
#check result
table(Agminus$migr)

####ID Variable erstellen und an dataframe binden
AGminus$ID <- c(1:1500)

####Alter und Klassenstufe erstellen und sortiert zusammenbinden
age <- round(runif(1500,12.5,18.4))
age.dat <- data.frame(age)

age.sorted <- age.dat[order(age.dat$age) , ]
age.klst <- data.frame(age.sorted)

age.klst$klst <- rep(8:12, each = 300)

#age und klst an AGminus nach emoreg sortiert binden

AGminus.sorted <- AGminus[order(AGminus$emoreg) , ]
AGminus.sorted$age <- c(age.klst$age)
AGminus.sorted$klst <- c(age.klst$klst)

###Klasse hinzufügen

AGminus.sorted$kl <- rep(1:60, each = 25)

####Lehrkräftevariablen erstellen und an AGminus.sorted binden

diaacty <- rnorm(60, 3, 0.05)
AGminus.teacher <- data.frame(diaacty)
AGminus.teacher$emostroop <- rnorm(60, 2.5, 0.05)
AGminus.teacher$secbelief <- rnorm(60, 2, 0.05)

AGminus.sorted$Tdiaacty <- rep(AGminus.teacher$diaacty, each = 25)
AGminus.sorted$Temostroop <- rep(AGminus.teacher$emostroop, each = 25)
AGminus.sorted$Tsecbelief <- rep(AGminus.teacher$secbelief, each = 25)

####Schulvariable erstellen und an dataframe binden
set.seed(123)
AGminus.sorted$Schule <- round(runif(1500, 0.5, 4.4))

###################################################################
##schulen mit mehr mobbing und geringerer Erkennungsrate s5 s6 s7##
###################################################################
##Aggressionsdaten Selbst- und Fremdeinschätzung##
####create dataset from correlation matrix
mu2 <- c(0.3, 0.3, 0.6, 0.4, 0.4, 0.2, 0.8, 0.2, 1.8, 3.4)
stddev2 <- c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)

corMat2 <- matrix(c(1	,0.5	,0.5	,0.2	,0.1	,0.1	,0.1	,0.1, -0.5, 0.5,
                    0.5	,1	,0.2	,0.2	,0.1	,0.1	,0.1	,0.1, -0.1, 0.2,
                    0.5	,0.2	,1	,0.3	,0.1	,0.1	,0.1	,0.1, -0.5, 0.5,
                    0.2	,0.2	,0.3	,1	,0.1	,0.1	,0.1	,0.1, -0.1, 0.2,
                    0.1	,0.1	,0.1	,0.1	,1	,0.3	,0.7	,0.2, -0.1, 0.5,
                    0.1	,0.1	,0.1	,0.1	,0.3	,1	,0.2	,0.2, -0.1, 0.1,
                    0.1	,0.1	,0.1	,0.1	,0.7	,0.2	,1	,0.3, -0.1, 0.1,
                    0.1	,0.1	,0.1	,0.1	,0.2	,0.2	,0.3	,1, -0.1, 0.1,
                    -0.5,-0.1 ,-0.5 ,-0.1 ,-0.1 ,-0.1 ,-0.1 ,-0.1 ,1, 0.1,
                    0.5 ,0.2  ,0.5 ,0.2  ,0.5  ,0.1  ,0.1 ,0.1 , 0.1, 1),
                 ncol = 10)


covMat2 <- stddev2 %*% t(stddev2) * corMat2
covMat2

##wenn die korrelationsmatrix die stichprobe oder eine manifeste ebene repräsentiert
library(MASS)
set.seed(123)
agplus <- mvrnorm(n = 1125, mu = mu2, Sigma = covMat2, empirical = TRUE)
colMeans(agplus)
cor(agplus)


AGplus<- data.frame(agplus)
names(AGplus) <- c("cb", "cv", "tb", "tv", "Tcb", "Tcv", "Ttb", "Ttv", "emoreg", "mnutz")

###Soziodemografika erstsellen und an AGplus binden
AGplus$sex <- c(rbinom(1125, 1, 0.5))
#check result
table(AGplus$sex)

AGplus$migr <- c(rbinom(1125, 1, 0.3))
#check result
table(AGplus$migr)

####ID Variable erstellen und an dataframe binden
AGplus$ID <- c(1501:2625)

####Alter und Klassenstufe erstellen und sortiert zusammenbinden
age2 <- round(runif(1125,12.5,18.4))
age.dat2 <- data.frame(age2)

age.sorted2 <- age.dat2[order(age.dat2$age2) , ]
age.klst2 <- data.frame(age.sorted2)

age.klst2$klst <- rep(8:12, each = 225)

#age und klst an AGplus nach emoreg sortiert binden

AGplus.sorted <- AGminus[order(AGplus$emoreg) , ]
AGplus.sorted$age <- c(age.klst2$age)
AGplus.sorted$klst <- c(age.klst2$klst)

###Klasse hinzufügen

AGplus.sorted$kl <- rep(61:105, each = 25)

####Lehrkräftevariablen erstellen und an AGminus.sorted binden

diaacty <- rnorm(45, 1, 0.05)
AGplus.teacher <- data.frame(diaacty)
AGplus.teacher$emostroop <- rnorm(45, 1.5, 0.05)
AGplus.teacher$secbelief <- rnorm(45, 2, 0.05)

AGplus.sorted$Tdiaacty <- rep(AGplus.teacher$diaacty, each = 25)
AGplus.sorted$Temostroop <- rep(AGplus.teacher$emostroop, each = 25)
AGplus.sorted$Tsecbelief <- rep(AGplus.teacher$secbelief, each = 25)

####Schulvariable erstellen und an dataframe binden
set.seed(123)
AGplus.sorted$Schule <- round(runif(1125, 4.5, 6.4))

##################################
###AGplus und AGminus verbinden###
##################################
library(dplyr)

tsad.sus <- bind_rows(AGplus.sorted, AGminus.sorted)

#######################
###Daten exportieren###
#######################

library(foreign)
write.foreign(tsad.sus, "C:/Users/User/Desktop/tsad.txt", "C:/Users/User/Desktop/tsad.sps",   package="SPSS")

###########
#  __     #
#<(o )___ #
# ( ._> / #
#  `---'  # 
###########
