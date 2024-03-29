## auf altem rechner ausf�hren: liste mit r paketen
installed <- as.data.frame(installed.packages())
write.csv(installed, 'installed_previously.csv')

## auf neuem rechner ausf�hren: pakete der liste installieren
installedPreviously <- read.csv('installed_previously.csv') 
baseR <- as.data.frame(installed.packages()) 
toInstall <- setdiff(installedPreviously, baseR)
install.packages(toInstall$X) 
