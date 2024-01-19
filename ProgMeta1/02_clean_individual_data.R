### require packages
library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
library(patchwork)

### get imported dfs from sharepoint into a list of dfs

### handle duplicates and accidental data rows in each df

## delete empty rows
## delete rows with no CODE
## check for duplicates by CODE
## delete CODE duplicates with highest NA
## delete unnecessary system variables?

### recode values e.g. NA?

### calculate new variables (M, rel) for each df in list

## for each contruct
# df$SK.FAEH.C2 <- df %>% select(contains("FAEH") & contains("C2")) %>% rowMeans(na.rm = FALSE)
# df$SK.FAEH.C2.rel <- df %>% select(contains("FAEH") & contains("C2"), -contains("SK")) %>% psych::alpha() %>% {. ->> tmp} %>% '[['(1) %>% select(std.alpha) %>% pull(std.alpha) %>% .[1]

### rename variables for easy reading?
