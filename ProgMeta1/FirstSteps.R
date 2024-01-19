library(dplyr)
library(psych)
library(splithalfr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(stringr)
library(dkjslama)

###########################
############ Get Data #####
###########################

###create empty list for fbdata
fblist <- list()

### get vector with fbnames
fbnames <- c("Evaluation_Schulerfolgdigital_retro", "Evaluation_JbK_vorher", "Evaluation_JbK_nachher", "TechnovationGirls01", "TechnovationGirls02")

## get Lamapoll data for every fbname and put it into list fbdata with fbname as name of df
for (i in fbnames) {
  df <- as.data.frame(get.lamapoll.data(i, "we@dkjs.de", "wGl5v4fz"))
  df <- df %>%
    mutate(across(where(is.integer), as.numeric))
  fblist[[i]] <- df
}


#### to do: make a df, in which the variable names and variable descriptions (full questions) are matched for each df
## check whether API can do that and implement into package {lamapoll} as function


##################################
############ Variable selection ##
##################################

### use only numeric variables
# done: CODE muss drin bleiben
fblist_num <- list()
for (j in 1:length(fblist)) {
  df_num <- fblist[[j]] %>% select(where(is.numeric) | matches("CODE"))
  fblist_num[[length(fblist_num) + 1]] <- df_num

  print(paste("Number of variables in df", j, ": ", ncol(fblist[[j]])))
  print(paste("Number of variables in df_num", j, ": ", ncol(df_num)))
}
names(fblist_num) <- names(fblist)


##################################
############ Scale Building ######
##################################

#### this requires building a filter system that applies different ways of calculating scales and reliability
#### depending on the first two prefixes in the variable name as well as the "_s1" part of the variable name.
#### lets build modules based on Tabelle Variablenbenennung!
#### in the end/at the beginning, loop through all modules for every df in fblist_num

#create list to fill in
fblist_num2 <- vector("list", length = length(fblist_num))

#### Module 1 for WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM; Module 2 for ERWA; ...TN .... ZUFR

############### Module 1

###########################
#### Sort Items into scales
#### done: extend for ".KJ" at the end of a variable name
#### done: check for different "_s0", which should be identical for all items in a scale
#### to do: build different scales for ROLLE?

  #create list to fill in
  fblist_num2 <- vector("list", length = length(fblist_num))

  # loop over the list with fb data
  for (k in 1:length(fblist_num)) {
    # Extract the part of the variable names without "_i1" etc.
    variable_names <- str_replace(names(fblist_num[[k]]), "_i\\d+", "")
    variable_names <- str_replace(variable_names, "_\\.", ".")

    # Get only the unique values
    unique_variable_names <- unique(variable_names)

    # extract variable names for scales
    scalevars <- unique_variable_names[grep("^(WISS|HAND|FAEH|ERFA|NETZ|TEIL|AFFM)_", unique_variable_names)]

    # Create empty list with names from scalevars
    scalevar_dfs <- vector("list", length = length(scalevars))
    names(scalevar_dfs) <- scalevars

    # Loop over each element in scalevars
    for (i in 1:length(scalevars)) {
      scalevarname <- scalevars[i]
      scalevarprefix <- sub("_.*", "", scalevarname)
      scalevarsuffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)

      matching_vars <- grep(paste0("^", scalevarprefix, ".*_", scalevarsuffix, "$"), names(fblist_num[[k]]), value = TRUE)

      if (length(matching_vars) > 0) {
        scalevar_dfs[[i]] <- fblist_num[[k]][, matching_vars, drop = FALSE]

        # extract suffix from scalevarname
        suffix <- sub(".*?_.*?_(.*)", "\\1", scalevarname)

        # filter variables in the current scalevar_dfs
        scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^.*_", suffix, "$"), names(scalevar_dfs[[i]])), drop = FALSE]

        # extract prefix from scalevarname
        prefix <- sub("(.*?_.*?_).*", "\\1", scalevarname)

        # filter variables in the current scalevar_dfs to keep only variables matching the prefix
        scalevar_dfs[[i]] <- scalevar_dfs[[i]][, grepl(paste0("^", prefix, ".*"), names(scalevar_dfs[[i]])), drop = FALSE]
      }
    }

    # Select only those list elements that are data frames with at least two columns
    #scalevar_dfs2 <- scalevar_dfs[sapply(scalevar_dfs, is.list)]   # retired
    scalevar_dfs <- scalevar_dfs[sapply(scalevar_dfs, function(x) ncol(x) >= 2)]


    # copy to prepared list fblist_num2
    fblist_num2[[k]] <- scalevar_dfs
    # Assign names to the list elements
    names(fblist_num2)[k] <- paste0(names(fblist_num[k]))
  }



########################################################################
#### calculate row means for every set of items with two or more items
#### to do: attach directly to initial df (last loop step)
#### done: error handling for psych::alpha(), put NA into .REL and go to next df

# loop through each data frame in the list

  # check if the data frame has more than two columns
  if (ncol(scalevar_dfs[[i]]) > 2) {

    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))

    #### to do: an if clause to only do the following calculations for item bundles based on similar "_s1" etc

    # calculate the row means for the data frame and add as new column
    scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      mutate(!!paste0("SK.", names(scalevar_dfs)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))

    # calculate reliability and attach as new column
    alpha <- tryCatch(
      psych::alpha(scalevar_dfs[[i]], check.keys = TRUE),
      error = function(e) NULL
    )

    if (!is.null(alpha)) {
      scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
        mutate(!!paste0("REL.", names(scalevar_dfs)[i]) := alpha %>% {. ->> tmp} %>% '[['(1) %>% pull(std.alpha) %>% .[1])
    } else {
      scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
        mutate(!!paste0("REL.", names(scalevar_dfs)[i]) := NA)
      next # skip to next iteration of for loop
    }
  }
  # check if the data frame has two columns
  if (ncol(scalevar_dfs[[i]]) < 3) {

    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
    scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      mutate_all(function(x) ifelse(is.nan(x), NA, x))

    # calculate the row means for the data frame and add as new column
    scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      mutate(!!paste0("SK.", names(scalevar_dfs)[i]) := rowMeans(.[, 1:ncol(.)], na.rm = TRUE))

    # calculate reliability and attach as new column
    scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
      mutate(!!paste0("REL.", names(scalevar_dfs)[i]) := spearman_brown(na.omit(.[,1]), na.omit(.[,2])))
  }
}

    # Select only those list elements that are data frames with at least two columns
    #scalevar_dfs2 <- scalevar_dfs[sapply(scalevar_dfs, is.list)]   # retired
    fblist_num2[[k]] <- scalevar_dfs[sapply(scalevar_dfs, function(x) ncol(x) >= 2)]


for (i in seq_along(scalevar_dfs)) {

    # replace NaN values with NA, because in some dfs there were still NaNs, just to make sure.
  scalevar_dfs[[i]] <- scalevar_dfs[[i]] %>%
    mutate_all(function(x) ifelse(is.nan(x), NA, x))
}

  # Assign names to the list elements
  names(fblist_num2)[k] <- paste0(names(fblist_num[k]))

  # Select only those list elements that are data frames with at least two columns
  fblist_num2[[k]] <- scalevar_dfs[[i]]

}


#### to do: one case of errorhandling
# Evaluation_Schulerfolgdigital_retro
#psych::alpha(scalevar_dfs[["WISS_inha5_s1_tr.C8"]])
#Number of categories should be increased  in order to count frequencies.
#Likely variables with missing values are  WISS_inha5_i10_s1_tr.C8 WISS_inha5_i16_s1_tr.C8
#Error in principal(x, scores = FALSE) :
#  I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue.
#Please drop those variables and try again.



#################################

############### Module 2









############################# Test Individual Data including REL graph!

df <- get.lamapoll.data("we@dkjs.de", "Evaluation_Schulerfolgdigital_retro", "wGl5v4fz")

#### Mittelwerte für Personen als Variable erstellen, Reliabilität für Stichprobe berechnen und in Variable packen (um später Grafiken damit einzufärben)
df$SK.FAEH.C2 <- df %>% select(contains("FAEH") & contains("C2")) %>% rowMeans(na.rm = FALSE)
df$SK.FAEH.C2.rel <- df %>% select(contains("FAEH") & contains("C2"), -contains("SK")) %>% psych::alpha() %>% {. ->> tmp} %>% '[['(1) %>% select(std.alpha) %>% pull(std.alpha) %>% .[1]

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

######
# change transparency of color gradient legend by adding two numbers after the hex, e.g. #FFFFFF wird zu #FFFFFF99
# scale_fill_gradientn(colors = c("#CC0033", "#3300CC"), labels=c("Minimum",0.6,"Maximum"), breaks=c(0,0.6,1), limits=c(0,1), guide = guide_legend(override.aes = list(alpha = 0.4))) +

























####################### First Trys




### get vector with varnames based on stem
varnames <- c("WISS_meth", "WISS", "NETZ", "XYZ.")
# varnames <- c("WISS_meth", "WISS", "NETZ", "XYZ.")

### continue here to do: calculate variable for reliability (s.u.)
## to do: separate for t0 and t1
for (j in 1:length(fblist_num)) {
  fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames){
    if (any(grepl(print(k), names(fblist_num[[j]]))) == TRUE) {
      fblist_num[[j]] <- fblist_num[[j]] %>% mutate(!!paste0("SK.", k) := rowMeans(select(.,contains(!!paste0(k))),na.rm = TRUE))
    }
  }
  fblist_num[[j]] <-  fblist_num[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}


###############
for (j in 1:length(fblist_num)) {
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames) {
    if (any(grepl(k, names(fblist_num[[j]])))) {
      # Check for variable with suffix _t0
      if (any(grepl(paste0(k, "_t0"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t0") := rowMeans(select(., contains(k, "_t0")), na.rm = TRUE))
      }
      # Check for variable with suffix _t1
      if (any(grepl(paste0(k, "_t1"), names(fblist_num[[j]])))) {
        fblist_num[[j]] <- fblist_num[[j]] %>%
          mutate(!!paste0("SK.", k, "_t1") := rowMeans(select(., contains(k, "_t1")), na.rm = TRUE))
      }
      # Create composite variable for base variable
      fblist_num[[j]] <- fblist_num[[j]] %>%
        mutate(!!paste0("SK.", k) := rowMeans(select(., contains(k)), na.rm = TRUE))
    }
  }
  fblist_num[[j]] <- fblist_num[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}



### loop through varnames in every df in list fblist and calculate mean for every person
### continue here to do: calculate variable for reliability (s.u.)
### and fix extension to list of df: 2. delete empty SK. variables - HANDBEGA is still being inserted even when no var in df, maybe if at all HAND then TRUE?
for (j in 1:length(fblist)) {
  fblist[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames){
    if (any(grepl(print(k), names(df))) == TRUE) {
      fblist[[j]] <- fblist[[j]] %>% mutate(!!paste0("SK.", k) := rowMeans(select(.,contains(!!paste(k))),na.rm = TRUE))
    }
  }
  fblist[[j]] <-  fblist[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}












#########################################################################
############################## loop through variable names ##############
#########################################################################

###create empty list for fbdata
fblist <- list()

### get vector with fbnames
fbnames <- c("Evaluation_JbK_vorher", "Evaluation_Schulerfolgdigital_retro")

### get vector with varnames
varnames <- c("WISS_meth", "WISS", "NETZ", "XYZ.")


## get Lamapoll data for every fbname and put it into list fbdata with fbname as name of df
for (i in fbnames) {
  df <- as.data.frame(get.lamapoll.data("we@dkjs.de", i, "wGl5v4fz"))
  df <- df %>%
    mutate(across(where(is.integer), as.numeric))
  fblist[[i]] <- df
}



##### TRYING OUT dealing with "_s"
############# NOT WORKING,  SKIP THIS, CODE ABOVE WORKS
library(dplyr)

# Function that detects unique patterns in a data frame
detect_patterns <- function(df) {
  varnames <- colnames(df)
  patterns <- unique(sub(".*_s(\\d+)$", "\\1", varnames))
  return(patterns)
}

# Loop through all data frames in the list
for (i in seq_along(scalevar_dfs)) {
  df <- scalevar_dfs[[i]]
  patterns <- detect_patterns(df)

  # If there's only one pattern, attach it to the name of the data frame
  if (length(patterns) == 1) {
    new_name <- paste0(names(scalevar_dfs)[i], "_s", patterns)
    scalevar_dfs[[new_name]] <- df
    scalevar_dfs[[i]] <- NULL

    # If there's more than one pattern, split the data frame into sub-data frames
  } else if (length(patterns) > 1) {
    for (j in seq_along(patterns)) {
      new_name <- paste0(names(scalevar_dfs)[i], "_s", patterns[j])
      df_sub <- df %>% select(matches(paste0("_s", patterns[j], "$")))
      scalevar_dfs[[new_name]] <- df_sub
    }
    scalevar_dfs[[i]] <- NULL
  }
}


#rm(df)

### loop through varnames in every df in list fblist and calculate mean for every person
#for (j in 1:length(fblist)) {
#  for (k in varnames){
#    name <- paste(k, sep = "")
#    fblist[[j]]$a <- fblist[[j]] %>% select(contains(paste0(k))) %>% rowMeans(na.rm = TRUE)
#  }
#}

#for (j in 1:length(fblist)) {
#  for (k in varnames){
#    attach(j)
#    if (exists(print(k) == TRUE)) {
#      fblist[[j]] %>% select(contains(paste0(k))) %>% mutate(paste0(k) = rowMeans(na.rm = TRUE))
#    }
#  }
#}

#for (k in varnames){
#  if (exists(print(k) == TRUE)) {
#    df$ <- df %>% select(contains(paste(k))) %>% rowMeans(na.rm = TRUE))
#  }
#}


#if (any(grepl(print(varnames[1]), names(df))) == TRUE) {
#  name <- paste(k)
#}

#for (k in varnames){
#if (any(grepl(print(k), names(df))) == TRUE) {print(k)}
#}

### continue here and to do: calculate variable for reliability (s.u.) and extend to list of df
#df <- df %>%
#  mutate(across(where(is.integer), as.numeric))
#for (k in varnames){
#  if (any(grepl(print(k), names(df))) == TRUE) {
#    df <- df %>% mutate(!!paste0("SK.", k) := rowMeans(select(.,contains(!!paste(k))),na.rm = TRUE))
#    }
#}
#df <- df %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))

### loop through varnames in every df in list fblist and calculate mean for every person
### continue here to do: calculate variable for reliability (s.u.)
### and fix extension to list of df: 2. delete empty SK. variables - HANDBEGA is still being inserted even when no var in df, maybe if at all HAND then TRUE?
for (j in 1:length(fblist)) {
  fblist[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames){
    if (any(grepl(print(k), names(df))) == TRUE) {
      fblist[[j]] <- fblist[[j]] %>% mutate(!!paste0("SK.", k) := rowMeans(select(.,contains(!!paste(k))),na.rm = TRUE))
    }
  }
  fblist[[j]] <-  fblist[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
 }


##
fblist[[j]] <- fblist[[j]] %>% select(c((starts_with("SK.")), is.na(.)))

fblist[[j]] <- fblist[[j]] %>% select(.,c(any_of(contains("HAND"))))

delNA <- function() {c(any_of(c((!starts_with("SK.")), is.na(.))))}

fblist[[j]] <- fblist[[j]] %>% filter_if(~!(starts_with("SK.") & all(is.na(.))))

fblist2 <- fblist

for (j in 1:length(fblist2)) {
fblist2[[j]] <-  fblist2[[j]] %>% select_if(function(x) !all(is.na(x)))
}
#### continue


## Mittelwerte für Personen als Variable erstellen, Reliabilität für Stichprobe berechnen und in Variable packen (um später Grafiken damit einzufärben)
df$SK.FAEH.C2 <- df %>% select(contains("FAEH") & contains("C2")) %>% rowMeans(na.rm = FALSE)
df$SK.FAEH.C2.rel <- df %>% select(contains("FAEH") & contains("C2"), -contains("SK")) %>% psych::alpha() %>% {. ->> tmp} %>% '[['(1) %>% select(std.alpha) %>% pull(std.alpha) %>% .[1]



############# Quick Calculation for Stiftungsbericht #########
### use only numeric variables
fblist_num <- list()
for (j in 1:length(fblist)) {
  df_num <- fblist[[j]] %>% select(where(is.numeric))
  fblist_num[[length(fblist_num) + 1]] <- df_num
}
names(fblist_num) <- names(fblist)

### continue here to do: calculate variable for reliability (s.u.)
### and fix that it only calculates a scale if the name in varnames is present in all df
for (j in 1:length(fblist_num)) {
  fblist_num[[j]] %>% mutate(across(where(is.integer), as.numeric))
  for (k in varnames){
    if (any(grepl(print(k), names(df))) == TRUE) {
      fblist_num[[j]] <- fblist_num[[j]] %>% mutate(!!paste0("SK.", k) := rowMeans(select(.,contains(!!paste(k))),na.rm = TRUE))
    }
  }
  fblist_num[[j]] <-  fblist_num[[j]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
}


#fblist_num[[1]] <- fblist_num[[1]] %>% mutate(!!paste0("SK.", "NETZ") := rowMeans(select(.,contains(!!paste("NETZ"))),na.rm = TRUE))
#fblist_num[[1]] <-  fblist_num[[1]] %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))

df_new <-


###############################################################################################
######
### transform by function
transform_var <- function(x) {
  p_l <- rlang::exprs(
    .data[[x]] == 'between 0 and 1' ~ 1,
    .data[[x]] == 'between 1 and 2' ~ 2)
  return(p_l)
}

dat <- dat %>%
  mutate(var =
           pull(mutate(dat, case_when(!!!transform_var('varname_in_df')))))



###### RESTE #############################################################################
test <- fblist[[1]] %>% select(where(is.numeric))
testliste <- vector("list", length(fblist))

fblist_num <- list()
  for (j in 1:length(fblist)) {
    df_num <- fblist[[j]] %>% select(where(is.numeric))
    fblist_num[[length(fblist_num) + 1]] <- df_num
                              }
names(fblist_num) <- names(fblist)


ggplot(data = df.1_long, aes(x = variables, y = values)) +
  scale_x_discrete(limits = rev(names(df.1[,1:9]))) +
  #scale_y_continuous(limits = c(0,3), breaks = seq(0,3,1)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(height = 0.1, alpha = 0.3) +
  stat_boxplot(geom = 'errorbar', width = 0.25) +
  stat_summary(fun = "mean", color = "gray", geom = "line", aes(group = 1)) +
  stat_summary(fun = "mean", color = "red", shape = "|") +
  coord_flip() +
  theme_light() +
  labs(title = "Title", y = "Value", x = "Methods") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())


library(ggplot2)
dat <- data.frame(ID = rnorm(10), P1 = rnorm(10), P2 = rnorm(10), P3 = rnorm(10), P4 = rnorm(10), P5 = rnorm(10))
dat.m <- reshape::melt(dat, id.vars = 1)
#Create faceting variable
dat.m <- transform(dat.m, facet = ifelse(variable %in% c("P1", "P2", "P3"), 1, ifelse(variable == "P4", 2, 3)))

ggplot(dat.m, aes(ID, value)) + geom_point() + facet_wrap(~facet)

a <- colMeans(df.1[,1:8], na.rm = T)

 labs(x="Education", y="Salary in US Dollars")
  geom_line(data = df_mean,
            mapping = aes(x = grp, y = average))
  facet_grid()

boxplot(df.1[,1])

who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)
