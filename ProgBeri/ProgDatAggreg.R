#### already programmed

## FirstSteps2
# get data, select and CODE ... possible replace with Stiftungsbericht_new2
# Stiftungsbericht_new2 does not operate on a list of dfs, but
# first produces matched dfs. I should probably do this and then 
# split the list of lists (dfs of programm IDs) to deploy the function for
# scale building of FirstSteps2 on these lists. Probably with a layered for loop.
# Or better yet: I split the list of lists into three lists with just dfs and the
# programme ID and then deploy the function without changing it!
# so....: first matching syntax from Stiftungsbericht_new2, then split into 3/4 lists
# (type of dfs: t0,t1,tr,matched), then apply scale building on these lists

# end result of FirstSteps2 is a list of lists of a df (programme name, scale name)
# including Items, mean and reliability

# It seems the scales have the same number of rows, so i could do.call(rbind) them into a df
# resulting in a list of dfs.
# this would enable me to match it again with other vars like role/gender etc.
# and basically recreate the original df, except for the added scale and reliability vars
# but maybe it's good to keep it in separate dfs for further graphics and tables etc.

## FirstSteps2_Aggregate (prepare data for meta-analysis of scale data)
# produces desclist3, a list of lists of dfs (programme, scale) with
# desriptives for the scales.
# then binds these into a single df (dfstage2).
# then attaches scale-meta-data for the meta-analysis
# then sorts the data into an aggregate list of contruct-scale-types (result_list_o)
# then calculates the (weighted) mean of all those scales with N (FBs) (prog_results_desc_o)
# and alternatively does the same for one aggregate level lower  (prog_results_desc_ou)


## Stiftungsbericht_new2
# matches and merges data
# calculates intra-individual statistics like this (option 1 & 2):
# it creates two registries of variables with a regex-pattern (_vor & _nac) 
# then merges these registries into one df with FB and variable names
# then extracts the paired vars from the original dfs into a list of dfs
# then calculates intra-individual stats on the raw data and puts them into a df

# alternatively (option 3), it first puts variables into scales (SK vor & nac)
# then performs non-parametric statistics on the scales (SK)
# !!!!! I could probably use this part, let's look at it in more detail below.


## Stiftungsbericht_meta_analyse
## this takes the resulting df of Stiftungsbericht_new2 for a meta-analysis

#### more detailed plan
## Stiftungsbericht_new2 has funtcion calculate_scales_bericht
# this calculates the scale, as input it takes the starting string of 
# the vars belonging to a scale and currently these are given by hand
# the rest of the data except for CODE gets deleted afterwards
# and the intra-individual statistics are calculated for the scales and
# put into a different table
# i could basically keep it this way, if the FBname or another identifier is left in

# the automatic scale building of FirstSteps2 seems to work good for "clean" datasets
# so lets try building the scales and put code and geschlecht etc. back in?
# or put the scale and reliability vars back into the original df? <- this seems to
# be the better option for the matching, that is still to be done!
# the second option works for me, next is the statistics.

### make statistics
##  first look at the document, make a list of analyses and values
##  compare it with result of stiftungsbericht_new2
##  add everything from the latter part to a final list of analyses
##  construct an empty df with all analysis result parameters as columns
##  -> either one entire table for all programms
##  -> or a list of dfs with programm-ID specific tables.



