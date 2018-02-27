################################################
#### Sayali's copy of Aaron's deduplication ####
################################################

####
#### Author: sphadke
#### Created on: 05/30/2017
#### Last edited on: 06/05/2017
####

rm(list=ls())
gc()

####
#### R setup
####
library(data.table)
library(RecordLinkage)
set.seed(132)

source("src/aschroed/parse_functions.R")


####
#### Get data
####
wvs <- fread("data/dhs_link_char/original/wvdemo4a.csv")
#asz <- fread("data/dhs_link_char/original/AnasaziClients.csv")


####
#### Data cleaning
####

## PREPARE DATA
# subset to the columns we want to match
wvs_dmg=wvs[,.(V1,V2,V3,V5,V12,V14,V18,V23)]
# parse dob to separate columns
wvs_dmg_prsd=parse_dob_mdy(wvs_dmg, "V12")
# create new column names
colnames(wvs_dmg_prsd)=c("id","last_name","first_name","gender","dob","ssn","street","zip","birth_yr","birth_mo","birth_dy")
# remove original dob column
wvs_dmg_prsd$dob<-NULL
# remove invalid ssn
wvs_dmg_prsd=remove_non_ssn(wvs_dmg_prsd, "ssn")
# change zip to string
wvs_dmg_prsd$zip <- as.character(wvs_dmg_prsd$zip)
# Geolocate addresses?

# take a look
summary(wvs_dmg_prsd)

## CREATE MATCH PAIRS - ??What is the JaroWinkler threshold set at??
pairs=compare.dedup(wvs_dmg_prsd,
                    exclude = c("id"),  # exclude fields not to be used
                    identity = wvs_dmg_prsd$id,  # set to a field that tells the real linkage (if available)
                    strcmp = TRUE,  # turns on string comparison, defaults to JaroWInkler
                    blockfld = list(c("last_name", "first_name"), c("birth_yr", "birth_mo"), c("ssn")))  # Creating Blocks to Reduce Useless Processing
# take a look
summary(pairs)

## SET m PROBABILITIES (u probabilities are set automatically using frequency)
ms=c(.95,.9,.85,.9,.4,.6,.95,.95,.85)

## GET AND APPLY WEIGHTS FOR PARAMETRIC MATCHING
fspairs=fsWeights(pairs, m = ms)
# Take a look at matched-pair weight distribution
table(fspairs$Wdata)
plot(table(fspairs$Wdata))
# Select a subset of pairs to study
extract=getPairs(fspairs, single.rows = TRUE)

extract=getPairs(fspairs, min.weight=30, max.weight=80)
# take a look (figure out your threshold(s))
View(extract)

# Extract matching pairs
#perfect_matched_ids <- extract[extract$id.1 == extract$id.2, c("id.1", "id.2", "id1", "id2")]
#perfect_match <- extract[which(extract$id.1 == extract$id.2),]


####
#### Demographics table for rows with identical system ids
####
conflict_rows <- matrix(NA, ncol = 2)
perfectly_matched_data <- data.frame(matrix(NA, nrow = nrow(perfect_matched_ids), ncol = ncol(wvs_dmg_prsd)))
colnames(perfectly_matched_data) <- colnames(wvs_dmg_prsd)

for(i in 1:nrow(perfect_matched_ids)){

  row_one <- perfect_matched_ids[i, 3]
  row_two <- perfect_matched_ids[i, 4]

  if(identical(wvs_dmg_prsd[row_one, ], wvs_dmg_prsd[row_two, ]) == TRUE){
    perfectly_matched_data[i, ] <- wvs_dmg_prsd[row_one, ]
  } else {

    for(j in 1:ncol(wvs_dmg_prsd)){
      if(is.na(wvs_dmg_prsd[row_one, j]) == TRUE && is.na(wvs_dmg_prsd[row_two ,j]) == FALSE) {
        perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_two, j]
      } else if(is.na(wvs_dmg_prsd[row_two ,j]) == TRUE && is.na(wvs_dmg_prsd[row_one ,j]) == FALSE) {
        perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_one, j]
      } else if(is.na(wvs_dmg_prsd[row_two ,j]) == TRUE && is.na(wvs_dmg_prsd[row_one ,j]) == TRUE) {
        perfectly_matched_data[i, j] <- NA
      } else if(wvs_dmg_prsd[row_one, j] == wvs_dmg_prsd[row_two ,j]){
        perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_one, j]
      } else{
        perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_one, j]
        conflict_rows <- rbind(conflict_rows, c(row_one, row_two))
      } #second if statement
      print(c(i,j))
    } #inner for loop
  } #first if statement
} #outer for loop


perfectly_matched_data <- unique(perfectly_matched_data)







# ## USE epiWeights instead of setting probabilities manually
# eppairs=epiWeights(pairs)  # How does this work!?
# #thrshld=getParetoThreshold(eppairs)
# cls=epiClassify(eppairs, .67)
# summary(eppairs)

# ## USE emWeights instead of setting probabilities manually -- VERY SLOWWWWW
# empairs=emWeights(pairs)  # How does this work!?
# cls=emClassify(empairs)
# summary(empairs)
