########################################################
#### Sayali + Zarni's copy of Aaron's deduplication ####
########################################################
## ANASAZI/CERNER system

####
#### Author: Aaron
#### Editor: Sayali and Zarni
#### Created on: 05/30/2017
#### Last edited on: 07/26/2017
####

rm(list=ls())
gc()


####
#### R setup
####
library(data.table)
library(dplyr)
library(ggplot2)
library(RecordLinkage)
library(stringr)

source("src/aschroed/parse_functions.R")


##
# Function to implement the majority rule
##
chooseBestModel <- function(x) {
  tabulatedOutcomes <- table(x)
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
}

##
# Remove rows that are completely NA
##
rm_all_NA <- function(x){
  all(is.na(x))
}


####
#### Get data
####
asz <- read.csv('~/git/dhs_link_char/data/dhs_link_char/original/AnasaziClients.csv')

## Below code loads other files pertaining to the same system
## These will be useful in later stage
## They have service dates. So deduplication should use record from the most recent one
# anasazi_client_roster <- read.csv('~/git/dhs_link_char/data/dhs_link_char/original/tblAnasaziClientRosterReport.csv')
#
# anasazi_encounters <- read.csv('~/git/dhs_link_char/data/dhs_link_char/original/tblAnasaziEncounters.csv')


####################
#### Data quirks
####################

#### Case.num == 0 includes various different things
## Deleted records, generic entries, entries where they didn't know what to put
## We are removing them all
asz[asz$Case.num == 0, ] <- NA

sub <- apply(asz, 1, rm_all_NA)
asz <- asz[!sub,]

row.names(asz) <- 1:nrow(asz)

#### Some Sort.name entries indicate that record A has been combined with B
## Format: "COMBINED: A INTO: B"

# Removes warning from matrix(NA, nrow(asz[grep(pattern = "COMBINED", asz$Sort.name),]), 2)
# and asz[grep(pattern = "COMBINED", asz$Sort.name),]
Sys.setlocale('LC_ALL','C')

# Creates a table of old and new ids
system_combined_ids <- matrix(NA, nrow(asz[grep(pattern = "COMBINED", asz$Sort.name),]), 2)
colnames(system_combined_ids) <- c("old_id", "new_id")
system_combined_ids <- as.data.frame(system_combined_ids)

temp <- asz[grep(pattern = "COMBINED", asz$Sort.name),]
temp$Sort.name <- as.character(temp$Sort.name)

# Print command shows problem cases
# Not dealing with those in the first go
# Must revisit
for(i in 1:nrow(temp)){
  unstr <- unlist(str_split(temp[i,4], " "))

  if(unstr[length(unstr)] == 0){
    system_combined_ids[i,] <- NA
  } else{
    if(length(unstr) == 4){
      system_combined_ids[i,1] <- unstr[2]
      system_combined_ids[i,2] <- unstr[4]
      } else if(length(unstr) == 3){
        system_combined_ids[i,1] <- temp$Case.num[i]
        system_combined_ids[i,2] <- unstr[3]
        } else {
          print(unstr)
        }
  }
}

sub <- apply(system_combined_ids, 1, rm_all_NA)
system_combined_ids <- system_combined_ids[!sub,]
system_combined_ids <- as.data.frame(system_combined_ids)

system_combined_ids[grep("INTO", system_combined_ids$new_id), 2] <- unlist(str_split(system_combined_ids[grep("INTO", system_combined_ids$new_id), 2], ":"))[2]

## Now we need to replace demographics in old_id records, with new_id records
# Choosing to do this after preprocessing (NA replacemets)


####
#### Preprocessing
####

#NA replacements so our majority rule function works from our experiment
asz[asz == ""] <- NA
asz[asz == "\n  "] <- NA
asz[asz == "NA"] <- NA

## Now we need to replace demographics in old_id records, with new_id records
# Choosing to do this after preprocessing (NA replacemets)
# The reason to not replace the case.num is that, this is the easiest way to building equivalent records
# The two observations will match perfectly, and list the two as same IDs

## Updated process to do this:
# Replicate the row with new_id, and change the case number
row.names(system_combined_ids) <- 1:nrow(system_combined_ids)

for(i in 1:nrow(system_combined_ids)){
# for(i in 1:41){
  # if(nrow(unique(asz[asz$Case.num == system_combined_ids$new_id[i],]) == 0))
  if(is.element(system_combined_ids$new_id[i], asz$Case.num)==FALSE) {
    asz <- asz
  } else {
    print(paste("old_id: ", system_combined_ids$old_id[i]))
    print(paste("case_id: ", which(asz$Case.num == system_combined_ids$old_id[i])))

    if(is.element(system_combined_ids$old_id[i], asz$Case.num)==TRUE) {
      rm_row_id <- which(asz$Case.num == system_combined_ids$old_id[i])
      asz <- asz[-rm_row_id, ]
    }
    asz <- rbind(asz, unique(asz[asz$Case.num == system_combined_ids$new_id[i],]))
    asz[nrow(asz), 3] <- as.numeric(system_combined_ids$old_id[i])
  }

  print(i)
}

row.names(system_combined_ids) <- 1:nrow(system_combined_ids)
row.names(asz) <- 1:nrow(asz)


####
#### Data cleaning
####

asz <- data.table(asz)

## PREPARE DATA
# subset to the columns we want to match
#sysid, last name, first name, gender, birthdaye, ssn, address, zipcode
asz_dmg <- asz[,.(Case.num, Last.name, First.name, Sex, Dob, Soc.sec.num, Address, Zip)]

# parse dob to separate columns
asz_dmg_prsd <- parse_dob_mdy(asz_dmg, "Dob")

# create new column names
colnames(asz_dmg_prsd) <- c("id", "last_name", "first_name", "gender", "dob", "ssn", "street", "zip", "birth_yr", "birth_mo", "birth_dy")

# remove original dob column
asz_dmg_prsd$dob <- NULL

# remove invalid ssn
asz_dmg_prsd <- remove_non_ssn(asz_dmg_prsd, "ssn")

# change zip to string
asz_dmg_prsd$zip <- as.character(asz_dmg_prsd$zip)

# Geolocate addresses?

# take a look
#summary(asz_dmg_prsd)
asz_dmg_prsd[asz_dmg_prsd == ""] <- NA
asz_dmg_prsd[asz_dmg_prsd == "\n  "] <- NA


####################
#### Matching
####################
## CREATE MATCH PAIRS - ??What is the JaroWinkler threshold set at??
pairs <- compare.dedup(asz_dmg_prsd,
                    exclude = c("id"),  # exclude fields not to be used
                    identity = asz_dmg_prsd$id,  # set to a field that tells the real linkage (if available)
                    strcmp = TRUE,  # turns on string comparison, defaults to JaroWInkler
                    blockfld = list(c("last_name", "first_name"), c("birth_yr", "birth_mo"), c("ssn")))  # Creating Blocks to Reduce Useless Processing
# take a look
#summary(pairs)


## SET m PROBABILITIES (u probabilities are set automatically using frequency)
# ORDER: ("last_name", "first_name", "gender", "ssn", "street", "zip", "birth_yr", "birth_mo", "birth_dy")
# Has very high missingness in SSN (table(is.na(asz_dmg_prsd$ssn)))
# So we changed the weight to 0.5 instead of 0.9
ms <- c(.95,.9,.85,.5,.5,.6,.95,.95,.85)

## GET AND APPLY WEIGHTS FOR PARAMETRIC MATCHING
fspairs <- fsWeights(pairs, m=ms)

# Take a look at matched-pair weight distribution
#table(fspairs$Wdata)
#plot(table(fspairs$Wdata))

# Select a subset of pairs to study
extract <- getPairs(fspairs, min.weight = 40, single.rows = TRUE)
extract <- getPairs(fspairs, min.weight = 40)


############################################
##GRAPH FOR CHOOSING DIFFERENT REGIONS
############################################
# ourpick <- fspairs$Wdata[fspairs$Wdata > 10]
# ourpick <- data.frame(ourpick)
# #View(ourpick)
# ourpick$match_status <- NA
# ourpick$match_status[ourpick$ourpick < 20] <- "definite_mismatch"
# ourpick$match_status[ourpick$ourpick >= 20 & ourpick$ourpick < 50] <- "under_review"
# ourpick$match_status[ourpick$ourpick > 50] <- "definite_match"
# #View(ourpick)
# ourpick$match_status

# #CUTS FOR THE HISTOGRAM PLOT
# cuts <- c(10,15,20,25,30,35,40,45, 50,55,60,65,70,75,80,100)
# #colors<- c(rep("#D55E00",2), rep("#F0E442",6), rep("#009E73",7))
# ggfinal <- ggplot(ourpick, aes(x=ourpick, fill = match_status)) +
#   geom_histogram(breaks = cuts) +
#   ggtitle("FS Weight Scores by Volume")+
#   labs(x = "FS Weight Scores", y = "Counts") +
#   scale_fill_manual(values = c( "#009E73","#D55E00","#F0E442"), name = "Match Status") +
#   theme(axis.text.x = element_text(size = 24),
#         axis.text.y = element_text(size = 24),
#         title = element_text(size = 24),
#         legend.text = element_text(size = 20),
#         axis.title = element_text(size = 20)) +
#   theme(plot.title = element_text(hjust = 0.5))
#
# suppressWarnings(print(ggfinal))

# png(filename = "./output/ggfinals.png",
#     units = "in", width = 8, height = 8,
#     res = 72, bg = "transparent")
# ggfinal
# dev.off()


####
#### DUPLICATES WITH SAME EXACT SYSTEM IDS
#### This is a matter of merging the demographic variables together
#### How can you have the same system ID and have multiple entries with different demographic variables input?
####

#################################
## SYSTEM IDS match
#################################
# We set min.weight = 52
extract <- getPairs(fspairs, min.weight = 52, single.rows = TRUE)
nrow(extract) #295

# Weight 50-52 has suspected twins of the same gender
# Weight ~47 has suspected twins of opposite gender
# Weights in between give funky matches!

# CASES NOT TAKEN CARE OF
# Extract matching pairs
# Check Logic here!
# Currently, we are not taking into account cases like:
# 27, 28
# 27, 29
# 28, 29
# 28, 54 (54 is an outlier: We have not taken care of that case)

matched_system_ids <- extract[extract$id.1 == extract$id.2, c("id1", "id2", "id.1", "id.2")]
# View(matched_system_ids)
nrow(matched_system_ids) #0

## Getting rid of all-NA rows
sub <- apply(matched_system_ids, 1, rm_all_NA)
matched_system_ids <- matched_system_ids[!sub,]


# Combined the row keys together
matched_system_ids$comb <- paste(matched_system_ids$id1, matched_system_ids$id2, sep = ",")
# Group all the row keys by system id together
matched_system_ids_group <- matched_system_ids %>% group_by(id.1) %>%
  mutate(pairs = paste(comb, collapse=','))
# View(matched_system_ids_group)
# Make the pair keys unique
matched_system_ids_list <- unique(matched_system_ids_group$pairs)

# Convert it back to data frame as it was a data table class to match with Aaron's
asz_dmg_prsd <- as.data.frame(asz_dmg_prsd)
# An empty data frame to save our results
demo_dedup_output <- matrix(NA, 1, ncol = ncol(asz_dmg_prsd))

# We pull a list of those rows that have the same system ID out.
# We merged them using marjority rule and output it to our perfect table.
for (i in 1:length(matched_system_ids_list)){
  results_perfect <- as.data.frame(asz_dmg_prsd[c(unique(unlist(strsplit(matched_system_ids_list[i], ",")))) , ])
  output <- as.matrix(t(apply(results_perfect, 2, chooseBestModel)))
  demo_dedup_output <- rbind(demo_dedup_output, output)
}

#View(demo_dedup_output)
demo_dedup_output <- demo_dedup_output[-1,]
nrow(demo_dedup_output) # 18222

demo_dedup_output <- data.frame(demo_dedup_output)

## To make sure that all hell doesn't break loose over these unpopulates matrices,
# we name the columns
colnames(demo_dedup_output) <- colnames(asz_dmg_prsd)
# View(demo_dedup_output)

# save(demo_dedup_output, file="./data/dhs_link_char/working/asz_sameid_dedup_21_50.RData")

#####################################################################################
#### SYSTEM IDS DO NOT MATCH:
#### We are grouping by RowIDs
#### We increased the weights because for the imperfect matches, less than 50 just doesn't make sense. Need to examine this logic further.
#####################################################################################
extract <- getPairs(fspairs, min.weight = 52, single.rows = TRUE)

imperfect_matched_ids <- extract[extract$id.1 != extract$id.2, c("id1", "id2", "id.1", "id.2")]
# View(imperfect_matched_ids)
nrow(imperfect_matched_ids)
#plot(table(imperfect_matched_ids$Weight))

## Getting rid of all-NA rows
sub <- apply(imperfect_matched_ids, 1, rm_all_NA)
imperfect_matched_ids <- imperfect_matched_ids[!sub,]


#Pulling the
imperfect_matched_ids$comb <- paste(imperfect_matched_ids$id.1, imperfect_matched_ids$id.2, sep = ",")
#View(imperfect_matched_ids)
imperfect_matched_ids_group <- imperfect_matched_ids %>% group_by(id.1) %>%
  mutate(pairs = paste(comb, collapse=','))
# View(imperfect_matched_ids_group)

#This is the System ID pairs that are the same.
imperfect_ids <- unique(imperfect_matched_ids_group$pairs)
length(imperfect_ids) #291 rows of it # But there are many ids inside
#imperfect_ids[1:10]

asz_dmg_prsd <- as.data.frame(asz_dmg_prsd)
imperfect_output <- matrix(NA, 1, ncol = ncol(asz_dmg_prsd))
for (i in 1:length(imperfect_ids)){
  #Should we bother to only put unique system keys in when the index will expand it back up
  results_imper <- as.data.frame(asz_dmg_prsd[which(asz_dmg_prsd$id %in% unique(unlist(strsplit(imperfect_ids[i], ",")))) , ])
  test <- results_imper
  #View(results_imper)
  output <- as.matrix(t(apply(results_imper, 2, chooseBestModel)))
  imperfect_output <- rbind(imperfect_output, output)
  #View(imperfect_output)
  print(i)
}
imperfect_output <- imperfect_output[-1,]
imperfect_output <- data.frame(imperfect_output)
# View(imperfect_output)

# save(imperfect_output, file="./data/dhs_link_char/working/asz_imperfect_matches_dem_40_high.RData")

################################################################################
###CHECK OVERLAPS BETWEEN THOSE WITH SAME SYSTEM IDS and DIFFERENT SYSTEM IDS
################################################################################

#######################################################
#1. Get the Overlapped rows from table with Same System IDs
#######################################################
missing_index_per <- match(imperfect_output$id, demo_dedup_output$id)
length(missing_index_per)
getting_demo_dedup_index <- which(!is.na(missing_index_per))
demo_dedup_index <- missing_index_per[getting_demo_dedup_index]
overlap_from_same_system_id <- data.frame(demo_dedup_output[demo_dedup_index,])

#######################################################
#1.5 Get the Overlapped rows from table with diff System IDs
#######################################################
missing_index_per <- match(demo_dedup_output$id,imperfect_output$id)
length(missing_index_per)
getting_imperfect_index <- which(!is.na(missing_index_per))
imperfect_index <- missing_index_per[getting_imperfect_index]
overlap_from_diff_system_id <- data.frame(imperfect_output[imperfect_index,])

##############################################################
#2. Rowbind overlaps and Merge Demographic data
# TO DO: Where does this Duplicate ID come from?
##############################################################
overlap_combined <- rbind(overlap_from_same_system_id, overlap_from_diff_system_id)
overlap_combined<- as.data.frame(apply(overlap_combined, 2, unlist))

#MERGE EACH OF THEM
# Nothing exists
overlap_merged <- overlap_combined %>% group_by(id) %>%
  summarize_each(funs(chooseBestModel))

##############################################################
#2. Rowbind overlaps and Merge Demographic data
# TO DO: Where does this Duplicate ID come from?
##############################################################

#Making sure both are data frames
demo_dedup_output <- as.data.frame(demo_dedup_output)
overlap_merged <- data.frame()

demo_dedup_output$id <- unlist(demo_dedup_output$id)
overlap_merged$id <- as.character(overlap_merged$id)

#APPLY ANTI JOINS TO GET
#1 FROM SAME SYSTEM IDS == THE UNMATCHED ONES
# Wouldn't work, since there's nothing in overlap
# So we create a blank one to facilitate rbinds
# same_sys_remaining <- anti_join(demo_dedup_output, overlap_merged, by = c("id"))
same_sys_remaining <- matrix(NA, 1, 10)
colnames(same_sys_remaining) <- colnames(demo_dedup_output)

#2 FROM DIFFERNET SYSTEM IDS == THE UNMATCHED ONES
imperfect_output$id <- as.character(imperfect_output$id)
diff_sys_remaining <- anti_join(imperfect_output, overlap_merged, by = c("id"))

#3 MERGE #1 + #2 with Overlap_merged
dedup_final_merged <- rbind(same_sys_remaining, diff_sys_remaining)
dedup_final_merged <- rbind(dedup_final_merged, overlap_merged)
sub <- apply(dedup_final_merged, 1, rm_all_NA)
dedup_final_merged <- dedup_final_merged[!sub,]

nrow(dedup_final_merged) #413
length(unique(dedup_final_merged$id)) #413! Yes, all the IDs are UNIQUE!

#4 GATHER ALL THE SYSTEM IDS THAT HAS BEEN DEDUPLICATED

#a) From the dedup_final_merged list
dedup_all_ids <- dedup_final_merged$id
length(dedup_all_ids) #291

#b) From the imperfect systemid list: Picked the second id list as this is the one we played by. Combine and Unique to get all Unique IDs
dedup_all_ids <- append(dedup_all_ids, imperfect_matched_ids$id.2)
dedup_all_ids <- unique(dedup_all_ids)
length(dedup_all_ids) #569 something
dedup_all_ids <- as.numeric(dedup_all_ids)

#c) Convert this to data frame then Anti-join with the main data set
dedup_all_ids <- as.data.frame(dedup_all_ids)
names(dedup_all_ids)[1] <- "id"

asz_dmg_prsd <- as.data.frame(asz_dmg_prsd)
asz_unmerged <- anti_join(asz_dmg_prsd, dedup_all_ids, by = "id")

#GET EVERYTHING ELSE THAT IS NOT FROM THE LIST (THE MAJOR LIST)

asz_final <- rbind(dedup_final_merged, asz_unmerged)

nrow(asz_final) #56245
nrow(asz_dmg_prsd) #56523

save(asz_final, file = "./data/dhs_link_char/final/asz_final.RData")
#278 rows has been removed from the total table

#### SAYALI STILL NEEDS TO SAVE MATCHED IDs


######################
## NEXT STEPS
######################
#Need to add table code to the table outputs we are having
#How do we combined the results back
#Perfect matches : 25745 ==> Pared it down to 18223 #7,522 duplicate
#Imperfect matches : 103 ==> Pared it down to 70 (This is of a threshold we are certain of) #Redo-counts of how many
#Such system Ids appear 103-70 --> 33
#How are we going to combine with the rest of the system?
#Out of the 60,000 We can reduce the system by system id keys from the perfect imperfect
#Whateveris remaining would be the table we haven't duplicated
#What if it's a lower extract point at around 40 for Web Vision
#Maybe 35. Do more explorations_save it with extraction points
#Labeled 21_50_80


####
#### Automation
####

# ## USE epiWeights instead of setting probabilities manually
# eppairs=epiWeights(pairs)  # How does this work!?
# #thrshld=getParetoThreshold(eppairs)
# cls=epiClassify(eppairs, .67)
# summary(eppairs)

# ## USE emWeights instead of setting probabilities manually -- VERY SLOWWWWW
# empairs=emWeights(pairs)  # How does this work!?
# cls=emClassify(empairs)
# summary(empairs)




####
#### Demographics table for rows with identical system ids
####
# conflict_rows <- matrix(NA, ncol = 2)
# perfectly_matched_data <- data.frame(matrix(NA, nrow = nrow(matched_system_ids), ncol = ncol(asz_dmg_prsd)))
# colnames(perfectly_matched_data) <- colnames(asz_dmg_prsd)
# asz_dmg_prsd <- as.data.frame(asz_dmg_prsd)
#
# for(i in 1:nrow(matched_system_ids)){
#
#   row_one <- matched_system_ids[i, 3]
#   row_two <- matched_system_ids[i, 4]
#
#   if(identical(asz_dmg_prsd[row_one, ], asz_dmg_prsd[row_two, ]) == TRUE){
#     perfectly_matched_data[i, ] <- asz_dmg_prsd[row_one, ]
#   } else {
#
#     for(j in 1:ncol(asz_dmg_prsd)){
#       if(is.na(asz_dmg_prsd[row_one, j]) == TRUE && is.na(asz_dmg_prsd[row_two ,j]) == FALSE) {
#         perfectly_matched_data[i, j] <- asz_dmg_prsd[row_two, j]
#       } else if(is.na(asz_dmg_prsd[row_two ,j]) == TRUE && is.na(asz_dmg_prsd[row_one ,j]) == FALSE) {
#         perfectly_matched_data[i, j] <- asz_dmg_prsd[row_one, j]
#       } else if(is.na(asz_dmg_prsd[row_two ,j]) == TRUE && is.na(asz_dmg_prsd[row_one ,j]) == TRUE) {
#         perfectly_matched_data[i, j] <- NA
#       } else if(asz_dmg_prsd[row_one, j] == asz_dmg_prsd[row_two ,j]){
#         perfectly_matched_data[i, j] <- asz_dmg_prsd[row_one, j]
#       } else{
#         perfectly_matched_data[i, j] <- asz_dmg_prsd[row_one, j]
#         conflict_rows <- rbind(conflict_rows, c(row_one, row_two))
#       } #second if statement
#       print(c(i,j))
#     } #inner for loop
#   } #first if statement
# } #outer for loop


