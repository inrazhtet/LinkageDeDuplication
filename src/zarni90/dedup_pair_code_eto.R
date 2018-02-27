################################################
#### Sayali + Zarni 's copy of Aaron's deduplication ####
################################################

####
#### Author: Aaron
#### Editor: Sayali and Zarni
#### Created on: 05/30/2017
#### Last edited on: 07/24/2017
####

rm(list=ls())
gc()

####
#### R setup
####
library(data.table)
library(dplyr)
library(RecordLinkage)
library(ggplot2)
set.seed(132)

source("src/aschroed/parse_functions.R")

# Function to implement the majority rule
chooseBestModel <- function(x) {
  tabulatedOutcomes <- table(x)
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
}

#TESTING:
#chooseBestModel(unlist(overlap_combined$first_name))


####
#### Get data
####
eto <- read.csv("data/dhs_link_char/original/ETO_DHSNewDemographic.csv", header = TRUE)
#asz <- fread("data/dhs_link_char/original/AnasaziClients.csv")
eto <- data.table(eto)
eto$street <- paste(eto$ResidenceStreetNumber, eto$ResidenceStreetName, eto$ResidenceDirectionIndicator,eto$ResidenceStreetType, sep = " ")
#NA replacements so our majority rule function works from our experiment
eto[eto == ""] <- NA
eto[eto == "\n  "] <- NA
eto[eto == "NA"] <- NA

####
#### Data cleaning
####

## PREPARE DATA
# subset to the columns we want to match
eto <- as.data.frame(eto)
View(eto)
sum(is.na(eto$SSN))/nrow(eto)
#8 columns for Web Vision
eto_dmg=eto[,c(1,3,5,6,7,8, 14,17)]
#convert this back to data table to work with Aaron's code
eto_dmg <- data.table(eto_dmg)
# parse dob to separate columns
eto_dmg_prsd=parse_dob_mdy(eto_dmg, "BirthDate")
# rearrange eto_dmg columns to fit the model from below
eto_dmg_prsd <- as.data.frame(eto_dmg_prsd)
eto_dmg_prsd <- eto_dmg_prsd[,c(1,3,2,4,5,6,8,7,9,10,11)]
View(eto_dmg_prsd)
# create new column names
colnames(eto_dmg_prsd)=c("id","last_name","first_name","gender","dob","ssn","street","zip","birth_yr","birth_mo","birth_dy")
# remove original dob column
eto_dmg_prsd$dob<-NULL
# remove invalid ssn
eto_dmg_prsd <- data.table(eto_dmg_prsd)
eto_dmg_prsd=remove_non_ssn(eto_dmg_prsd, "ssn")
# change zip to string
eto_dmg_prsd$zip <- as.character(eto_dmg_prsd$zip)

nrow(unique(eto_dmg_prsd)) #25,437

# Geolocate addresses?


# take a look
#summary(wvs_dmg_prsd)

## CREATE MATCH PAIRS - ??What is the JaroWinkler threshold set at??
pairs=compare.dedup(eto_dmg_prsd,
                    exclude = c("id"),  # exclude fields not to be used
                    identity = eto_dmg_prsd$id,  # set to a field that tells the real linkage (if available)
                    strcmp = TRUE,  # turns on string comparison, defaults to JaroWInkler
                    blockfld = list(c("last_name", "first_name"), c("birth_yr", "birth_mo"), c("ssn")))  # Creating Blocks to Reduce Useless Processing
# take a look
#summary(pairs)

## SET m PROBABILITIES (u probabilities are set automatically using frequency)
ms=c(.95,.9,.85,.9,.4,.6,.95,.95,.85)
names(eto_dmg_prsd)

## GET AND APPLY WEIGHTS FOR PARAMETRIC MATCHING
fspairs=fsWeights(pairs, m = ms)

# Take a look at matched-pair weight distribution
#table(fspairs$Wdata)
plot(table(fspairs$Wdata))
# Select a subset of pairs to study
#extract=getPairs(fspairs, single.rows = TRUE)

# ############################################
# ##GRAPH FOR CHOOSING DIFFERENT REGIONS
# ############################################
# ourpick <- fspairs$Wdata[fspairs$Wdata > 10]
# ourpick <- data.frame(ourpick)
# #View(ourpick)
# ourpick$match_status <- NA
# ourpick$match_status[ourpick$ourpick < 20] <- "definite_mismatch"
# ourpick$match_status[ourpick$ourpick >= 20 & ourpick$ourpick < 50] <- "under_review"
# ourpick$match_status[ourpick$ourpick > 50] <- "definite_match"
# #View(ourpick)
# ourpick$match_status
#
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
#
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
# We set min.weight = 21 because for identical system.ids, 21 is the lowest
extract = getPairs(fspairs, single.rows = TRUE)
nrow(extract) #29,345
View(extract)

# CASES NOT TAKEN CARE OF
# Extract matching pairs
# Check Logic here!
# Currently, we are not taking into account cases like:
# 27, 28
# 27, 29
# 28, 29
# 28, 54 (54 is an outlier: We have not taken care of that case)

matched_system_ids <- extract[extract$id.1 == extract$id.2, c("id1", "id2", "id.1", "id.2")]
View(matched_system_ids)

#Combined the row keys together
matched_system_ids$comb <- paste(matched_system_ids$id1, matched_system_ids$id2, sep = ",")
#Group all the row keys by system id together
matched_system_ids_group <- matched_system_ids %>% group_by(id.1) %>%
  mutate(pairs = paste(comb, collapse=','))
View(matched_system_ids_group)
#Make the pair keys unique
matched_system_ids_list <- unique(matched_system_ids_group$pairs)

#Convert it back to data frame as it was a data table class to match with Aaron's
eto_dmg_prsd <- as.data.frame(eto_dmg_prsd)
#An empty data frame to save our results
demo_dedup_output <- matrix(NA, 1, ncol = ncol(eto_dmg_prsd))

#We pull a list of those rows that have the same system ID out.
#We merged them using marjority rule and output it to our perfect table.
for (i in 1:length(matched_system_ids_list)){
  results_perfect <- as.data.frame(eto_dmg_prsd[c(unique(unlist(strsplit(matched_system_ids_list[i], ",")))) , ])
  output <- as.matrix(t(apply(results_perfect, 2, chooseBestModel)))
  demo_dedup_output <- rbind(demo_dedup_output, output)
}

#There are bounds to be NA NA NA NA streets as we inserted NA NA NA to each of the row for the majority rule to work
View(demo_dedup_output) # 143
demo_dedup_output <- demo_dedup_output[-1,]
nrow(demo_dedup_output) # 142 outputs

demo_dedup_output <- data.frame(demo_dedup_output)
View(demo_dedup_output)

save(demo_dedup_output, file="./data/dhs_link_char/working/eto_sameid_dedup.RData")

#####################################################################################
#### SYSTEM IDS DO NOT MATCH:
#### We are grouping by RowIDs
#### We increased the weights because for the imperfect matches, less than 50 just doesn't make sense. Need to examine this logic further.
#####################################################################################
#12 -- 37 so far not matching for SSN and address

extract=getPairs(fspairs, min.weight=37, max.weight=80, single.rows = TRUE)
View(extract)

imperfect_matched_ids <- extract[extract$id.1 != extract$id.2, c("id1", "id2", "id.1", "id.2")]
View(imperfect_matched_ids)
nrow(imperfect_matched_ids) #105 Systems Ids are not the same
#plot(table(imperfect_matched_ids$Weight))

#Pulling the
imperfect_matched_ids$comb <- paste(imperfect_matched_ids$id.1, imperfect_matched_ids$id.2, sep = ",")
#View(imperfect_matched_ids)
imperfect_matched_ids_group <- imperfect_matched_ids %>% group_by(id.1) %>%
  mutate(pairs = paste(comb, collapse=','))
View(imperfect_matched_ids_group)

#This is the System ID pairs that are the same.
imperfect_ids <- unique(imperfect_matched_ids_group$pairs)
length(imperfect_ids) #309 rows of it # But there are many ids inside
#imperfect_ids[1:10]

eto_dmg_prsd <- as.data.frame(eto_dmg_prsd)
imperfect_output <- matrix(NA, 1, ncol = ncol(eto_dmg_prsd))
for (i in 1:length(imperfect_ids)){
  #Should we bother to only put unique system keys in when the index will expand it back up
  results_imper <- as.data.frame(eto_dmg_prsd[which(eto_dmg_prsd$id %in% unique(unlist(strsplit(imperfect_ids[i], ",")))) , ])
  test <- results_imper
  #View(results_imper)
  output <- as.matrix(t(apply(results_imper, 2, chooseBestModel)))
  imperfect_output <- rbind(imperfect_output, output)
  #View(imperfect_output)
  #print(i)
}
imperfect_output <- imperfect_output[-1,]
imperfect_output <- data.frame(imperfect_output)
View(imperfect_output)

save(imperfect_output, file="./data/dhs_link_char/working/eto_imperfect_matches_dem_37_80.RData")

################################################################################
###CHECK OVERLAPS BETWEEN THOSE WITH SAME SYSTEM IDS and DIFFERENT SYSTEM IDS
################################################################################

#######################################################
#1. Get the Overlapped rows from table with Same System IDs
#######################################################
imperfect_output$id <- unlist(imperfect_output$id)
demo_dedup_output$id <- unlist(demo_dedup_output$id)

missing_index_per <- match(imperfect_output$id, demo_dedup_output$id)
length(missing_index_per)
#THERE ARE NO OVERLAPS SO CODE BELOW IS USELESS AND THUS COMMENTED OUT!
# getting_demo_dedup_index <- which(!is.na(missing_index_per))
# demo_dedup_index <- missing_index_per[getting_demo_dedup_index]
# overlap_from_same_system_id <- data.frame(demo_dedup_output[demo_dedup_index,])
#
# #######################################################
# #1.5 Get the Overlapped rows from table with diff System IDs
# #######################################################
# missing_index_per <- match(demo_dedup_output$id,imperfect_output$id)
# length(missing_index_per)
# getting_imperfect_index <- which(!is.na(missing_index_per))
# imperfect_index <- missing_index_per[getting_imperfect_index]
# overlap_from_diff_system_id <- data.frame(imperfect_output[imperfect_index,])
#
# ##############################################################
# #2. Rowbind overlaps and Merge Demographic data
# # TO DO: Where does this Duplicate ID come from?
# ##############################################################
# overlap_combined <- rbind(overlap_from_same_system_id, overlap_from_diff_system_id)
# overlap_combined<- as.data.frame(apply(overlap_combined, 2, unlist))
#
# View(overlap_combined)
#
# #MERGE EACH OF THEM
# overlap_merged <- overlap_combined %>% group_by(id) %>%
#   summarize_each(funs(chooseBestModel))
#
# ##############################################################
# #2. Rowbind overlaps and Merge Demographic data
# # TO DO: Where does this Duplicate ID come from?
# ##############################################################

#Making sure both are data frames
demo_dedup_output <- as.data.frame(demo_dedup_output)
overlap_merged <- as.data.frame(rbind(demo_dedup_output, imperfect_output))

demo_dedup_output$id <- unlist(demo_dedup_output$id)
overlap_merged$id <- as.character(overlap_merged$id)

########################################################################
#UNNEEDED BECAUSE THERE WAS NO OVERLAP!
#APPLY ANTI JOINS TO GET
# #1 FROM SAME SYSTEM IDS == THE UNMATCHED ONES
# same_sys_remaining <- anti_join(demo_dedup_output, overlap_merged, by = c("id"))
#
# #2 FROM DIFFERNET SYSTEM IDS == THE UNMATCHED ONES
# imperfect_output$id <- as.character(imperfect_output$id)
# diff_sys_remaining <- anti_join(imperfect_output, overlap_merged, by = c("id"))
#
# #3 MERGE #1 + #2 with Overlap_merged
# dedup_final_merged <- rbind(same_sys_remaining, diff_sys_remaining)
# dedup_final_merged <- rbind(dedup_final_merged, overlap_merged)
# nrow(dedup_final_merged) #18240
# length(unique(dedup_final_merged$id)) #18240! Yes, all the IDs are UNIQUE!

#4 GATHER ALL THE SYSTEM IDS THAT HAS BEEN DEDUPLICATED
dedup_final_merged <- overlap_merged

#a) From the dedup_final_merged list
dededup_all_ids <- dedup_final_merged$id
length(dededup_all_ids) #451
#b) From the imperfect systemid list: Picked the second id list as this is the one we played by. Combine and Unique to get all Unique IDs
dedup_all_ids <- append(dededup_all_ids, imperfect_matched_ids$id.2)
dedup_all_ids <- unique(dedup_all_ids)
length(dedup_all_ids) #729
#c) Convert this to data frame then Anti-join with the main data set
dededup_all_ids <- as.data.frame(dedup_all_ids)

names(dededup_all_ids)[1] <- "id"
eto_dmg_prsd$id <- as.character(eto_dmg_prsd$id)
dededup_all_ids$id <- as.character(dededup_all_ids$id)
eto_unmerged <- anti_join(eto_dmg_prsd, dededup_all_ids, by = c("id"))

#GET EVERYTHING ELSE THAT IS NOT FROM THE LIST (THE MAJOR LIST)

eto_final <- rbind(dedup_final_merged, eto_unmerged)
eto_final <- as.data.frame(eto_final)
View(eto_final)

save(eto_final, file = "~/git/dhs_link_char/data/dhs_link_char/final/eto_final.RData")

nrow(eto_final) #25159
nrow(eto_dmg_prsd) #25619

#21595 rows has been removed from the total table




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
# perfectly_matched_data <- data.frame(matrix(NA, nrow = nrow(matched_system_ids), ncol = ncol(wvs_dmg_prsd)))
# colnames(perfectly_matched_data) <- colnames(wvs_dmg_prsd)
# wvs_dmg_prsd <- as.data.frame(wvs_dmg_prsd)
#
# for(i in 1:nrow(matched_system_ids)){
#
#   row_one <- matched_system_ids[i, 3]
#   row_two <- matched_system_ids[i, 4]
#
#   if(identical(wvs_dmg_prsd[row_one, ], wvs_dmg_prsd[row_two, ]) == TRUE){
#     perfectly_matched_data[i, ] <- wvs_dmg_prsd[row_one, ]
#   } else {
#
#     for(j in 1:ncol(wvs_dmg_prsd)){
#       if(is.na(wvs_dmg_prsd[row_one, j]) == TRUE && is.na(wvs_dmg_prsd[row_two ,j]) == FALSE) {
#         perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_two, j]
#       } else if(is.na(wvs_dmg_prsd[row_two ,j]) == TRUE && is.na(wvs_dmg_prsd[row_one ,j]) == FALSE) {
#         perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_one, j]
#       } else if(is.na(wvs_dmg_prsd[row_two ,j]) == TRUE && is.na(wvs_dmg_prsd[row_one ,j]) == TRUE) {
#         perfectly_matched_data[i, j] <- NA
#       } else if(wvs_dmg_prsd[row_one, j] == wvs_dmg_prsd[row_two ,j]){
#         perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_one, j]
#       } else{
#         perfectly_matched_data[i, j] <- wvs_dmg_prsd[row_one, j]
#         conflict_rows <- rbind(conflict_rows, c(row_one, row_two))
#       } #second if statement
#       print(c(i,j))
#     } #inner for loop
#   } #first if statement
# } #outer for loop


