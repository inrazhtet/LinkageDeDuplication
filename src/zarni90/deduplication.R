library(data.table)
library(RecordLinkage)
library(mice)
library(dplyr)

#A lot of the heavy liftings is done
#Two functions that take care of month and day
#Check it out later

#ETO, Anasazi, Web Vision

source("src/aschroed/parse_functions.R")

## IMPORT DATA
#WV demo file that she sent us
#Fastest Load -- Orders of magnitude it.
wvs <- read.csv("data/dhs_link_char/original/wvdemo4a.csv", header = FALSE)
#What is WVS
View(wvs)
wvs <- data.table(wvs)
head(wvs)
dim(wvs)
head(wvs)

## PREPARE DATA
# subset to the columns we want to match
wvs_dmg=wvs[,.(V1,V2,V3,V5,V12,V14,V18,V23)]
names(wvs)
head(wvs)
View(wvs)
dim(wvs)
md.pattern(wvs_dmg)
names(wvs_dmg)
#Get rid of empty rows: TO DO: WHAT IS IT THAT WE ARE SUPOSED TO DO HERE?

# parse dob to separate columns

#Function takes care of spitting DOB into separate components
wvs_dmg_prsd=parse_dob_mdy(wvs_dmg, "V12")
names(wvs_dmg)
nrow(wvs_dmg_prsd)
View(wvs_dmg_prsd)
names(wvs_dmg_prsd)
# create new column names
colnames(wvs_dmg_prsd)=c("id","last_name","first_name","gender","dob","ssn","street","zip","birth_yr","birth_mo","birth_dy")
# remove original dob column
# cannot be used in the other match.
wvs_dmg_prsd$dob<-NULL
# remove invalid ssn
# SSN command # Improve to have it 8 or 9.
wvs_dmg_prsd=remove_non_ssn(wvs_dmg_prsd, "ssn")
# change zip to string
# Zip as a character: Use as an identifier then have it as a character
wvs_dmg_prsd$zip <- as.character(wvs_dmg_prsd$zip)
# Geolocate addresses?

# take a look
summary(wvs_dmg_prsd)

## CREATE MATCH PAIRS - ??What is the JaroWinkler threshold set at??
##
pairs=compare.dedup(wvs_dmg_prsd,
                    exclude = c("id"),  # exclude fields not to be used
                    identity = wvs_dmg_prsd$id,  # set to a field that tells the real linkage (if available) # Checking it? #Not going to be use
                    strcmp = TRUE,  # turns on string comparison, defaults to JaroWInkler # What's the default threshold? Delve into the code! #What's the threshold
                    blockfld = list(c("last_name", "first_name"), c("birth_yr", "birth_mo"), c("ssn")))  # Creating Blocks to Reduce Useless Processing # Give me a block of all those that already matches on first and last name. To reduce expand.grid matches as a hierarchy:

pairs$pairs

#EXPLORATION CODE COMMENTED OUT
# summary(pairs)
# nrow(pairs$data)
# head(pairs$data) #What is ths data showing
# length(pairs$frequencies) #calculating the frequencies value
# pairs$frequencies #How do you interpret the frequencies from this?
# length(pairs$type)
# pairs$type
# test <- pairs$pairs[1:10]
# test
# #When does Jarowinkler returns NA? It probably does when you do not an empty column entry in one of the string comparator.
# #Let's check
# names(wvs_dmg_prsd)
# class(wvs_dmg_prsd$id)
# wvs_dmg_prsd[wvs_dmg_prsd$id == "1",]
# wvs_dmg_prsd[1,] # NO SSN HERE!, YES STREET
# wvs_dmg_prsd[2667,] # NO SSN HERE!, NO STREET!
#
# length(unique(wvs_dmg_prsd$id))
# length(unique(pairs$pairs$id1))
#
# test1 <- test[test$id1 == "1",]
# View(test1)
# apply(test1[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8
# min(apply(test1[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8)
#
# test2 <- test[test$id1 == "2",]
# apply(test2[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8
# min(apply(test2[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8)
#
# test2 <- test[test$id1 == "1000",]
# apply(test2[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8
# min(apply(test2[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8)
#
# #0.25 is the minimum in this round
# #This is one computation of it!Assuming we count the NAs and we are looking for a minimum total cut off point.
# min(apply(pairs$pairs[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8)
# #
# View(pairs$pairs)


#I believe pairs would refer to each of the row.

#Matches are the ones that are duplicates
#The rest are not duplicates


## SET m PROBABILITIES (u probabilities are set automatically using frequency)
## We are putting in our weights!
## M are a measure of quality.
## U is just specificity (A thousand values)

## Right weights for some of them: How do we figure them out?
## Don't set the U because this program does it for us.

#last name #first name # gender # ssn # street # zip # birth year  birth month # birth date
#This is Aaron's code to weight things

#QUESTION: There are 10 columns and you only have 9 weights!

ms=c(.95,.9,.85,.9,.4,.6,.95,.95,.85)
names(wvs_dmg_prsd)
##
#[1] "id"         "last_name"  "first_name" "gender"     "ssn"        "street"     "zip"        "birth_yr"   "birth_mo"
#[10] "birth_dy"
##
## GET AND APPLY WEIGHTS FOR PARAMETRIC MATCHING

## fs: Felligi & Sunter
## Taking the ms and calcuing us
## Two cut off points: We will give them the two cut off points
## Where the matches are good/very good?
## Where the matches are bad and why might it be bad?
## Two cut off points:

#With weights we are calculating the scores!

fspairs=fsWeights(pairs, m = ms) #We are calculating Fellegi and Sunter's weight scores

typeof(fspairs$Wdata)
head(fspairs$pairs)
head(fspairs$data)
typeof(fspairs$Wdata)

#View(fspairs$Wdata)


# Take a look at matched-pair weight distribution
table(fspairs$Wdata) #Examine it and figure out where all the clusters of scores are. In those cases, you would want to pull them because they are going to have similar
                     #characteristics depending on the weights you have put down.

#It's good that a lot of pairs do not match
plot(table(fspairs$Wdata), xlab = "Weight Scores", ylab = "Pair Volumes", main = "Distribution of FS Scores across pairs")
#plot(table(fspairs$))
plot(table(fspairs$Wdata > 50))

table(fspairs$Wdata > 50)
ourpick <- fspairs[fspairs$Wdata > 15]
plot(table(ourpick$Wdata), xlab = "Weight Scores", ylab = "Pair Volumes", main = "Distribution of FS Scores across pairs")

#fsclassify performs classification based on the calculated weights. All record paris with weights greater or equal threshold.upper
#are classified as links. Record pairs with weights smaller than threshold.upper and greater or equal threshold.lower are
#classied as possible links. All remains records are classified as non-links.


#STARTS HERE!!!
#LOOK AT fspairs$data, fspairs$pairs, extract and wvs_dmg_prsd
# Select a subset of pairs to study
extract=getPairs(fspairs, min.weight=10, max.weight=80,single.rows = TRUE)
#These should be considered matches that need further review


View(extract)

table(extract$id.1 == extract$id.2)

extract_perfect_match <- extract[extract$id.1 == extract$id.2,]
View(extract_perfect_match) #Bottom for exact perfect match is around 37.
nrow(extract_perfect_match) #25745
View(extract_perfect_match[24000:24500,])
extract_non_perfect_match <- extract[extract$id.1 != extract$id.2,]
View(extract_non_perfect_match)

table(extract$id.1 == extract$id.2)
nrow(extract)


extract_low_end = getPairs(fspairs, min.weight = 40, max.weight = 50)
View(extract_low_end)

extract_other_end <- getPairs(fspairs, min.weight = 50, max.weight = 80)

extract_experiment <- getPairs(fspairs, min.weight = 50, max.weight = 80, filter.match = c("match"))

# take a look (figure out your threshold(s))

nrow(fspairs$data)
nrow(fspairs$pairs)
View(extract)
View(fspairs$pairs)
View(fspairs$data)
View(wvs_dmg_prsd)
View(extract_other_end) #Run this in front of Aaron

#There is an ID column which is a separate column that specifies their ID in the system.
#Do we group by the ID?
#The pairs find the rows they are in by ID of row and we have the weights for it.
#It has specific pairs in the top end but, it doesn't seem to have pairs at the bottom end.
#The pairs appears to have matched up on actual User_ID too!
#In my table up to 18,000 rows do not have scores!
#It doesn't provide us as pairs in the tail end, so what do we group by?
#####
#####THE END!!
#####



#If it's only a single line, then, it has no pairiing but itself? It's impossible to be itself from the fspairs$pairs comparison
#It only returns as a single row. If there are no matches.
#We want to investigate if it's only for that row..
#Inquiry



fspairs$pairs[fspairs$pairs$id1 == "13",]


#You have to take second column of ID by majority rule!
#How does one write code for it?






sum(fspairs$pairs[fspairs$pairs$id1 == "12",]$is_match) #1
sum(fspairs$pairs[fspairs$pairs$id1 == "13",]$is_match) #0

#The Other end of the spectrum:#AAAROONNNNNNNNNNNN!!!
extract$NWeight <- as.numeric(as.character(extract$Weight))
extract[extract$NWeight > 50 | extract$NWeight <52,]
extract[as.numeric(as.character(extract$Weight)) >= 50 & as.numeric(as.character(extract$Weight)) <= 52,]

#After checking out the extract from the bottom end:
#Why are we listing the ones that do not have a match?
sum(fspairs$pairs[fspairs$pairs$id1 == "47872",]$is_match)
#This has a match.
sum(fspairs$pairs[fspairs$pairs$id1 == "57195",]$is_match)

test13 <- test[test$id1 == "13",]
apply(test13[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8
min(apply(test13[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8)
max(apply(test13[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8)

#Do Column by Colum calculation # Find Minium of each of the column

#What is Minimum Jarowinkler value that is taken as part of the pairs. for the extract comparison. That's the cut off point we want.
#So for each column the jarowinkler value goes as low as 0
apply(pairs$pairs[,3:10], 2, function(x) min(x, na.rm = TRUE) )
#Across the rows, the minium jarowinkler value is
min(apply(pairs$pairs[,3:10], 1,function(x) sum(x, na.rm = TRUE))/8)

#Dplyr and try to figure out the minimum cut off point you take for each of the

# ?
min_weight <- extract
names(min_weight)
colnames(min_weight)[2] <- "id2"
min_weight <- min_weight %>% group_by(id) %>%
  summarize(results = min(NWeight, na.rm = TRUE))

min(min_weight$results, na.rm = TRUE)

#I WILL CREATE A SAMPLE DATA SET with my name and run Jarowinkler




#TO DO! TO DO! TO DO!
#In the ID column: which ID is the original ID column of the data set? The first or the second one?
#Check by calling the actual data set's ID? Later



##
##It's really fast in comparsion to emWeights
## Better than EM scheme
##
## How would we know what the threshold is?
## What is the threshold? Extreme Value Theory: Talked
## about in one of the vignettes in the package
## thrshld = Pareto threshold.. Plot for you that isinteractive
## It's compute the threshold...
## Supposed to be obvious what you click on
## Can we use the epi weights approach and get to the same results?
## Same number of results: Accept/Reject criteria
##

##EXPLORE EXTRACTS
length(extract$id)
nrow(extract)

test_id <- extract$id
test_id[1:15]



# # ## USE epiWeights instead of setting probabilities manually
# eppairs=epiWeights(pairs)  # How does this work!?
# plot(table(summary(eppairs)))
# table(eppairs)
# thrshld=getParetoThreshold(eppairs)
# cls=epiClassify(eppairs, .67)
# summary(eppairs)


# ## See if you can get acceptable results
# ## USE emWeights instead of setting probabilities manually -- VERY SLOWWWWW
# empairs=emWeights(pairs)  # How does this work!?
# cls=emClassify(empairs)
# summary(empairs)

# Look at the M Scores :

##FOR LOOP FOR EXTRACTS
test_run <- extract
colnames(test_run)[1] <- "row_id"
colnames(test_run)[2] <- "sys_id"
names(test_run)

View(test_run)
test_run$rows_num <- as.numeric(as.character(test_run$row_id))
match_frame <- data.frame(list())

#How many pairs there are is the number of NAs

length(which(!is.na(test_run$rows_num))) #22078
#How many pairs?
length(which(!is.na(test_run$rows_num)))/2 #22078 11039 pairs
#Create a data frame of matches

#indexes of matches

























