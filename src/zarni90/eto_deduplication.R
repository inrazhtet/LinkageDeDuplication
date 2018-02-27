library(data.table)
library(RecordLinkage)
source("src/aschroed/parse_functions.R")

## IMPORT DATA
eto <- read.csv("~/git/dhs_link_char/data/dhs_link_char/original/ETO_DHSNewDemographic.csv", header = TRUE)
## EXPLORE DATA
colnames(eto)
head(eto)
nrow(eto)
ncol(eto)
## PREPARE DATA
#  subset to the columns we want to match
#  ParticipantUniqueIdentifier, First Name, Middle Name, Last Name,
#  We dropped the middle name
eto_dmg <- eto[, c(1,3,5:16 )]
class(eto_dmg)
ncol(eto_dmg)
names(eto_dmg)
eto_dmg <- data.table(eto_dmg)
# parse dob to separate columns
eto_dmg_prsd=parse_dob_mdy(eto_dmg, "BirthDate") # 3 failed to parse
head(eto_dmg_prsd)
ncol(eto_dmg)
ncol(eto_dmg_prsd)
names(eto_dmg_prsd)
# create new column names
# QUESTION: Should the street names be combined into one column?
colnames(eto_dmg_prsd)=c("id","first_name","last_name","gender","dob","ssn","street","streetname", "streetdirection","streettype","aptnumber","zip","dhs_id",  "consent", "birth_yr","birth_mo","birth_dy")
# remove original dob column
eto_dmg_prsd$dob<-NULL
# remove invalid ssn
eto_dmg_prsd=remove_non_ssn(eto_dmg_prsd, "ssn")
# change zip to string
eto_dmg_prsd$zip <- as.character(eto_dmg_prsd$zip)
# Geolocate addresses?

## CREATE MATCH PAIRS - ??What is the JaroWinkler threshold set at??
pairs_eto=compare.dedup(eto_dmg_prsd,
                    exclude = c("id"),  # exclude fields not to be used
                    identity = eto_dmg_prsd$id,  # set to a field that tells the real linkage (if available)
                    strcmp = TRUE,  # turns on string comparison, defaults to JaroWInkler
                    blockfld = list(c("last_name", "first_name"), c("birth_yr", "birth_mo"), c("ssn")))  # Creating Blocks to Reduce Useless Processing
# take a look
summary(pairs_eto)

## SET m PROBABILITIES (u probabilities are set automatically using frequency)
names(eto_dmg_prsd)

## QUESTION: WHAT PROBABILIES SHOULD I SET? SHOULD I IGNORE
ms=c(.95,.85,.9,.9,.4,.6,.6,.6,.6,.6,.95,.4,.4,.95,.85)
## GET AND APPLY WEIGHTS FOR PARAMETRIC MATCHING
fspairs_eto=fsWeights(pairs_eto, m = ms)
# Take a look at matched-pair weight distribution
table(fspairs$Wdata)
plot(table(fspairs_eto$Wdata))
# Select a subset of pairs to study
extract_eto=getPairs(fspairs_eto, min.weight=50, max.weight=116)
# take a look (figure out your threshold(s))
View(extract_eto)

# ## USE epiWeights instead of setting probabilities manually
# eppairs=epiWeights(pairs)  # How does this work!?
# #thrshld=getParetoThreshold(eppairs)
# cls=epiClassify(eppairs, .67)
# summary(eppairs)

# ## USE emWeights instead of setting probabilities manually -- VERY SLOWWWWW
# empairs=emWeights(pairs)  # How does this work!?
# cls=emClassify(empairs)
# summary(empairs)
