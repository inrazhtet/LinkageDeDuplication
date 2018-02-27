library(data.table)
library(RecordLinkage)
source("src/aschroed/parse_functions.R")

## IMPORT DATA
wvs <- fread("data/dhs_link_char/original/wvdemo4a.csv")
eto <- read.csv("~/git/dhs_link_char/data/dhs_link_char/original/ETO_DHSNewDemographic.csv", header = TRUE)
colnames(eto)
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
extract=getPairs(fspairs, min.weight=50, max.weight=80)
# take a look (figure out your threshold(s))
View(extract)

# ## USE epiWeights instead of setting probabilities manually
# eppairs=epiWeights(pairs)  # How does this work!?
# #thrshld=getParetoThreshold(eppairs)
# cls=epiClassify(eppairs, .67)
# summary(eppairs)

# ## USE emWeights instead of setting probabilities manually -- VERY SLOWWWWW
# empairs=emWeights(pairs)  # How does this work!?
# cls=emClassify(empairs)
# summary(empairs)
