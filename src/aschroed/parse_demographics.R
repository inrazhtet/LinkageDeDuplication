### LOAD LIBRARIES & CUSTOM FUNCTIONS ###
#install.packages("rio")
library("rio")
library("data.table")
library("gsubfn")

source("parse_functions.R")

######## GET DATA ########
abq_arrest_records <- import("/home/sdal/projects/abq_iteam/Data/APDdata/Bloomberg_2016_VATechARSCITSUM_SSN.xls")

# copy to work table
dmg_parsed <- abq_arrest_records

######## SPLIT COMBINED NAME ########
# create copy of combined name column
setDT(dmg_parsed)[, combined_name:=dmg_parsed[, Name_Lst_Fst_M]]

# move suffixes from name to separate column
dmg_parsed <- move_suffix(dmg_parsed, "combined_name", c("JR", "SR", "III", "II", "IV"))

# parse and fill new column for first name
setDT(dmg_parsed)[, last_name:=get_before_separator(combined_name), ","]

# parse and fill new column for last name
setDT(dmg_parsed)[, first_name:=get_between_separators(combined_name, ",", " ")]

# parse and fill new column for middle name
setDT(dmg_parsed)[, middle_name:=strapplyc(combined_name, ",.* (.)", simplify = TRUE)]

######## SPLIT DOB ########
# parse and fill new columns for birth year, month, day
dmg_parsed <- parse_dob_ymd(dmg_parsed, "DOB")

######## CLEAN UP ########
# remove combined name column copy
setDT(dmg_parsed)[, combined_name:=NULL]

######## DISPLAY RESULT ########
head(dmg_parsed)

