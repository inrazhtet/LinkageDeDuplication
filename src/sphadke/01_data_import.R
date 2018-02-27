############################
#### Importing dhs data ####
############################

#### Created by: sphadke
#### Creted on: 06/26/2017
#### Last edited on: 06/26/2017


####################
#### Cleanup
####################
rm(list=ls())
gc()
set.seed(312)


####################
#### R Setup
####################
library(rio)


####################
#### Import each dataset
####################

## Currently, only the main tables imported
## Other import lines commented out

## Anasazi tables
anasazi_clients <- import('~/git/dhs_link_char/data/dhs_link_char/original/AnasaziClients.csv')

anasazi_client_roster <- import('~/git/dhs_link_char/data/dhs_link_char/original/tblAnasaziClientRosterReport.csv')

anasazi_encounters <- import('~/git/dhs_link_char/data/dhs_link_char/original/tblAnasaziEncounters.csv')

anasazi_employee_number <- import('~/git/dhs_link_char/data/dhs_link_char/original/tblAnasaziStaffEmployeeNumber.csv')


## ETO tables
eto_new_demographic <- read.csv('~/git/dhs_link_char/data/dhs_link_char/original/ETO_DHSNewDemographic.csv', header = TRUE, fill = TRUE, skipNul = TRUE)
# table(eto_new_demographic$Gender)

eto_program <- read.csv('~/git/dhs_link_char/data/dhs_link_char/original/ETO_DHSprogram.csv', stringsAsFactors = FALSE)


## Webvision tables
vision_encounters <- read.table('~/git/dhs_link_char/data/dhs_link_char/original/VisionSiteEncountersName.txt', sep = "|", stringsAsFactors = FALSE, fill = TRUE, quote = NULL)

# vision_program_data_names <- read.table('~/git/dhs_link_char/data/dhs_link_char/original/webvisionProgramDataNames.txt', sep = "|", stringsAsFactors = FALSE, fill = TRUE, quote = NULL)

vision_demo <- read.table('~/git/dhs_link_char/data/dhs_link_char/original/wvdemo4a.txt', sep = "|", stringsAsFactors = FALSE, fill = TRUE, quote = NULL)


