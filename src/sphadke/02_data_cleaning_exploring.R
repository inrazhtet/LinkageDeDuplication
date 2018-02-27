#########################################
#### Cleaning and exploring dhs data ####
#########################################

#### Created by: sphadke
#### Creted on: 06/26/2017
#### Last edited on: 06/27/2017


####################
#### Cleanup
####################
rm(list=ls())
gc()
set.seed(312)


####################
#### R Setup
####################
library(lubridate)
library(rio)

# Sourcing file that imports data
source('~/git/dhs_link_char/src/sphadke/01_data_import.R')


####################
#### Some cleaning
####################

## Renaming columns in webvision datasets
colnames(vision_demo) <- c("CLientIDtxt", "LastName", "FirstName",
                           "MiddleName", "Sex", "Race", "Hispanic",
                           "ethnicOrigin", "lang_code", "maritalStatus",
                           "EducationStatus", "Birth_Date", "pin_Type",
                           "pin", "Addresstype", "HouseNumber",
                           "PredirectionPrefix", "streetname", "streetType",
                           "aptno", "city", "state", "zip", "phoneType",
                           "areacode", "PhoneNo")

colnames(vision_program_data_names) <- c("CLientIDtxt", "Last_Name", "First_Name",
                                         "Date_of_birth", "clientType", "programCode",
                                         "ProgramDesc", "StartDate", "EndDate")

colnames(vision_encounters) <- c("CLientIDtxt", "Last_Name", "First_Name",
                                 "Date_of_birth", "EncounterDate", "programCode",
                                 "programCodeDesc", "SiteCode", "SiteCodeDesc")


####################
#### Code Setup
####################

summarize <- function(x){
  # x is a data frame

  print(dim(x))

  for(i in 1:ncol(x)){
    c <- x[,i]
    c[c == ""] <- NA

    # name of the column
    name <- colnames(x)[i]
    # class of the column
    class <- class(c)

    # Proportion of missing values
    miss <- round(sum(is.na(c))*100/nrow(x), digits = 2)

    # How many unique values to the variable?
    vals <- length(unique(c))
    # summary <- summary(x)
    print(c(name, class, miss, vals))
    if(vals <= 10){
      tab <- table(c)
      print(tab)
    }
  }
}


summarize(anasazi_client_roster)
summarize(anasazi_clients)
summarize(anasazi_employee_number)
summarize(anasazi_encounters)

summarize(eto_new_demographic)
summarize(eto_program)

summarize(vision_demo)
summarize(vision_encounters)
summarize(vision_program_data_names)




