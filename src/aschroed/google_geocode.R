library(ggmap)

register_google(key = "AIzaSyBMF5AKQW6-RtwI64N1eXi3vNyLVUUZwbM")

# connect to database
setwd("~/sdal/projects/census/analysis/aschroed")
source(file="pg_connect.R")

# example pulling addresses from MLS database - build and run query to get listing address info
listingAddresses <- dbGetQuery(con, "SELECT list_number, st_number, st_direction,
                                     street_name, street_suffix, city, state, zip_code
                                     FROM housing.w_mls_location_data limit 5")

# get geocoding of each address and add column to end of listingAddresses
listingAddresses$geo <- lapply(paste(listingAddresses$st_number,
                                     listingAddresses$st_direction,
                                     listingAddresses$street_name,
                                     listingAddresses$street_suffix,
                                     listingAddresses$city,
                                     listingAddresses$state,
                                     listingAddresses$zip_code,
                                     sep=" "),
                               geocode,
                               output = 'more',
                               source = 'google')

print(listingAddresses$geo)