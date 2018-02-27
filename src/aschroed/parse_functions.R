# return everything before the first occurence of provided separator
get_before_separator <-  function(string = "", separator = ",") {
  trimws(substr(string, 1, regexpr(separator, string) - 1))
}

# return everything between first occurence of first separator and first occurence
# of second separator after the first separator
get_between_separators <- function(string = "", first_separator = ",", second_separator = ",") {
  trimws(substr(string, regexpr(first_separator, string) + 1, regexpr(paste0(second_separator, "|$"), string)))
}

# create separate year, month, day columns (parse_dob_ymd for ymd formatted date, parse_dob_mdy for mdy formatted date)
library(data.table)
library(lubridate)
fix_century <- function(x, year=2018) {
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}
parse_dob_ymd <- function(dt, date_field) {
  setDT(dt)[, birth_yr:=year(fix_century(ymd(dt[, get(date_field)])))]
  setDT(dt)[, birth_mo:=month(ymd(dt[, get(date_field)]))]
  setDT(dt)[, birth_dy:=day(ymd(dt[, get(date_field)]))]
}
parse_dob_mdy <- function(dt, date_field) {
  setDT(dt)[, birth_yr:=year(fix_century(mdy(dt[, get(date_field)]))) ]
  setDT(dt)[, birth_mo:=month(mdy(dt[, get(date_field)]))]
  setDT(dt)[, birth_dy:=day(mdy(dt[, get(date_field)]))]
}

# move provided suffixes to separate column
library(data.table)
move_suffix <- function(dt, name_field, suffix_list) {
  lapply(suffix_list, function(sfx) {
    field <- dt[, get(name_field)]
    setDT(dt)[field %like% paste0(" ", sfx), suffix := sfx]
    setDT(dt)[, eval(name_field):=sub(paste0(" ", sfx), "", field)]
  })
  as.data.table(dt)
}

# remove non-alphanumeric characters
remove_non_alphanum <- function(string) {
  gsub("[^[:alnum:] ]", "", string)
}

# remove non-ssn - remove dashes, non9digits and 123456789 and 999999999
remove_non_ssn <- function(dt, ssn_field) {
  dt[,get(ssn_field)]
  setDT(dt)[, eval(ssn_field):= remove_non_alphanum(get(ssn_field))]
  setDT(dt)[get(ssn_field)=="123456789" | get(ssn_field)=="999999999", eval(ssn_field):=""]
  setDT(dt)[get(ssn_field) %like% "[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]"==FALSE, eval(ssn_field):= ""]
}
