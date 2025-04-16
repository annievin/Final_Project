install.packages("dplyr")
install.packages("purrr")
install.packages("jsonlite")
library(dplyr)
library(purrr)
library(jsonlite)

library(data.table)

###TENNESSEE
# ---------------------
# Victims
# ---------------------
victim_vars <- c("INCNUM", "INCDATE", "STATE", 
                 "V40191", "V40192", "V40193",            # Sex of the victim -1 and 2 and 3 (Male = 1)
                 "V40221", "V40222", "V40223",            # Resident status of victim 1,2,3, (Resident = 1)
                 "V40231", "V40232", "V40233",            # Homicide circumstance (6 for domestic violence)
                 "V40261", "V40262", "V40263",            # Injury types (1-8 from none to unconsciousness)
                 "V40321", "V40322", "V40323",            # Relationship of victim to offender (1 - spouse, 18 - boyfriend/gf, 26 - ex-relationship)
                 "V40071", "V40072", "V40073",            # Offence type (1 - murder, 111 - rape, 112 - sodomy, 113 - sexual assault with an object, 362 - staturary rape, 641 - human trafficing)
                 "V40181", "V40182", "V40183")            # Age of victim


victims <- fread("Data/38925-0003-Data.tsv", select = victim_vars)[STATE == 41]

# ---------------------
# Offenders
# ---------------------
offender_vars <- c("INCNUM", "STATE", 
                   "V50081", "V50082", "V50083",          # Sex of offender (Male = 1) 
                   "V20201", "V20202", "V20203",          # Bias motivation (22 - anti-Catholic)
                   "V50071", "V50072", "V50073")          # Age of the offender


offenders <- fread("Data/38925-0003-Data.tsv", select = offender_vars)[STATE == 41]

#incident_vars <- c("INCNUM", "STATE", "INCDATE", 
                   "V20061", "V20062", "V4007", "V4016", "V20203")   # Offense label and category #Offence code V4007
#incidents <- fread("38925-0003-Data.tsv", select = incident_vars)[STATE == 41]


##Renaming variables
victim_vars_named <- c("INCNUM", "INCDATE", "STATE",
                       "victim_sex_1", "victim_sex_2", "victim_sex_3",
                       "victim_resident_1", "victim_resident_2", "victim_resident_3",
                       "homicide_circum_1", "homicide_circum_2", "homicide_circum_3",
                       "injury_type_1", "injury_type_2", "injury_type_3",
                       "relationship_to_offender_1", "relationship_to_offender_2", "relationship_to_offender_3",
                       "offense_type_1", "offense_type_2", "offense_type_3",
                       "victim_age_1", "victim_age_2", "victim_age_3")

setnames(victims, old = victim_vars, new = victim_vars_named)

offender_vars_named <- c("INCNUM", "STATE",
                         "offender_sex_1", "offender_sex_2", "offender_sex_3",
                         "bias_motivation_1", "bias_motivation_2", "bias_motivation_3",
                         "offender_age_1", "offender_age_2", "offender_age_3")

setnames(offenders, old = offender_vars, new = offender_vars_named)


###Merging

full_data <- merge(victims, offenders, by = "INCNUM", all.x = TRUE)

