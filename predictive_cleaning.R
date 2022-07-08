# libraries
library(tidyr)
library(dplyr)
library(writexl)
library(janitor)

# loading raw dataset
crime =  read.csv('crime.csv', sep = ",", na.strings = c('','NA','na','N/A','n/a','NaN','nan'), strip.white = TRUE, stringsAsFactors = FALSE)
crime = clean_names(crime)
nrow(crime)

# summary of missing data per column
sort(sapply(crime, function(x) sum(is.na(x))), decreasing = TRUE)
summary(crime)

# # drop rows with missing district
# crime = raw_crime %>% drop_na(district)
# sort(sapply(crime, function(x) sum(is.na(x))), decreasing = TRUE)
# nrow(crime)

# derive season from months
crime = crime %>% mutate(season =
                           ifelse(month %in% c(6,7,8), "Summer",
                                  ifelse(month %in% c(9,10,11), "Fall",
                                         ifelse(month %in% c(12,1,2), "Winter", "Spring"))))

# derive time of the day from hour
crime = crime %>% mutate(timeday =
                           ifelse(hour %in% c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), "Day-Time", "Night-Time"))

#derive weekday/weekend
crime = crime %>%
  mutate(dayweek = ifelse(day_of_week %in% c("Sunday", "Saturday"), "Weekend", "Weekday"))

# #rename values of shooting
# crime$shooting = crime$shooting %>% replace_na("No")
# s_name = c(No = 'No', Y = 'Yes')
# crime$shooting = as.factor(s_name[(crime$shooting)])


# # rename district name
# district_name = c(A1 = 'Downtown',
#                   A15 = 'Charlestown',
#                   A7 = 'East Boston',
#                   B2 = 'Roxbury',
#                   B3 = 'Mattapan',
#                   C6 = 'South Boston',
#                   C11 = 'Dorchester',
#                   D4 = 'South End',
#                   D14 = 'Brighton',
#                   E5 = 'West Roxbury',
#                   E13 = 'Jamaica Plain',
#                   E18 = 'Hyde Park')
# 
# crime$district = as.factor(district_name[(crime$district)])

# drop unnecessary columns
colnames(crime)
raw_crime = subset(crime,
               select = c(incident_number, offense_code_group, season, timeday, dayweek))
summary(raw_crime)
View(raw_crime)

#identify felony/misdemeanour
felony = c(
  "Larceny",
  "Drug Violation",
  "Vandalism",
  "Aggravated Assault",
  "Property Lost",
  "Larceny From Motor Vehicle",
  "Fraud",
  "Residential Burglary",
  "Auto Theft",
  "Homicide",
  "Arson",
  "Manslaughter",
  "HUMAN TRAFFICKING",
  "Burglary - No Property Taken",
  "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE"
)
misdemeanour = c(
  "Simple Assault",
  "Verbal Disputes",
  "Towed",
  "Robbery",
  "Harassment",
  "Disorderly Conduct",
  "Motor Vehicle Accident Response",
  "Firearm Violations",
  "License Violation",
  "Restraining Order Violations",
  "Counterfeiting",
  "Liquor Violation",
  "Landlord/Tenant Disputes",
  "Assembly or Gathering Violations",
  "Prostitution",
  "Criminal Harassment",
  "HOME INVASION",
  "Bomb Hoax",
  "Phone Call Complaints",
  "Biological Threat"
)

#View(sort(table(raw_crime$offense_code_group)))

#derive dependent variable
raw_crime = raw_crime %>%
  mutate(offense_type = ifelse(offense_code_group %in% felony, "Felony",
                               ifelse(offense_code_group %in% misdemeanour, "Misdemeanour", "Vague")))

#drop vague
raw_crime = subset(raw_crime, offense_type != "Vague")

#convert to codes
y_name = c(Felony = 1, Misdemeanour = 0)
raw_crime$offense_type = as.factor(y_name[(raw_crime$offense_type)])

season_name = c(Winter = 0, Spring = 1, Summer = 2, Fall = 3)
raw_crime$season = as.factor(season_name[(raw_crime$season)])

timeday_name = c('Day-Time' = 1, 'Night-Time' = 0)
raw_crime$timeday = as.factor(timeday_name[(raw_crime$timeday)])

dayweek_name = c(Weekend = 1, Weekday = 0)
raw_crime$dayweek = as.factor(dayweek_name[(raw_crime$dayweek)])

# save as excel file
write_xlsx(raw_crime, "task3_final_crime.xlsx")
