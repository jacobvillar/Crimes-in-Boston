# libraries
library(tidyr)
library(dplyr)
library(writexl)

# loading raw dataset
raw_crime =  read.csv('crime.csv', sep = ",", na.strings = c('','NA','na','N/A','n/a','NaN','nan'), strip.white = TRUE, stringsAsFactors = FALSE)
raw_crime = clean_names(raw_crime)
nrow(raw_crime)

# summary of missing data per column
sort(sapply(raw_crime, function(x) sum(is.na(x))), decreasing = TRUE)
summary(raw_crime)

# drop rows with missing district
crime = raw_crime %>% drop_na(district)
sort(sapply(crime, function(x) sum(is.na(x))), decreasing = TRUE)
nrow(crime)

# derive season from months
crime = crime %>% mutate(season =
                           ifelse(month %in% c(6,7,8), "Summer",
                           ifelse(month %in% c(9,10,11), "Fall",
                           ifelse(month %in% c(12,1,2), "Winter", "Spring"))))

# derive time of the day from hour
crime = crime %>% mutate(timeDay =
                           ifelse(hour %in% c(1, 2, 3, 4, 5, 6), "Early Morning",
                           ifelse(hour %in% c(7, 8, 9, 10, 11, 12), "Morning",
                           ifelse(hour %in% c(13, 14, 15, 16, 17, 18), "Afternoon", "Evening"))))

#rename values of shooting
crime$shooting = crime$shooting %>% replace_na("No")
s_name = c(No = 'No', Y = 'Yes')
crime$shooting = as.factor(s_name[(crime$shooting)])


# rename district name
district_name = c(A1 = 'Downtown',
                  A15 = 'Charlestown',
                  A7 = 'East Boston',
                  B2 = 'Roxbury',
                  B3 = 'Mattapan',
                  C6 = 'South Boston',
                  C11 = 'Dorchester',
                  D4 = 'South End',
                  D14 = 'Brighton',
                  E5 = 'West Roxbury',
                  E13 = 'Jamaica Plain',
                  E18 = 'Hyde Park')

crime$district = as.factor(district_name[(crime$district)])

# drop unnecessary columns
crime = subset(crime,
               select = -c(offense_code, offense_description, reporting_area, occurred_on_date, street, location))
summary(crime)

# save as excel file
write_xlsx(crime, "final_crime.xlsx")