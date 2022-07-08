# libraries
library(tidyr)
library(janitor)
library(writexl)
library(DataExplorer)

# import excel file
crime = read.csv('crime.csv', sep = ",", na.strings = c('','NA','na','N/A','n/a','NaN','nan'), strip.white = TRUE, stringsAsFactors = FALSE)
crime = clean_names(crime)
  
# factor conversion for better Summary Results
crime$incident_number = as.factor(crime$incident_number)
crime$offense_code_group = as.factor(crime$offense_code_group)
crime$district = as.factor(crime$district)
crime$shooting = as.factor(crime$shooting)
crime$day_of_week = as.factor(crime$day_of_week)
crime$ucr_part = as.factor(crime$ucr_part)
#crime$season = as.factor(crime$season)
#crime$timeDay = as.factor(crime$timeDay)

# save as excel file
write_xlsx(crime, "raw_crime.xlsx")

# variables
colnames(crime)

# summary
sort(sapply(crime, function(x) sum(is.na(x))), decreasing = TRUE)
plot_missing(crime)
