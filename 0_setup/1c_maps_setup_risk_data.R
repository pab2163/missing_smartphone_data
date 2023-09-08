## Set up MAPS risk data (weekly suicide EMA)
# Paul Bloom
# 9/19/2022


## libraries ----
library(tidyverse)
library(testit)

# glob all risk files into a vector
risk_files = Sys.glob('/Volumes/AUERBACHLAB/Columbia/MAPS_Data/EARS_Features/RISK/*/RISK_day_*')

# remove "converted" risk_files
#risk_files = risk_files[!grepl('converted', risk_files)]

# little function to pull the participant ID from the file path to the  file
get_id = function(path){ 
  id = stringr::str_split(path, pattern = '/')[[1]][8] %>% 
    as.character()
  return(id)
}

# get a vector of ids
ids = sapply(risk_files, get_id)

# check that length of id vector matches length of files vector
assert('IDs vector length matches risk file path vector length', length(ids) == length(risk_files))

# pull all risk files into 1 dataframe (long), and label with IDs
combined_risk = risk_files %>% 
  map_dfr(read_csv, .id = 'source') %>%
  mutate(ID = ids[as.numeric(source)])

# Pull in start/end dates
dates = read_csv('/Volumes/auerbachlab/Columbia/MAPS_Data/Analysis/Wrangling/MAPS_StartEnd_Dates/maps_startend_dates_2.13.2023.csv') %>%
  mutate(ID = as.character(ID)) %>%
  dplyr::select(ID, participant_start_date = start_date, participant_end_date = end_date, TIMEPNT) 

# Set up grid of dates
date_grid = expand.grid(ID = unique(combined_risk$ID),
                        date = min(dates$participant_start_date):max(dates$participant_end_date)) %>%
  mutate(date = as.Date(date, origin = '1970-01-01'))

# Filter dates by particiapnt-specific start/end dates
# weeks start on wednesdays here (since almost all suicide risk surveys were sent Weds)
dates = dplyr::left_join(date_grid, dates, by = 'ID') %>%
  dplyr::filter(date >= participant_start_date, 
                date <= participant_end_date) %>%
  mutate(week = floor_date(date, unit = 'week',
                           week_start = getOption("lubridate.week.start", 3))) 
  
# put observed responses in grid of when responses COULD have been obvserved
# between start/end dates for each participant
combined_risk = dplyr::rename(combined_risk, date = dt_local)
risk_weekly = left_join(dates, combined_risk, by = c('ID', 'date'))

risk_weekly$dayofweek <- weekdays(risk_weekly$date)
risk_weekly$month <- as.numeric(format(risk_weekly$date,"%m"))

# Dataset of whether risk survey was observed within each week
risk_week_present = risk_weekly %>%
  group_by(ID, week, TIMEPNT) %>%
  summarise(days = n(),
         risk_observations = sum(!is.na(riskOne)),
         risk_missing = ifelse(risk_observations==0,1,0),
         month = case_when(
           risk_observations>=1 ~ month[!is.na(riskOne)][1],
           risk_observations==0 ~ month[1])) %>%
  group_by(ID) %>%
  mutate(week_num = rank(week))

save(risk_weekly, risk_week_present, file='../../cleaned_data/risk.rda')
