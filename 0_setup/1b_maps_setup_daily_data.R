# MAPS SET UP DAILY AFFECT DATA
# David Pagliaccio 1/28/22 / Updated Paul Bloom 10/2022

## libraries ----
library(tidyverse)
library(hms)
library(lubridate, quietly=TRUE, warn.conflicts=FALSE)
library(scales)

# Function for converting epoch to local date/time
epoch2timestamplocal <- function(epoch, tz, milliseconds=TRUE){
    if (milliseconds) {dividend <- 1000} else{dividend <- 1}
    tz_sign = stringr::str_extract(tz, '[+-]')
    tz_hour = stringr::str_extract(tz, '[0-9]{1,2}(?=:)')
    tz_minute = stringr::str_extract(tz, '(?<=:)[0-9]{2}')
    tz_adjust = (as.numeric(tz_hour)*60*60 + as.numeric(tz_minute)*60)*dividend
    epoch_local = as.numeric(if_else(tz_sign == '+', epoch + tz_adjust, epoch - tz_adjust))
    options(digits.secs=0)
    return(as.POSIXct(as.numeric(epoch_local) / dividend, origin = '1970-01-01', tz = 'GMT')) # Removed as.integer to preserve ms
}

## read individual-level data files and aggregate ----
DAILY <- data.frame(ID=NA,id_file=NA,epoch_initiated=NA,epoch_completed=NA,daily=NA,timezone=NA,id_filepath_converted=NA,tm_local=NA,date=NA)

subjlist <- dir("/Volumes/auerbachlab/Columbia/MAPS_Data/EARS_Features/DAILY/")
for (ID in subjlist) {
    filelist <- dir(str_c("/Volumes/auerbachlab/Columbia/MAPS_Data/EARS_Features/DAILY/",ID,"/"),pattern = "DAILY_complete")
     for (f in filelist) {
         tmp <- cbind(ID,read.csv(str_c("/Volumes/auerbachlab/Columbia/MAPS_Data/EARS_Features/DAILY/",ID,"/",f)))
         DAILY <- rbind(DAILY,setNames(tmp, names(DAILY)))
    }
}

# code timings
DAILY = DAILY %>%
    mutate(datetime_initiated = epoch2timestamplocal(epoch = epoch_initiated, tz = timezone),
           datetime_completed = epoch2timestamplocal(epoch = epoch_completed, tz = timezone))


## format dates ----
DAILYdata <- DAILY[,c("ID","date",'datetime_initiated', 'datetime_completed', "daily")]
DAILYdata$date <- as.Date(DAILYdata$date, "%Y-%m-%d",tz = "EST")
DAILYdata$ID <- as.factor(DAILYdata$ID)

# Calculate timing for surveys
DAILYdata = mutate(DAILYdata, time_initiated = hms::as_hms(datetime_initiated),
                               time_completed = hms::as_hms(datetime_completed),
                    survey_time = as.numeric(time_completed - time_initiated, units = 'secs'))

# filter out duplicates (multiple responses on the same day -- should remove responses after first one chronologically) ----
DAILYdata <-  DAILYdata[!duplicated(DAILYdata[,c("ID","date")]),]

# yesterday and tomorrow (rather than lag function, if missing days...)
lag  <- DAILYdata[,c("ID","date","daily")] %>% rename(daily_lag1=daily)  %>% mutate(date=date+1)
lead <- DAILYdata[,c("ID","date","daily")] %>% rename(daily_lead1=daily) %>% mutate(date=date-1)

# labl time information
DAILYdata <- merge(DAILYdata,lag, by=c("ID","date"),all = T)
DAILYdata <- merge(DAILYdata,lead,by=c("ID","date"),all = T)
DAILYdata$dayofweek <- weekdays(DAILYdata$date)
DAILYdata$weekend <- as.factor(ifelse(DAILYdata$dayofweek == "Saturday" | DAILYdata$dayofweek =="Sunday",1,0))
DAILYdata$weekedndchangelag <- as.factor(ifelse(DAILYdata$dayofweek == "Saturday" | DAILYdata$dayofweek =="Monday",1,0))
DAILYdata$month <- as.numeric(format(DAILYdata$date,"%m"))
DAILYdata$summerbreak <- as.factor(ifelse(DAILYdata$month == 7 | DAILYdata$month == 8,1,0))

## save out
save(file = "../../cleaned_data/daily_data.rda", list=c("DAILYdata"))
rm(DAILY, lag,lead,tmp)
