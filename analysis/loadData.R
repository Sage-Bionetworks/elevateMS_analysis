rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapser)
synapser::synLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra')
install_load('tm', 'SnowballC', 'wordcloud', 'RColorBrewer')


PARENT_FOLDER = 'syn10140063'

insert_study_times <- function(df, timeStampCol, timeZoneCol, userStartDates){
  df['timeZone'] = as.numeric(df[[timeZoneCol]] ) / 100
  df['activity_start_timestamp_GMT'] = lubridate::ymd_hms(df[[timeStampCol]])
  df<- df %>%  inner_join(userStartDates) %>%
    dplyr::mutate(activity_start_timestamp_local = activity_start_timestamp_GMT + lubridate::hours(timeZone),
                  participant_day = as.numeric(lubridate::date(activity_start_timestamp_GMT) - elevateMS_startDate_GMT ) + 1,
                  participant_week = ((participant_day - 1) %/% 7 ) + 1,
                  study_day = as.numeric(lubridate::date(activity_start_timestamp_GMT) - lubridate::ymd("2017-08-14")) + 1,
                  study_week = ((study_day - 1) %/% 7 ) + 1) %>%
    dplyr::filter(participant_day >= 1)
  df
}


loadTable <- function(tableSynID){
  df <- synTableQuery(paste("select * from", tableSynID))
  df <- df$asDataFrame()
  df %>% filter(healthCode %in% STUDY_HEALTHCODES)
}

stringfy <- function(x){
  gsub('[\\[\"\\]]','',x, perl=T) 
}

getUserActivity <- function(){
  masterTable <- "syn9758009"  #poorly named elevate-ms-appVersion
  #remove all testing users and records created before Monday, August 14, 2017 12:00:00 AM (GMT)
  userActivity <- synTableQuery(paste("select * from", masterTable, "WHERE dataGroups NOT LIKE '%test%' AND createdOn > 1502668800000"))
  userActivity <- userActivity$asDataFrame()
  
  #Data Manipulation
  userActivity <- userActivity %>% dplyr::mutate(# Ignoring the fraction timezones for issues with conversion to local times
                                                 #createdOnTimeZone = as.numeric( createdOnTimeZone %/% 100 + ((createdOnTimeZone %% 100)/60)),
                                                 createdOnTimeZone =  createdOnTimeZone %/% 100,
                                                 createdOn =  lubridate::as_datetime(createdOn / 1000, tz='UTC'),
                                                 originalTable = gsub('elevate-ms-', '', originalTable))
  
  #get start dates for people 
  userStartDates <- userActivity %>% dplyr::group_by(healthCode) %>% 
    dplyr::summarise(participantStartDate = min(lubridate::date(createdOn)))
  userActivity <- userActivity %>% inner_join(userStartDates)
  
  userActivity <- userActivity %>% 
    dplyr::mutate(createdOn_localTime = createdOn + lubridate::hours(createdOnTimeZone),
                  participant_day = as.numeric(lubridate::date(createdOn) - participantStartDate ) + 1,
                  participant_week = ((participant_day - 1) %/% 7 ) + 1,
                  study_day = as.numeric(lubridate::date(createdOn) - lubridate::ymd("2017-08-14")) + 1,
                  study_week = ((study_day - 1) %/% 7 ) + 1,
                  currentDate = Sys.Date())
}


userActivity <- getUserActivity()
#get start dates for people 
userStartDates <- userActivity %>% dplyr::group_by(healthCode) %>% 
  dplyr::summarise(elevateMS_startDate_GMT = min(lubridate::date(createdOn)))


# GLOBAL VARS
STUDY_HEALTHCODES <- unique(userActivity$healthCode)
TOTAL_USERS <- n_distinct(userActivity$healthCode)
NUM_MS_PATIENTS <- userActivity %>% dplyr::filter(dataGroups %like% 'ms_patient') %>% .$healthCode %>% n_distinct()
NUM_CONTROLS <- userActivity %>% dplyr::filter(dataGroups %like% 'control') %>% .$healthCode %>% n_distinct()


get_demographics <- function(){
  demog <-  synTableQuery(paste("select * from syn10295288"))
  demog <- demog$asDataFrame() 
  demog$ROW_ID <- NULL
  demog$ROW_VERSION <-NULL
  demog$metadata.json.dataGroups <- NULL
  colnames(demog) <- gsub('.json.answer', '',colnames(demog))
  colnames(demog)  <- gsub('metadata.json.', '',colnames(demog))
  demog <- demog %>% dplyr::select(-scheduledActivityGuid, -endDate, -endDate.timezone, -appVersion, -phoneInfo, -validationErrors, -startDate.timezone, -createdOn, -createdOnTimeZone, -recordId, -uploadDate, -externalId) %>%
    filter(dataGroups %in% c('control', 'ms_patient') & healthCode %in% STUDY_HEALTHCODES) %>%
    dplyr::mutate(startDate = as.Date(lubridate::as_datetime(startDate/1000)))
  
  demog <- ddply(.data = demog, .variables = c('healthCode'), .fun = function(x){
    x <- x %>% arrange(desc(startDate))
    x[1,]
  })
  
  race <- demog %>% tidyr::gather(race, value, 7:16) %>% dplyr::filter(value == 'true') %>% 
    dplyr::mutate(race = gsub('race.','',race)) %>% select(healthCode, race) %>% group_by(healthCode) %>%
    dplyr::summarise(race = paste(race,collapse=','))
  demog <- merge(demog, race) %>% select(-c(7:16))
  
  profiles <- synTableQuery(paste("select * from syn10235463"))$asDataFrame()
  profiles <- profiles %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES) %>%
    dplyr::transmute(healthCode = healthCode, 
                     startDate = as.Date(lubridate::as_datetime(createdOn/1000)),
                     zipcode = demographics.zipcode, age = demographics.age,
                     gender = demographics.gender, height = demographics.height,
                     weight = demographics.weight, education = demographics.education,
                     health_insurance = demographics.health_insurance,
                     employment = demographics.employment, race = demographics.race,
                     userSharingScope=NA) %>%
    dplyr::group_by(healthCode) %>%
    dplyr::summarise( startDate = unique(startDate)[1], zipcode = unique(zipcode)[1],
                      age = unique(age)[1], gender = unique(gender)[1], height = unique(height)[1],
                      weight = unique(weight)[1], education = unique(education)[1],
                      health_insurance = unique(health_insurance)[1], employment=unique(employment)[1],
                      race = unique(race)[1], userSharingScope=unique(userSharingScope)[1]) %>%
    dplyr::mutate(race = stringfy(race))
  
  #Add age to demog
  demog <- merge(demog, profiles %>% dplyr::select(healthCode, age), all.x=T)
  
  demog_derived_from_profiles <- profiles %>% dplyr::filter(!healthCode %in% demog$healthCode)
  dataGroups <- userActivity %>% select(healthCode, dataGroups)
  dataGroups <- dataGroups[!duplicated(dataGroups),]
  demog_derived_from_profiles <- merge(demog_derived_from_profiles , dataGroups, all.x=T)
  
  demog_derived_from_profiles <- demog_derived_from_profiles %>% 
    dplyr::select(healthCode, age, dataGroups, userSharingScope, gender, height,
                  weight, zipcode, education, health_insurance, employment, startDate, race)

  demog_total <- rbind(demog, demog_derived_from_profiles %>% select(colnames(demog)))
}





######
#Trigger
######
getTriggerData <- function(){
  trigger_survey_syntable <- "syn10232189"
  df <- loadTable(trigger_survey_syntable)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  #keep  triggers only for MS patients
  df <- df %>% filter(dataGroups == 'ms_patient' & healthCode %in% STUDY_HEALTHCODES) %>%
    dplyr::mutate(triggers = myTriggers.json.choiceAnswers,
                  metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate),
                  metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                  activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate),
                  triggers = stringfy(triggers)) %>% 
    select(healthCode, dataGroups, recordId, activity_start_timestamp_GMT, timeZone, elevateMS_startDate_GMT, 
           activity_start_timestamp_local, participant_day, participant_week, study_day, study_week, triggers)
}



######
#Relapse
######
getRelapseData <- function(){
  relapse_survey_syntable <- "syn9872551"
  df <- loadTable(relapse_survey_syntable)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df %>% dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)) %>%
    dplyr::select(-appVersion, -phoneInfo, -uploadDate, -createdOn, -validationErrors, -createdOnTimeZone, -recordId,
                  -userSharingScope, -metadata.json.startDate.timezone, -metadata.json.scheduledActivityGuid,
                  -metadata.json.endDate, -metadata.json.startDate, -metadata.json.taskRunUUID, -myRelapses.json.relapseDate,
                  -metadata.json.endDate.timezone, -metadata.json.taskIdentifier)
}


getSymptomsData <- function(){
  symptoms_survey_syntable <- "syn9765702"
  df <- loadTable(symptoms_survey_syntable)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>% filter(healthCode %in% STUDY_HEALTHCODES & symptomTiming.json.choiceAnswers != 'NA' &
                        dataGroups  == 'ms_patient') %>%
    dplyr::mutate(metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate),
                  metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                  activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)) %>%
    dplyr::select(-appVersion, -phoneInfo, -uploadDate, -createdOn, -createdOnTimeZone, -metadata.json.startDate.timezone,
                  -metadata.json.endDate.timezone, -metadata.json.taskRunUUID, -metadata.json.scheduledActivityGuid,
                  -metadata.json.taskIdentifier, -validationErrors, -metadata.json.endDate, -userSharingScope, -metadata.json.startDate)
  
  symptoms <- ddply(.data = df, 
                    .variables = c("recordId", "healthCode", "externalId", "dataGroups", 'timeZone',
                                   'activity_start_timestamp_GMT', 'elevateMS_startDate_GMT', 'activity_start_timestamp_local',
                                   'participant_day', 'participant_week', 'study_day', 'study_week', 'activityDuration'), 
                    .fun = function(row){ jsonlite::fromJSON(row$symptomTiming.json.choiceAnswers)
                    })
  symptoms <- symptoms %>% dplyr::mutate(symptom = gsub('symptomTiming.','', identifier)) %>%
    dplyr::select(-endDate, -startDate, -identifier)
}


######
#Daily Check-in's
######
getDailyCheckin <- function(){
  dailyCheckin_survey_syntable <- "syn9758010"
  df <- loadTable(dailyCheckin_survey_syntable)
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}


#MSIS29
getMSIS29 <- function(){
  df <- fread(synGet("syn11336100")@filePath, data.table = F)
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}


#WalkingF
getWalkingF <- function(){
  df <- fread(synGet("syn10647801")@filePath, data.table = F) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient'))
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>% 
    dplyr::mutate(metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate),
                  metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                  activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate))
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}


#RestF
getRestF <- function(){
  df <- fread(synGet("syn10669596")@filePath, data.table = F) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient'))
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>% 
    dplyr::mutate(metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate),
                  metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                  activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate))
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}



#TappingF
getTappingF <- function(){
  df <- fread(synGet("syn10648415")@filePath, data.table = F) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient'))
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>% dplyr::mutate(metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate),
                             metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                             activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate))
  df %>% select(c(-1,-2,-c(12:40)))
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}



####
#Process nQOL's and MSIS surveys 

#source("score_PRO_surveys.R")
source("~/dev/elevateMS_analysis/analysis/score_PRO_surveys.R")


get_nQOL_uppExtremity <- function(){
  df <- fread(synGet("syn11315757")@filePath, data.table = F) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient'))
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}

get_nQOL_lowExtremity <- function(){
  df <- fread(synGet("syn11315765")@filePath, data.table = F) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient'))
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}

get_nQOL_cognition <- function(){
  df <- fread(synGet("syn11315747")@filePath, data.table = F) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient'))
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}


kelvin_to_fahrenheit <- function(x){
  round(((x * 9)/5) - 459.67, digits=2)
}


get_weatherData <- function(){
  weather_syntable <- "syn11171602"
  df <- loadTable(weather_syntable)
  df <- df %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient'))
  df <- df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
  colnames(df) <- gsub('weather.json.', '',colnames(df))
  df <- df %>% mutate(currentTemperature = kelvin_to_fahrenheit(currentTemperature),
                      maximumTemperature = kelvin_to_fahrenheit(maximumTemperature),
                      minimumTemperature = kelvin_to_fahrenheit(minimumTemperature))
}

#Tremor // TBD AFTER FEATURES ARE RE-DONE
# getTremorF <- function(){
#   #Left
#   df_left <- fread(synGet("syn10701254")@filePath, data.table = F) %>%
#     dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
#     dplyr::mutate(createdOnTimeZone = as.numeric(metadata.json.endDate.timezone)/100,
#                   createdOn = lubridate::ymd_hms(metadata.json.endDate, tz='UTC'),
#                   side = 'left')
#   #Right 
#   df_right <- fread(synGet("syn10701250")@filePath, data.table = F) %>%
#     dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
#     dplyr::mutate(createdOnTimeZone = as.numeric(metadata.json.endDate.timezone)/100,
#                   createdOn = lubridate::ymd_hms(metadata.json.endDate, tz='UTC'),
#                   side = 'right')
#   
#   df <- rbind(df_left, df_right)
#   insert_study_times(df, userStartDates)
# }

