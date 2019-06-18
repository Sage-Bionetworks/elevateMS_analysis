rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c( 'tidyverse', 'synapser', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', "bit64")
install_load("scales", "ggsci")
#show_col(pal_npg("nrc")(10))
#pal_npg("nrc")(10)
COL_MS_PATIENT = '#DC0000FF'
COL_CONTROL = '#00A087FF'
synapser::synLogin()

START_DATE = lubridate::ymd("2017-08-14")
PARENT_FOLDER = 'syn10140063'

fixTimeZone <- function(x){
  x <- as.integer(x)
  mins <- x %% 100
  hours <- x %/% 100
  #total
  #hours <- hours + mins/60
  hours
}

insert_study_times <- function(df, timeStampCol, timeZoneCol, userStartDates){
  df['timeZone'] = fixTimeZone(df[[timeZoneCol]])
  df['activity_start_timestamp_GMT'] = lubridate::as_datetime(df[[timeStampCol]])
  df<- df %>%  inner_join(userStartDates) %>%
    dplyr::mutate(activity_start_timestamp_local = activity_start_timestamp_GMT + lubridate::hours(timeZone),
                  participant_day = as.numeric(lubridate::date(activity_start_timestamp_GMT) - elevateMS_startDate_GMT ) + 1,
                  participant_week = ((participant_day - 1) %/% 7 ) + 1,
                  study_day = as.numeric(lubridate::date(activity_start_timestamp_GMT) - lubridate::ymd("2017-08-14")) + 1,
                  study_week = ((study_day - 1) %/% 7 ) + 1) %>%
    dplyr::filter(participant_day >= 1)
  df
}

loadTable <- function(tableSynID, filter=T){
  df <- synTableQuery(paste("select * from", tableSynID))
  df <- df$asDataFrame()
  if(filter == T){
    df <- df %>% filter(healthCode %in% STUDY_HEALTHCODES & dataGroups %in% c('control', 'ms_patient'))  %>%
      select(-dataGroups) %>%
      inner_join( baselineChar %>% select(healthCode, dataGroups))
  }
  df
}

stringfy <- function(x){
  gsub('[\\[\"\\]]','',x, perl=T) 
}

#####
#get cleaned demographics features
####
baselineChar <- fread(synGet('syn17115631')$path)

# GLOBAL VARS
STUDY_HEALTHCODES <- unique(baselineChar$healthCode)

getUserActivity <- function(){
  masterTable <- "syn9758009"  #poorly named elevate-ms-appVersion
  #remove all testing users and records created before Monday, August 14, 2017 12:00:00 AM (GMT)
  userActivity <- synTableQuery(paste("select * from", masterTable, "WHERE dataGroups NOT LIKE '%test%' AND createdOn > 1502668800000"))
  userActivity <- userActivity$asDataFrame() 
  
  #Data Manipulation
  userActivity <- userActivity %>% 
    filter(createdOn >= START_DATE) %>%
    filter(healthCode %in% STUDY_HEALTHCODES) %>%
    select(-dataGroups) %>%
    inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::mutate(createdOnTimeZone =  fixTimeZone(createdOnTimeZone),
                  createdOn =  lubridate::as_datetime(createdOn, tz='UTC'),
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
                  currentDate = Sys.Date()) %>%
    dplyr::select(-ROW_ID, -ROW_VERSION, -recordId, -validationErrors)
}
userActivity <- getUserActivity()

#get start dates for people 
userStartDates <- userActivity %>% dplyr::group_by(healthCode) %>% 
  dplyr::summarise(elevateMS_startDate_GMT = min(lubridate::date(createdOn)))


######
#Trigger
######
getTriggerData <- function(){
  trigger_survey_syntable <- "syn10232189"
  df <- loadTable(trigger_survey_syntable) %>%
    select(-ROW_ID, -ROW_VERSION, -rawData, -phoneInfo,
           -metadata.json.scheduledActivityGuid, -metadata.json.taskRunUUID)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol, timeZoneCol, userStartDates)
  #keep  triggers only for MS patients
  df <- df %>% filter(dataGroups == 'ms_patient')  %>%
    dplyr::mutate(trigger = myTriggers.json.choiceAnswers,
                  activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)/1000,
                  trigger = stringfy(trigger)) %>% 
    filter(activity_start_timestamp_GMT >= START_DATE) %>%
    select(healthCode, dataGroups, recordId, activity_start_timestamp_GMT, timeZone, 
           elevateMS_startDate_GMT, activity_start_timestamp_local, activityDuration,
           participant_day, participant_week, study_day, study_week, trigger) %>%
    distinct(healthCode, activity_start_timestamp_GMT, activity_start_timestamp_local, activityDuration,
             .keep_all = T) %>%
    dplyr::filter(participant_day >= 1)
} 
triggers <- getTriggerData()
tmp <- function(x){
  x <- strsplit(as.character(x), ',')
  unique(unlist(x))
}
## deleting cases when same trigger is present more than once in the entry
triggers <- triggers %>% 
  mutate(trigger = purrr::map(.$trigger, tmp)) %>%
  unnest(trigger)


######
#Relapse
######
getRelapseData <- function(){
  relapse_survey_syntable <- "syn9872551"
  df <- loadTable(relapse_survey_syntable)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol,
                           timeZoneCol=timeZoneCol, userStartDates=userStartDates) %>%
    #convert miliseconds to seconds
    dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate) / 1000) %>%
    dplyr::select(-appVersion, -phoneInfo, -uploadDate, -createdOn, -validationErrors, -createdOnTimeZone, -recordId,
                  -userSharingScope, -metadata.json.startDate.timezone, -metadata.json.scheduledActivityGuid,
                  -metadata.json.endDate, -metadata.json.startDate, -metadata.json.taskRunUUID, -myRelapses.json.relapseDate,
                  -metadata.json.endDate.timezone, -metadata.json.taskIdentifier,
                  -ROW_ID, -ROW_VERSION, -rawData) %>%
    filter(activity_start_timestamp_GMT >= START_DATE) 
}
relapses <- getRelapseData()


####
#Voice-based digit substitution test 
####
get_dsst_features <- function(){
  healthCode_to_externalID <- synTableQuery("select * from syn11439398")
  healthCode_to_externalID <- healthCode_to_externalID$asDataFrame() %>%
    dplyr::select(externalId, healthCode)
  healthCode_to_externalID <- healthCode_to_externalID[!duplicated(healthCode_to_externalID),]
  
  dsstResults <- synTableQuery("select * from syn11309241")
  dsstResults <- dsstResults$asDataFrame()
  dsstResults <- merge(dsstResults, healthCode_to_externalID, all.x=T)
  dsst <- dsstResults %>%  dplyr::group_by(healthCode, blockId) %>% 
    dplyr::summarise(numDigits = n(), 
                     activtyStartTime_GMT = min(createdOn),
                     numCorrect = sum(accuracy == 'y'),
                     percentCorrect = 100 * round(numCorrect/numDigits, digits=2),
                     avgTime = mean(durationMS/1000, na.rm=T),
                     sdTime = sd(durationMS/1000, na.rm=T),
                     totalTime = sum(durationMS/1000, na.rm=T))
  intersect(colnames(dsst), colnames(userStartDates))
  df<- dsst %>%  inner_join(userStartDates) %>%
    dplyr::mutate(participant_day = as.numeric(lubridate::date(activtyStartTime_GMT) - elevateMS_startDate_GMT ) + 1,
                  participant_week = ((participant_day - 1) %/% 7 ) + 1,
                  study_day = as.numeric(lubridate::date(activtyStartTime_GMT) - lubridate::ymd("2017-08-14")) + 1,
                  study_week = ((study_day - 1) %/% 7 ) + 1) %>%
    dplyr::filter(participant_day >= 1) %>%
    inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    filter(healthCode %in% STUDY_HEALTHCODES)
}
dsst <- get_dsst_features()

#############
#Symptoms
#############
getSymptomsData <- function(){
  symptoms_survey_syntable <- "syn9765702"
  df <- loadTable(symptoms_survey_syntable)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>% filter(symptomTiming.json.choiceAnswers != 'NA' & dataGroups == 'ms_patient' ) %>%
    dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)/1000) %>%
    dplyr::select(-appVersion, -phoneInfo, -uploadDate, -createdOn, -createdOnTimeZone, -metadata.json.startDate.timezone,
                  -metadata.json.endDate.timezone, -metadata.json.taskRunUUID, -metadata.json.scheduledActivityGuid,
                  -metadata.json.taskIdentifier, -validationErrors, -metadata.json.endDate, -userSharingScope, -metadata.json.startDate)
  df <- df %>% distinct(recordId, healthCode, externalId, dataGroups, activity_start_timestamp_GMT,
                  participant_day, participant_week, study_day, study_week, activityDuration, .keep_all = T)

  tmp_process_json <- function(row){
    x <- row$symptomTiming.json.choiceAnswers
    tryCatch({ 
      df <- jsonlite::fromJSON(x)
      df %>% distinct()
      }, error = function(x){
        print(row)
        stop('error')
        }
    )
  }
  symptoms <- plyr::ddply(.data = df,
                    .variables = c("recordId", "healthCode", "externalId", "dataGroups", 'timeZone',
                                   'activity_start_timestamp_GMT', 'elevateMS_startDate_GMT', 'activity_start_timestamp_local',
                                   'participant_day', 'participant_week', 'study_day', 'study_week', 'activityDuration'),
                    .fun = tmp_process_json
                    )
  symptoms <- symptoms %>% 
    dplyr::mutate(symptom = gsub('symptomTiming.','', identifier)) %>%
    dplyr::select(-endDate, -startDate, -identifier) %>%
    dplyr::distinct(healthCode, activity_start_timestamp_GMT, elevateMS_startDate_GMT,
                    activity_start_timestamp_local, time_of_day, severity, symptom, .keep_all=T )
}
symptoms <- getSymptomsData()

######
#Daily Check-in's
######
getDailyCheckin <- function(){
  dailyCheckin_survey_syntable <- "syn9758010"
  df <- loadTable(dailyCheckin_survey_syntable)
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df %>% dplyr::select(-ROW_ID, -ROW_VERSION, -createdOn, -validationErrors,
                       -createdOnTimeZone) %>%
    dplyr::filter(participant_day >= 1) %>%
    dplyr::select(-rawData)
}
dailyCheckins <- getDailyCheckin()

##########
#TappingF
##########
getTappingF <- function(){
  df <- fread(synGet('syn10648415')$path, data.table = F) %>%
    dplyr::select( -ROW_ID, -ROW_VERSION,
                  -accelerometer_tapping_right.json.items, -accelerometer_tapping_left.json.items,
                  -tapping_right.json.ButtonRectLeft, -tapping_left.json.ButtonRectLeft, 
                  -tapping_right.json.ButtonRectRight, -tapping_left.json.ButtonRectRight,
                  -tapping_right.json.TappingSamples, -tapping_left.json.startDate.timezone, -tapping_left.json.startDate,
                  -tapping_right.json.endDate, -tapping_right.json.endDate.timezone, -tapping_left.json.endDate,
                  -tapping_left.json.endDate.timezone, -tapping_right.json.startDate, -tapping_right.json.startDate.timezone,
                  -tapping_right.fileLocation.TappingSamples, -tapping_left.fileLocation.TappingSamples,
                  -tapping_left.json.TappingViewSize, -tapping_right.json.TappingViewSize, -tapping_left.json.TappingSamples)
  
  df <- df %>% filter(healthCode %in% STUDY_HEALTHCODES & dataGroups %in% c('control', 'ms_patient')) %>%
    select(-dataGroups) %>%
    inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::mutate(metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                  metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate))
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df  %>%
    dplyr::filter(participant_day >= 1) %>%
    dplyr::mutate(activity_start_timestamp_local = activity_start_timestamp_GMT + lubridate::hours(timeZone),
                  activityDuration_seconds = as.numeric(metadata.json.endDate - metadata.json.startDate)) %>%
    dplyr::select(-uploadDate, -createdOn, -validationErrors,
                  -metadata.json.scheduledActivityGuid, -metadata.json.taskRunUUID,
                  -metadata.json.startDate, -metadata.json.startDate.timezone,
                  -metadata.json.endDate, -metadata.json.endDate.timezone,
                  -metadata.json.dataGroups, -metadata.json.taskIdentifier,
                  -rawData)
}
tapF <- getTappingF()

##############
#WalkingF
##############
getWalkingF <- function(){
  df <- fread(synGet('syn10647801')$path, data.table = F) %>%
    dplyr::select(-accelerometer_walking_outbound.json.items, -deviceMotion_walking_outbound.json.items,
                  -pedometer_walking_outbound.json.items, -accelerometer_walking_rest.json.items, 
                  -deviceMotion_walking_rest.json.items, -rawData)
  df <- df %>% filter(healthCode %in% STUDY_HEALTHCODES) %>%
    dplyr:: select(-dataGroups) %>%
    dplyr::inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
    dplyr::mutate(metadata.json.startDate = lubridate::as_datetime(as.numeric(metadata.json.startDate)/1000),
                  metadata.json.endDate = lubridate::as_datetime(as.numeric(metadata.json.endDate)/1000))
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>%
    dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate))  %>%
    dplyr::select(-uploadDate, -createdOn, -validationErrors,
  -metadata.json.scheduledActivityGuid, -metadata.json.taskRunUUID,
  -metadata.json.startDate, -metadata.json.startDate.timezone,
  -metadata.json.endDate, -metadata.json.endDate.timezone,
  -metadata.json.dataGroups, -metadata.json.taskIdentifier) %>%
    dplyr::filter(participant_day >= 1)
}
walkF <- getWalkingF()


##############
#RestF
##############
getRestF <- function(){
  df <- fread(synGet('syn17783299')$path, data.table = F) %>%
  dplyr::select(-accelerometer_walking_outbound.json.items, -deviceMotion_walking_outbound.json.items, 
                  -pedometer_walking_outbound.json.items, -accelerometer_walking_rest.json.items,   
                  -deviceMotion_walking_rest.json.items)
  df <- df %>% filter(healthCode %in% STUDY_HEALTHCODES) %>%
    dplyr:: select(-dataGroups) %>%
    dplyr::inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
    dplyr::mutate(metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                  metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate))
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>%
    dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)) %>%
    dplyr::filter(participant_day >= 1) %>%
    dplyr::select(-uploadDate, -createdOn, -validationErrors,
                  -metadata.json.scheduledActivityGuid, -metadata.json.taskRunUUID,
                  -metadata.json.startDate, -metadata.json.startDate.timezone,
                  -metadata.json.endDate, -metadata.json.endDate.timezone,
                  -metadata.json.dataGroups, -metadata.json.taskIdentifier)
}
restF <- getRestF()

###############
#Weather
###############
kelvin_to_fahrenheit <- function(x){
  round(((x * 9)/5) - 459.67, digits=2)
}

get_weatherData <- function(){
  weather_syntable <- "syn11171602"
  df <- loadTable(weather_syntable) %>%
    dplyr:: select(-dataGroups) %>%
    dplyr::inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
    dplyr::select(-ROW_ID, -ROW_VERSION)
  colnames(df) <- gsub('weather.json.', '',colnames(df))
  
  df <- df %>% 
    dplyr::mutate(currentTemperature = kelvin_to_fahrenheit(currentTemperature),
                  maximumTemperature = kelvin_to_fahrenheit(maximumTemperature),
                  minimumTemperature = kelvin_to_fahrenheit(minimumTemperature),
                  metadata.json.startDate = lubridate::ymd_hms(metadata.json.startDate),
                  metadata.json.endDate = lubridate::ymd_hms(metadata.json.endDate)) %>%
    dplyr::select(-weather.code, -weather.detail, -rawData, -validationErrors, -createdOn,
                  -metadata.json.scheduledActivityGuid, -uploadDate, -createdOnTimeZone,
                  -metadata.json.taskRunUUID,-metadata.json.startDate, -metadata.json.startDate.timezone,
                  -metadata.json.endDate, -metadata.json.endDate.timezone, -metadata.json.scheduledOn,
                  -metadata.json.scheduledOn.timezone, -metadata.json.activityLabel, -metadata.json.dataGroups, 
                  -metadata.json.scheduleIdentifier, -recordId) %>%
    distinct()
}
weatherF <- get_weatherData()


####
# Load nQOL's and MSIS surveys 
####
get_nQOL_uppExtremity <- function(){
  df <- fread(synGet('syn11315757')$path, data.table = F) %>%
    dplyr:: select(-dataGroups) %>%
    dplyr::inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
    dplyr::mutate(createdOn)
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates) %>%
    dplyr::select(-rawData)
}
nQOL_uppExtremity <- get_nQOL_uppExtremity()


get_nQOL_lowExtremity <- function(){
  df <- fread(synGet('syn11315765')$path, data.table = F) %>%
    dplyr:: select(-dataGroups) %>%
    dplyr::inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
    dplyr::mutate(createdOn)
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates) %>%
    dplyr::select(-rawData)
}
nQOL_lowExtremity <- get_nQOL_lowExtremity()


get_nQOL_cognition <- function(){
  df <- fread(synGet('syn11315747')$path, data.table = F) %>%
    dplyr:: select(-dataGroups) %>%
    dplyr::inner_join( baselineChar %>% select(healthCode, dataGroups)) %>%
    dplyr::filter(dataGroups %in% c('control', 'ms_patient')) %>%
    dplyr::mutate(createdOn)
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates) %>%
    dplyr::select(-rawData)
}
nQOL_cognition <- get_nQOL_cognition()

######
# Keeping NeuroQoL weekly avg
######
nQOL_uppExtremity_week_avg <- nQOL_uppExtremity %>%  dplyr::group_by(healthCode, participant_week) %>%
  dplyr::summarise(TScore = mean(TScore, na.rm = T))

nQOL_lowExtremity_week_avg <- nQOL_lowExtremity %>%   dplyr::group_by(healthCode, participant_week) %>%
  dplyr::summarise(TScore = mean(TScore, na.rm = T))

nQOL_cognition_week_avg <- nQOL_cognition %>%  dplyr::group_by(healthCode, participant_week) %>%
  dplyr::summarise(TScore = mean(TScore, na.rm = T))





#   fixTimeZone <- function(x){
#     x <- as.integer(x)
#     mins <- x %% 100
#     hours <- x %/% 100
#     #total
#     #hours <- hours + mins/60
#     hours
#   }
#   
#   df$createdOn
#   
#   fixTimeZone(df$createdOnTimeZone)
#   lubridate::as_datetime(df$createdOn)
#   
#   df['timeZone'] = fixTimeZone(df[[timeZoneCol]])
#   df['activity_start_timestamp_GMT'] = lubridate::as_datetime(df[[timeStampCol]])
#   df<- df %>%  inner_join(userStartDates) %>%
#     dplyr::mutate(activity_start_timestamp_local = activity_start_timestamp_GMT + lubridate::hours(timeZone),
#                   participant_day = as.numeric(lubridate::date(activity_start_timestamp_GMT) - elevateMS_startDate_GMT ) + 1,
#                   participant_week = ((participant_day - 1) %/% 7 ) + 1,
#                   study_day = as.numeric(lubridate::date(activity_start_timestamp_GMT) - lubridate::ymd("2017-08-14")) + 1,
#                   study_week = ((study_day - 1) %/% 7 ) + 1) %>%
#     dplyr::filter(participant_day >= 1)
#   df



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


# getWalkingF_new <- function(){
#   df <- loadFile("syn17093339")
#   timeStampCol = 'metadata.json.startDate'
#   timeZoneCol = 'metadata.json.startDate.timezone'
#   df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
#   df <- df %>%
#     dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)/1000) %>%
#     dplyr::filter(healthCode %in% STUDY_HEALTHCODES) %>%
#     select(-c(1:6, 14:28))
# }
# walkF_new <- getWalkingF_new()

# #MSIS29
# getMSIS29 <- function(){
#   df <- loadFile("syn11336100")
# }



