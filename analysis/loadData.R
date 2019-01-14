rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c( 'tidyverse', 'synapser', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', "bit64")
install_load('tm', 'SnowballC', 'wordcloud', 'RColorBrewer')
install_load("scales", "ggsci")
#show_col(pal_npg("nrc")(10))
#pal_npg("nrc")(10)
COL_MS_PATIENT = '#DC0000FF'
COL_CONTROL = '#00A087FF'
synapser::synLogin()



START_DATE = lubridate::ymd("2017-08-14")
PARENT_FOLDER = 'syn10140063'

insert_study_times <- function(df, timeStampCol, timeZoneCol, userStartDates){
  df['timeZone'] = round((df[[timeZoneCol]] ) / 100, digits=0)
  df['activity_start_timestamp_GMT'] = lubridate::as_datetime(df[[timeStampCol]]/ 1000)
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
  df %>% filter(healthCode %in% STUDY_HEALTHCODES & dataGroups %in% c('control', 'ms_patient'))
}

loadFile <- function(tableSynID){
  fread(synGet(tableSynID)$path, data.table = F) %>%
    dplyr::filter(healthCode %in% STUDY_HEALTHCODES & dataGroups %in% c('control', 'ms_patient'))
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
                  currentDate = Sys.Date()) %>%
    filter(createdOn >= START_DATE)

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




#####
#get demographics features
####
baselineChar <- loadFile('syn17115631')


######
#Trigger
######
getTriggerData <- function(){
  trigger_survey_syntable <- "syn10232189"
  df <- loadTable(trigger_survey_syntable)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol, timeZoneCol, userStartDates)
  #keep  triggers only for MS patients
  df <- df %>% filter(healthCode %in% STUDY_HEALTHCODES & dataGroups == 'ms_patient')  %>%
    dplyr::mutate(trigger = myTriggers.json.choiceAnswers,
                  activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)/1000,
                  trigger = stringfy(trigger)) %>% 
    filter(activity_start_timestamp_GMT >= START_DATE) %>%
    select(healthCode, dataGroups, recordId, activity_start_timestamp_GMT, timeZone, 
           elevateMS_startDate_GMT, activity_start_timestamp_local, activityDuration,
           participant_day, participant_week, study_day, study_week, trigger) %>%
    distinct(healthCode, activity_start_timestamp_GMT, activity_start_timestamp_local, activityDuration,
             .keep_all = T)
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
                           timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df %>% filter(healthCode %in% STUDY_HEALTHCODES)  %>%
    dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate) / 1000) %>%
    dplyr::select(-appVersion, -phoneInfo, -uploadDate, -createdOn, -validationErrors, -createdOnTimeZone, -recordId,
                  -userSharingScope, -metadata.json.startDate.timezone, -metadata.json.scheduledActivityGuid,
                  -metadata.json.endDate, -metadata.json.startDate, -metadata.json.taskRunUUID, -myRelapses.json.relapseDate,
                  -metadata.json.endDate.timezone, -metadata.json.taskIdentifier) %>%
    filter(activity_start_timestamp_GMT >= START_DATE)
}
relapses <- getRelapseData()


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
    dplyr::filter(participant_day >= 1)
}
dsst <- get_dsst_features()

getSymptomsData <- function(){
  symptoms_survey_syntable <- "syn9765702"
  df <- loadTable(symptoms_survey_syntable)
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>% filter(healthCode %in% STUDY_HEALTHCODES & symptomTiming.json.choiceAnswers != 'NA' & dataGroups == 'ms_patient' ) %>%
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
  df %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES)
}
dailyCheckins <- getDailyCheckin()


#TappingF
getTappingF <- function(){
  df <- loadFile("syn10648415")
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>%  inner_join(userStartDates) %>%
    dplyr::mutate(activity_start_timestamp_local = activity_start_timestamp_GMT + lubridate::hours(timeZone),
                  activityDuration_seconds = as.numeric(metadata.json.endDate - metadata.json.startDate)/1000) %>%
    dplyr::filter(participant_day >= 1) %>%
    dplyr::select(-c(14:33), -rawData,  -tapping_left.fileLocation.TappingSamples, 
                  -tapping_right.fileLocation.TappingSamples, -metadata.json.endDate, -metadata.json.endDate.timezone)
}
tapF <- getTappingF()

#WalkingF
getWalkingF <- function(){
  df <- loadFile("syn10647801")
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>%
    dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)/1000) %>%
    dplyr::filter(healthCode %in% STUDY_HEALTHCODES) %>%
    select(-accelerometer_walking_outbound.json.items, -deviceMotion_walking_outbound.json.items,
           -pedometer_walking_outbound.json.items, -accelerometer_walking_rest.json.items,
           -deviceMotion_walking_rest.json.items, -metadata.json.endDate, -metadata.json.endDate.timezone,
           -metadata.json.startDate, -metadata.json.startDate.timezone, -rawData,
           -userSharingScope, -validationErrors)
}
walkF <- getWalkingF()


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




#RestF
getRestF <- function(){
  df <- loadFile("syn17783299")
  timeStampCol = 'metadata.json.startDate'
  timeZoneCol = 'metadata.json.startDate.timezone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
  df <- df %>%
    dplyr::mutate(activityDuration = as.numeric(metadata.json.endDate - metadata.json.startDate)/1000) %>%
    dplyr::filter(participant_day >= 1) %>%
    dplyr::select(-c(12:25))
}
restF <- getRestF()

# getWalkingPedoF <- function(){
#   df <- fread(synGet("syn17052176")$path)
#   dim(df)
#   View(df)
#   sum(df$floorsDescended, na.rm = )
#   
#   
# }

####
# Load nQOL's and MSIS surveys 
####
get_nQOL_uppExtremity <- function(){
  df <- loadFile("syn11315757")
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
}
nQOL_uppExtremity <- get_nQOL_uppExtremity()

get_nQOL_lowExtremity <- function(){
  df <- loadFile("syn11315765")
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
}
nQOL_lowExtremity <- get_nQOL_lowExtremity()

get_nQOL_cognition <- function(){
  df <- loadFile("syn11315747")
  timeStampCol = 'createdOn'
  timeZoneCol = 'createdOnTimeZone'
  df <- insert_study_times(df, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
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


kelvin_to_fahrenheit <- function(x){
  round(((x * 9)/5) - 459.67, digits=2)
}

# #MSIS29
# getMSIS29 <- function(){
#   df <- loadFile("syn11336100")
# }

get_weatherData <- function(){
  weather_syntable <- "syn11171602"
  df <- loadTable(weather_syntable)
  colnames(df) <- gsub('weather.json.', '',colnames(df))
  df <- df %>% mutate(currentTemperature = kelvin_to_fahrenheit(currentTemperature),
                      maximumTemperature = kelvin_to_fahrenheit(maximumTemperature),
                      minimumTemperature = kelvin_to_fahrenheit(minimumTemperature))
}
weatherF <- get_weatherData()


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
