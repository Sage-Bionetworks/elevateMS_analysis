rm(list=ls())
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))


source("~/dev/elevateMS_analysis/analysis/loadData.R")
FREEZE_DATE = lubridate::ymd("2017-11-17")
DAYS_SINCE_LAUNCH = as.numeric(FREEZE_DATE - lubridate::ymd("2017-08-14"))
WEEKS_SINCE_LAUNCH = ( (DAYS_SINCE_LAUNCH - 1) %/% 7) + 1



# Loads > trigger_survey df
triggers <- getTriggerData() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
dailyCheckins <- getDailyCheckin() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
relapses <- getRelapseData() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
symptoms <- getSymptomsData() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
nQOL_uppExtremity <- get_nQOL_uppExtremity()  %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
nQOL_lowExtremity  <- get_nQOL_lowExtremity() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
nQOL_cognition  <- get_nQOL_cognition() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
tapF <- getTappingF() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
walkF <- getWalkingF()  %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
restF <- getRestF() %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)

stringfy <- function(x){
  gsub('[\\[\"\\]]','',x, perl=T) 
}

userActivity <- getUserActivity() %>% filter(createdOn_localTime <= FREEZE_DATE & participant_week <= 12)
demog <- get_demographics() %>% filter(startDate <= FREEZE_DATE)

needed <- c('triggers', 'dailyCheckins', 'relapses', 'symptoms', 'nQOL_uppExtremity', 'nQOL_lowExtremity',
  'nQOL_cognition', 'tapF', 'walkF', 'restF', 'userActivity', 'demog', 'FREEZE_DATE', 'DAYS_SINCE_LAUNCH', 'WEEKS_SINCE_LAUNCH')

rm(list=ls()[!ls() %in% needed])
ls()
save.image("elevateMS_paper1_dataFreeze.Rda")
synStore(File("elevateMS_paper1_dataFreeze.Rda",
              parentId = "syn11649013"),
         executed="https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/PAPER-1/createDataFreeze.R")
unlink("elevateMS_paper1_dataFreeze.Rda")
