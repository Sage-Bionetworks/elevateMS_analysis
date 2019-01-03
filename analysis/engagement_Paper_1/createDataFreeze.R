rm(list=ls())
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))


source("~/dev/elevateMS_analysis/analysis/loadData.R")
FREEZE_DATE = lubridate::ymd("2019-01-05")
DAYS_SINCE_LAUNCH = as.numeric(FREEZE_DATE - lubridate::ymd("2017-08-14"))
WEEKS_SINCE_LAUNCH = ( (DAYS_SINCE_LAUNCH - 1) %/% 7) + 1


# Loads > trigger_survey df
triggers <- triggers %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
relapses <- relapses %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
dsst <- dsst %>% filter(activtyStartTime_GMT <= FREEZE_DATE & participant_week <= 12)
symptoms <- symptoms %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
dailyCheckins <-  dailyCheckins %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
tapF <- tapF %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
nQOL_uppExtremity <- nQOL_uppExtremity  %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
nQOL_lowExtremity  <- nQOL_lowExtremity %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
nQOL_cognition  <- nQOL_cognition %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
walkF <- walkF  %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
restF <- restF %>% filter(activity_start_timestamp_local <= FREEZE_DATE & participant_week <= 12)
userActivity <- userActivity %>% filter(createdOn_localTime <= FREEZE_DATE & participant_week <= 12)
baselineChar <- baselineChar %>% filter(healthCode %in% unique(userActivity$healthCode))


ls()
needed <- c('triggers', 'dailyCheckins', 'relapses', 'symptoms', 'nQOL_uppExtremity', 'nQOL_lowExtremity', 'dsst',
  'nQOL_cognition', 'tapF', 'walkF', 'restF', 'userActivity', 'baselineChar', 'FREEZE_DATE', 'DAYS_SINCE_LAUNCH', 'WEEKS_SINCE_LAUNCH')

rm(list=ls()[!ls() %in% needed])
ls()
save.image("elevateMS_paper1_dataFreeze.Rda")
synStore(File("elevateMS_paper1_dataFreeze.Rda",
              parentId = "syn11649013"),
         executed="")
unlink("elevateMS_paper1_dataFreeze.Rda")
