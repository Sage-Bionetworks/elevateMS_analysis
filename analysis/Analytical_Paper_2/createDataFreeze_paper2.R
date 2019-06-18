rm(list=ls())
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))

source("~/dev/elevateMS_analysis/analysis/loadData.R")
FREEZE_DATE = lubridate::ymd("2019-05-31")
DAYS_SINCE_LAUNCH = as.numeric(FREEZE_DATE - lubridate::ymd("2017-08-14"))
WEEKS_SINCE_LAUNCH = ( (DAYS_SINCE_LAUNCH - 1) %/% 7) + 1


#baselineChar - no need to filter

userActivity <- userActivity %>% filter(createdOn_localTime <= FREEZE_DATE)
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

needed <- c('triggers', 'dailyCheckins', 'relapses',
            'symptoms', 'nQOL_uppExtremity', 
            'nQOL_lowExtremity', 'dsst','nQOL_cognition',
            'tapF', 'walkF', 'restF', 'userActivity', 
            'baselineChar', 'FREEZE_DATE', 
            'DAYS_SINCE_LAUNCH', 'WEEKS_SINCE_LAUNCH')

rm(list=ls()[!ls() %in% needed])
ls()
COL_MS_PATIENT = '#DC0000FF'
COL_CONTROL = '#00A087FF'
COL_MS_PATIENT_CLINICAL_REF = '#e66101'

save.image("elevateMS_paper2_dataFreeze.Rda")
synStore(File("elevateMS_paper2_dataFreeze.Rda",
              parentId = "syn11649013"),
         executed="https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/Analytical_Paper_2/createDataFreeze_paper2.R")
unlink("elevateMS_paper2_dataFreeze.Rda")
 