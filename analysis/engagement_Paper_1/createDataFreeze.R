rm(list=ls())
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))


source("~/dev/elevateMS_analysis/analysis/loadData.R")
FREEZE_DATE = lubridate::ymd("2019-02-28")
DAYS_SINCE_LAUNCH = as.numeric(FREEZE_DATE - lubridate::ymd("2017-08-14"))
WEEKS_SINCE_LAUNCH = ( (DAYS_SINCE_LAUNCH - 1) %/% 7) + 1

baselineChar <- baselineChar %>% filter(dataGroups %in% c('ms_patient', 'control'))
userActivity <- userActivity %>% 
  filter(createdOn_localTime <= FREEZE_DATE & 
        healthCode %in% baselineChar$healthCode)

needed <- c( 'userActivity', 'baselineChar', 'FREEZE_DATE', 'DAYS_SINCE_LAUNCH', 'WEEKS_SINCE_LAUNCH')
rm(list=ls()[!ls() %in% needed])
ls()
COL_MS_PATIENT = '#DC0000FF'
COL_CONTROL = '#00A087FF'
COL_MS_PATIENT_CLINICAL_REF = '#e66101'

save.image("elevateMS_paper1_dataFreeze.Rda")
synStore(File("elevateMS_paper1_dataFreeze.Rda",
              parentId = "syn11649013"),
         executed="https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/engagement_Paper_1/createDataFreeze.R")
unlink("elevateMS_paper1_dataFreeze.Rda")
 