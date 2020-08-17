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
FREEZE_DATE = lubridate::ymd("2019-10-30")


#####
#get cleaned demographics features
####
baselineChar <- fread(synGet('syn17115631')$path)

# GLOBAL VARS
STUDY_HEALTHCODES <- unique(baselineChar$healthCode)



#WalkF
walkD <- fread(synGet('syn10647801')$path, data.table = F) %>%
  dplyr::select(-dataGroups) %>%
  dplyr::inner_join( baselineChar %>% select(healthCode, dataGroups)) 


##Final records to be QC'd
walkD_flt <- walkD %>% 
  dplyr::filter(createdOn >= START_DATE & createdOn <= FREEZE_DATE,
                healthCode %in% STUDY_HEALTHCODES,
                dataGroups == "ms_patient")
