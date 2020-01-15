rm(list=ls())
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))

source("~/dev/elevateMS_analysis/analysis/loadData.R")
FREEZE_DATE = lubridate::ymd("2019-10-30")
DAYS_SINCE_LAUNCH = as.numeric(FREEZE_DATE - lubridate::ymd("2017-08-14"))
WEEKS_SINCE_LAUNCH = ( (DAYS_SINCE_LAUNCH - 1) %/% 7) + 1


#baselineChar - no need to filter
userActivity <- userActivity %>% filter(createdOn_localTime <= FREEZE_DATE)
triggers <- triggers %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
relapses <- relapses %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
dsst <- dsst %>% filter(activtyStartTime_GMT <= FREEZE_DATE)
symptoms <- symptoms %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
dailyCheckins <-  dailyCheckins %>% filter(activity_start_timestamp_local <= FREEZE_DATE )
tapF <- tapF %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
nQOL_uppExtremity <- nQOL_uppExtremity  %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
nQOL_lowExtremity  <- nQOL_lowExtremity %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
nQOL_cognition  <- nQOL_cognition %>% filter(activity_start_timestamp_local <= FREEZE_DATE )
walkF <- walkF  %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
restF <- restF %>% filter(activity_start_timestamp_local <= FREEZE_DATE)
tremorF <- tremorF %>% filter(activity_start_timestamp_local <= FREEZE_DATE)


### Following data tables are not filtered as they have value over the study period
#nQOL_uppExtremity_week_avg
#nQOL_lowExtremity_week_avg
#nQOL_cognition_week_avg
#weatherF <- weatherF - not filtered due to passive nature
#wapi_survey


##TAP FEATURE COLS -
TAP_FEATURES <- c('meanTapInter', 'medianTapInter', 'iqrTapInter', 'minTapInter', 'maxTapInter', 
                  'skewTapInter', 'kurTapInter', 'sdTapInter', 'madTapInter', 'cvTapInter', 'rangeTapInter', 
                  'tkeoTapInter', 'ar1TapInter', 'ar2TapInter', 'fatigue10TapInter', 'fatigue25TapInter', 
                  'fatigue50TapInter', 'meanDriftLeft', 'medianDriftLeft', 'iqrDriftLeft', 'minDriftLeft', 
                  'maxDriftLeft', 'skewDriftLeft', 'kurDriftLeft', 'sdDriftLeft', 'madDriftLeft', 
                  'cvDriftLeft', 'rangeDriftLeft', 'meanDriftRight', 'medianDriftRight', 'iqrDriftRight', 
                  'minDriftRight', 'maxDriftRight', 'skewDriftRight', 'kurDriftRight', 'sdDriftRight',
                  'madDriftRight', 'cvDriftRight', 'rangeDriftRight', 'numberTaps', 'buttonNoneFreq', 'corXY')


##TREMOR FEATURE COLS -
TREMOR_FEATURES <- c('cent.fr.IMF1.iqr_ud_accelerometer', 'cent.fr.IMF2.iqr_uad_gyroscope', 'cent.fr.IMF2.iqr_ud_accelerometer',
                     'cent.fr.IMF2.md_uad_gyroscope', 'dfa.tm.IMF1.iqr_uaacf_accelerometer', 'dfa.tm.IMF1.iqr_uavacf_gyroscope', 
                     'dfa.tm.IMF1.md_uaacf_accelerometer', 'dfa.tm.IMF1.md_ud_accelerometer', 'dfa.tm.IMF2.iqr_uaacf_accelerometer', 
                     'dfa.tm.IMF2.iqr_uad_gyroscope', 'dfa.tm.IMF2.iqr_uv_accelerometer', 'dfa.tm.IMF2.md_uad_gyroscope', 
                     'energy.tm.IMF1.iqr_uaa_gyroscope', 'energy.tm.IMF1.iqr_uaacf_accelerometer', 'energy.tm.IMF1.iqr_uavacf_gyroscope',
                     'energy.tm.IMF1.iqr_uj_accelerometer', 'energy.tm.IMF1.md_uj_accelerometer', 'energy.tm.IMF2.iqr_ua_accelerometer', 
                     'energy.tm.IMF2.iqr_uavacf_gyroscope', 'energy.tm.IMF2.iqr_uj_accelerometer', 'energy.tm.IMF2.md_ua_accelerometer', 
                     'energy.tm.IMF2.md_ud_accelerometer', 'ewt.permEnt.fr.IMF1.iqr_ua_accelerometer', 
                     'ewt.permEnt.fr.IMF1.iqr_uaa_gyroscope', 'ewt.permEnt.fr.IMF1.iqr_uavacf_gyroscope', 
                     'ewt.permEnt.fr.IMF1.md_uaa_gyroscope', 'ewt.permEnt.fr.IMF1.md_uaacf_accelerometer', 
                     'ewt.permEnt.fr.IMF1.md_uad_gyroscope', 'ewt.permEnt.fr.IMF1.md_uav_gyroscope',
                     'ewt.permEnt.fr.IMF2.iqr_ua_accelerometer', 'ewt.permEnt.fr.IMF2.iqr_uaacf_accelerometer', 
                     'ewt.permEnt.fr.IMF2.iqr_uav_gyroscope', 'ewt.permEnt.fr.IMF2.iqr_uavacf_gyroscope', 
                     'ewt.permEnt.fr.IMF2.iqr_uv_accelerometer', 'ewt.permEnt.fr.IMF2.md_uad_gyroscope', 
                     'ewt.permEnt.fr.IMF2.md_ud_accelerometer', 'ewt.tsallisEnt.fr.IMF1.iqr_ua_accelerometer', 
                     'ewt.tsallisEnt.fr.IMF1.iqr_uaa_gyroscope', 'ewt.tsallisEnt.fr.IMF1.md_uaacf_accelerometer', 
                     'ewt.tsallisEnt.fr.IMF2.iqr_ua_accelerometer', 'ewt.tsallisEnt.fr.IMF2.iqr_uav_gyroscope', 
                     'IQR.fr.IMF1.iqr_uavacf_gyroscope', 'IQR.fr.IMF2.iqr_uad_gyroscope', 'IQR.fr.IMF2.md_ud_accelerometer', 
                     'IQR.tm.IMF2.iqr_uaa_gyroscope', 'kurt.fr.IMF1.iqr_uaa_gyroscope', 'kurt.fr.IMF1.md_uj_accelerometer', 
                     'kurt.fr.IMF2.iqr_uad_gyroscope', 'kurt.fr.IMF2.iqr_uavacf_gyroscope', 'kurt.fr.IMF2.iqr_uv_accelerometer', 
                     'kurt.fr.IMF2.md_uad_gyroscope', 'kurtosis.tm.IMF1.iqr_uaa_gyroscope', 'kurtosis.tm.IMF1.iqr_ud_accelerometer',
                     'kurtosis.tm.IMF2.md_uav_gyroscope', 'md.fr.IMF2.md_ud_accelerometer', 'mean.tm.IMF1.iqr_uavacf_gyroscope', 
                     'mean.tm.IMF2.iqr_uaa_gyroscope', 'mean.tm.IMF2.md_uavacf_gyroscope', 'median.tm.IMF2.iqr_uj_accelerometer', 
                     'median.tm.IMF2.md_ud_accelerometer', 'mn.tm.IMF2.md_uavacf_gyroscope', 'mn.tm.IMF2.md_ud_accelerometer', 
                     'mtkeo.tm.IMF1.iqr_uavacf_gyroscope', 'mtkeo.tm.IMF2.iqr_uaacf_accelerometer', 'mx.fr.IMF1.md_uaa_gyroscope', 
                     'mx.tm.IMF1.iqr_uj_accelerometer', 'Q25.fr.IMF1.iqr_uavacf_gyroscope', 'Q25.fr.IMF2.iqr_uad_gyroscope',
                     'Q25.fr.IMF2.iqr_uv_accelerometer', 'Q25.tm.IMF1.md_uad_gyroscope', 'Q25.tm.IMF2.md_uad_gyroscope', 
                     'Q75.fr.IMF2.iqr_ud_accelerometer', 'Q75.tm.IMF1.md_uad_gyroscope', 'Q75.tm.IMF2.iqr_uavacf_gyroscope', 
                     'Q75.tm.IMF2.md_uad_gyroscope', 'rough.tm.IMF2.md_ua_accelerometer', 'sd.fr.IMF1.iqr_uaacf_accelerometer',
                     'sd.fr.IMF1.md_uaa_gyroscope', 'sd.fr.IMF2.iqr_uaacf_accelerometer', 'sd.fr.IMF2.iqr_uad_gyroscope',
                     'sd.fr.IMF2.iqr_uavacf_gyroscope', 'sd.fr.IMF2.md_uaacf_accelerometer', 'sfm.fr.IMF1.iqr_uad_gyroscope', 
                     'sfm.fr.IMF1.iqr_uav_gyroscope', 'sfm.fr.IMF1.iqr_uv_accelerometer', 'skewness.tm.IMF1.iqr_uaa_gyroscope',
                     'skewness.tm.IMF1.iqr_ud_accelerometer', 'skewness.tm.IMF1.iqr_uv_accelerometer', 
                     'skewness.tm.IMF1.md_ua_accelerometer', 'skewness.tm.IMF1.md_uav_gyroscope', 
                     'skewness.tm.IMF1.md_uv_accelerometer', 'skewness.tm.IMF2.iqr_ua_accelerometer', 
                     'skewness.tm.IMF2.iqr_uad_gyroscope', 'skewness.tm.IMF2.iqr_uav_gyroscope', 
                     'skewness.tm.IMF2.iqr_ud_accelerometer', 'skewness.tm.IMF2.iqr_uj_accelerometer', 
                     'skewness.tm.IMF2.iqr_uv_accelerometer', 'skewness.tm.IMF2.md_ua_accelerometer', 
                     'skewness.tm.IMF2.md_uad_gyroscope', 'skewness.tm.IMF2.md_uavacf_gyroscope')


##WALK FEATURE COLS -
WALK_FEATURES <- c('meanX', 'sdX', 'modeX', 'skewX', 'kurX', 'q1X', 'medianX', 'q3X',
                   'iqrX', 'rangeX', 'acfX', 'zcrX', 'dfaX', 'cvX', 'tkeoX', 'F0X',
                   'P0X', 'F0FX', 'P0FX', 'medianF0FX', 'sdF0FX', 'tlagX', 'meanY', 
                   'sdY', 'modeY', 'skewY', 'kurY', 'q1Y', 'medianY', 'q3Y', 'iqrY',
                   'rangeY', 'acfY', 'zcrY', 'dfaY', 'cvY', 'tkeoY', 'F0Y', 'P0Y', 
                   'F0FY', 'P0FY', 'medianF0FY', 'sdF0FY', 'tlagY', 'meanZ', 'sdZ', 
                   'modeZ', 'skewZ', 'kurZ', 'q1Z', 'medianZ', 'q3Z', 'iqrZ', 'rangeZ', 
                   'acfZ', 'zcrZ', 'dfaZ', 'cvZ', 'tkeoZ', 'F0Z', 'P0Z', 'F0FZ', 'P0FZ', 
                   'medianF0FZ', 'sdF0FZ', 'tlagZ', 'meanAA', 'sdAA', 'modeAA', 'skewAA', 
                   'kurAA', 'q1AA', 'medianAA', 'q3AA', 'iqrAA', 'rangeAA', 'acfAA', 'zcrAA',
                   'dfaAA', 'cvAA', 'tkeoAA', 'F0AA', 'P0AA', 'F0FAA', 'P0FAA', 'medianF0FAA', 
                   'sdF0FAA', 'tlagAA', 'meanAJ', 'sdAJ', 'modeAJ', 'skewAJ', 'kurAJ', 'q1AJ', 
                   'medianAJ', 'q3AJ', 'iqrAJ', 'rangeAJ', 'acfAJ', 'zcrAJ', 'dfaAJ', 'cvAJ', 
                   'tkeoAJ', 'F0AJ', 'P0AJ', 'F0FAJ', 'P0FAJ', 'medianF0FAJ', 'sdF0FAJ', 'tlagAJ',
                   'corXY', 'corXZ', 'corYZ', 'activityDuration')

##REST FEATURE COLS -
REST_FEATURES <- c('meanAA', 'sdAA', 'modeAA', 'skewAA', 'kurAA', 'q1AA', 
                   'medianAA', 'q3AA', 'iqrAA', 'rangeAA', 'acfAA', 'zcrAA', 'dfaAA',
                   'turningTime', 'postpeak', 'postpower', 'alpha', 'dVol', 'ddVol')



needed <- c('triggers', 'dailyCheckins', 'relapses', 'weatherF',
            'nQOL_uppExtremity_week_avg', 'nQOL_lowExtremity_week_avg',
            'nQOL_cognition_week_avg',
            'symptoms', 'nQOL_uppExtremity', 
            'nQOL_lowExtremity', 'dsst','nQOL_cognition',
            'tapF', 'walkF', 'restF', 'tremorF',
            'userActivity', 'baselineChar', 'FREEZE_DATE', 'wapi_survey',
            'DAYS_SINCE_LAUNCH', 'WEEKS_SINCE_LAUNCH', 
            'TAP_FEATURES', 'TREMOR_FEATURES', 'WALK_FEATURES', 'REST_FEATURES')

rm(list=ls()[!ls() %in% needed])
ls()
COL_MS_PATIENT = '#DC0000FF'
COL_CONTROL = '#00A087FF'
COL_MS_PATIENT_CLINICAL_REF = '#e66101'

save.image("elevateMS_paper2_dataFreeze.Rda")
synStore(File("elevateMS_paper2_dataFreeze.Rda",
              parentId = "syn19273715"),
         executed="https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/Analytical_Paper_2/createDataFreeze_paper2.R")
unlink("elevateMS_paper2_dataFreeze.Rda")
 
