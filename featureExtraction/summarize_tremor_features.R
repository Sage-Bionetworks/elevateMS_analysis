############################################################################
# ElevateMS project
# Purpose: Summarize Tremor features into median and IQR
# Author: Meghasyam Tummalacherla
# Code modeled after: https://github.com/th1vairam/mPowerAnalysis/blob/c285ace3e1288c2da4ad4e8c3c5c2b05f6e5298d/tremor_module/summariseMedianIqrFeatures_hc.Rmd
############################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(synapser)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(doMC)
library(jsonlite)
library(parallel)
library(tidyr)
library(lubridate)
library(stringr)
library(sqldf)
library(parsedate)
library(githubr) 
# devtools::install_github("brian-bot/githubr")
library(mhealthtools) 
# devtools::install_github("Sage-Bionetworks/mhealthtools")

#############
# Download data from Synapse
##############
# login to Synapse
synapser::synLogin()

# set system environment to UTC
Sys.setenv(TZ='GMT')

# Download the tremor table
tremor.tbl.id = 'syn10278767' # Tremor Activity-v5
tremor.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id))
tremor.tbl <- tremor.tbl.syn$asDataFrame()
all.used.ids <- tremor.tbl.id

# Download the profile table (for age)
profile.tbl.id <- 'syn10235463'
profile.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", profile.tbl.id))
profile.tbl <- profile.tbl.syn$asDataFrame() 
all.used.ids <- c(all.used.ids, profile.tbl.id)

# Certain healthCodes have multiple ages 
# (62, 63 etc., so we just consider the minimum of those two)
profile.tbl.cleaned <- profile.tbl %>% 
  dplyr::select(healthCode, age = demographics.age) %>% 
  dplyr::group_by(healthCode) %>% 
  dplyr::summarise(age = min(age)) %>% 
  dplyr::ungroup() %>% 
  unique() %>% 
  na.omit()

# Download the demographics table
demo.tbl.id = 'syn10295288' # Demographics table-v2
demo.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", demo.tbl.id))
demo.tbl <- demo.tbl.syn$asDataFrame()
all.used.ids <- c(all.used.ids, demo.tbl.id)

demo.tbl <- demo.tbl %>% 
  dplyr::mutate(MS = purrr::map(dataGroups, function(x){
    if(is.na(x)){
      return('NA') # if diagnosis is NA, return 'NA'
    }
    
    if(grepl('control', x)){ # for 'control', 'control,test_user'
      return('control')
    }else{
      return('ms') # for 'ms_patient', 'ms_patient,test_user'
    }
  }) %>% unlist()) %>% 
  dplyr::rename(gender = gender.json.answer) %>% 
  dplyr::filter(!(MS == 'NA')) %>% # filter out NA diagnosis
  dplyr::inner_join(profile.tbl.cleaned)

# Download the kinetic tremor features (hand to nose)
tremor_features_left <- synapser::synGet('syn12104398')$path %>% 
  read.csv(sep='\t') %>% 
  dplyr::select(-ac4_motion_tremor_handToNose_left.fileLocation.items)
tremor_features_right <- synapser::synGet('syn12104396')$path %>%
  read.csv(sep='\t') %>% 
  dplyr::select(-ac4_motion_tremor_handToNose_right.fileLocation.items)
all.used.ids <- c(all.used.ids, 'syn12104398', 'syn12104396')

tremor_features <- rbind(tremor_features_left, tremor_features_right) %>% 
  dplyr::mutate(IMF = paste0('IMF',IMF)) %>% 
  dplyr::inner_join(tremor.tbl %>%
                      dplyr::select(recordId, healthCode)) %>%
  dplyr::left_join(demo.tbl %>%
                     dplyr::select(healthCode, age, gender, MS)) %>%
  dplyr::filter(!is.na(MS), !is.na(gender), !is.na(age), 
                MS %in% c('control', 'ms'),
                gender %in% c('Female', 'Male')) %>%
  dplyr::select(-contains('EnergyInBand')) %>%
  tidyr::gather(Feature, Value, -IMF, -recordId, -sensor, -measurementType, -axis, -window,
                -healthCode, -age, -gender, -MS) %>%
  dplyr::group_by(sensor, measurementType, healthCode, age, gender, MS, IMF, Feature)

tremor_features.md = tremor_features %>%
  dplyr::summarise(Value = median(Value, na.rm = T)) %>%
  dplyr::mutate(type = 'md') %>%
  tidyr::unite(Feature, Feature, IMF, type, sep = '.') %>%
  tidyr::unite(Feature, Feature, measurementType, sensor, sep = '_')

tremor_features.iqr = tremor_features %>%
  dplyr::summarise(Value = IQR(Value, na.rm = T)) %>%
  dplyr::mutate(type = 'iqr') %>%
  tidyr::unite(Feature, Feature, IMF, type, sep = '.') %>%
  tidyr::unite(Feature, Feature, measurementType, sensor, sep = '_')

tremor_summary_features = rbindlist(list(tremor_features.md, tremor_features.iqr), use.names = T, fill = T) %>%
  tidyr::spread(Feature, Value) 

#############
# Upload data to Synapse
#############
# upload file to Synapse with provenance
# to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html

## Github link
# Copy paste the github token string and store it as 'github_token.txt' file
# A github token is required to access the elevateMS_analysis repository as it is private
gtToken = 'github_token.txt'
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- "featureExtraction/summarize_tremor_features.R" # location of file inside github repo
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Summarize tremor features"
activityDescription = "Summarize tremor features into IQR and median"

# upload to Synapse, summary features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "hcwiseSummaryFeatures.tsv" # name your file
write.table(tremor_summary_features, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = thisFile)
unlink(OUTPUT_FILE)
