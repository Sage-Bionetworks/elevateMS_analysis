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

# Tremor activity tables from elevateMS project
tremor.tbl.id = 'syn10278767' # Tremor Activity-v5
tremor.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id))
tremor.tbl <- tremor.tbl.syn$asDataFrame()

all.used.ids = tremor.tbl.id

# Get demographics from synapse
demo.tbl.id = 'syn10295288' # Demographics table-v2
demo.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", demo.tbl.id))
demo.tbl <- demo.tbl.syn$asDataFrame()
all.used.ids <- c(all.used.ids, demo.tbl.id)

# Get Profile data (age data)
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

# Get tremor features from synapse and count number of windows available for each hc
ftrs.id = c(handToNose_left = 'syn12104398', handToNose_right = 'syn12104396')
all.used.ids = c(all.used.ids, as.character(ftrs.id))

# Load features from synapse
ftrs = purrr::map(ftrs.id, function(id){
  fread(synapser::synGet(id)$path, fill = TRUE) %>%
    dplyr::filter(IMF %in% c(1,2)) %>%
    unique()
}) %>%
  data.table::rbindlist(idcol = 'Assay') %>%
  dplyr::inner_join(tremor.tbl %>%
                      dplyr::select(recordId, healthCode)) %>%
  dplyr::left_join(demo.tbl %>%
                     dplyr::select(healthCode, gender.json.answer, dataGroups) %>%
                     unique()) %>%
  dplyr::rename(MS = dataGroups, gender = gender.json.answer) %>% 
  dplyr::mutate(IMF = paste0('IMF',IMF)) %>% 
  dplyr::filter(MS %in% c('ms_patient','control'))

ftrs$energy.tm <- as.numeric(ftrs$energy.tm)

###########################################################
## Combine energy features in to 1 Hz band
###########################################################
energy.ftr = ftrs %>%
  tidyr::unite(rid, recordId, Assay, sensor, measurementType, IMF, axis, window, sep = '.') %>%
  dplyr::select(rid, contains('EnergyInBand'))

energy.ftr.cmbn = purrr::map2(seq(1,24,by=1),
                              seq(1.5,24.5,by=1),
                              function(x, y, innerFtr){
                                ind = paste0('EnergyInBand',gsub('\\.','_', seq(x, y, by=0.5)))
                                innerFtr = innerFtr %>%
                                  dplyr::select(rid, one_of(ind))
                                rowSums(innerFtr[,2:3], na.rm = T)
                              }, energy.ftr) %>%
  do.call(cbind,.)
colnames(energy.ftr.cmbn) = paste0('EnergyInBand', seq(1,24,by=1))
energy.ftr.cmbn = cbind(data.frame(rid = energy.ftr$rid), energy.ftr.cmbn)

###########################################################
## Summarize features for elevateMS
###########################################################
# Get kinetic tremor features
kinetic.ftr = ftrs %>%
  tidyr::unite(rid, recordId, Assay, sensor, measurementType, IMF, axis, window, sep = '.') %>%
  dplyr::select(-contains('EnergyInBand')) %>%
  dplyr::left_join(energy.ftr.cmbn) %>%
  tidyr::separate(rid, c('recordId', 'Assay', 'sensor', 'measurementType', 'IMF', 'axis', 'window'), sep = '\\.') %>%
  dplyr::select(-recordId, -Assay, -axis, -window) %>%
  tidyr::gather(Feature, Value, -healthCode, -gender, -MS, -sensor, -measurementType, -IMF) %>%
  dplyr::group_by(Feature, healthCode, gender, MS, sensor, measurementType, IMF) %>%
  dplyr::summarise(iqr = stats::IQR(Value, na.rm = T),
                   md = stats::median(Value, na.rm = T))

# Get kinetic data and covariates seperately
kinetic.cov = kinetic.ftr %>%
  dplyr::ungroup() %>%
  dplyr::select(healthCode, gender, MS) %>%
  unique() %>%
  purrr::map_df(factor) %>%
  as.data.frame()
rownames(kinetic.cov) = kinetic.cov$healthCode

# Get median of features
kinetic.ftr.md = kinetic.ftr %>%
  dplyr::ungroup() %>%
  dplyr::select(healthCode, sensor, measurementType, Feature, IMF, md) %>%
  dplyr::mutate(type = 'md') %>%
  tidyr::unite(nFeature, Feature, IMF, type, sep = '.') %>%
  tidyr::spread(nFeature, md)

# Get iqr of features
kinetic.ftr.iqr = kinetic.ftr %>%
  dplyr::ungroup() %>%
  dplyr::select(healthCode, sensor, measurementType, Feature, IMF, iqr) %>%
  dplyr::mutate(type = 'iqr') %>%
  tidyr::unite(nFeature, Feature, IMF, type, sep = '.') %>%
  tidyr::spread(nFeature, iqr)

# Combine median and iqr features (all data)
kinetic.ftr = dplyr::inner_join(kinetic.ftr.md, kinetic.ftr.iqr)

# # Remove linearly associated features
# tmp.mat = kinetic.ftr %>%
#   dplyr::select(-healthCode, -sensor, -measurementType)
# lm.combo = caret::findLinearCombos(tmp.mat)

kinetic.ftr.all = kinetic.ftr %>%
  # dplyr::select(-one_of(colnames(tmp.mat)[lm.combo$remove])) %>%
  tidyr::gather(Feature, Value, -healthCode, -sensor, -measurementType) %>%
  tidyr::unite(featureName, Feature, measurementType, sensor, sep = '_') %>%
  tidyr::spread(featureName, Value)

#############
# Upload data to Synapse
#############
# upload file to Synapse with provenance
# to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html

## Github link
# Copy paste the github token string and store it as 'github_token.txt' file
# A github token is required to access the elevateMS_analysis repository as it is private
gtToken = '~/github_token.txt'
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
write.table(kinetic.ftr.all, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = thisFile)
unlink(OUTPUT_FILE)
