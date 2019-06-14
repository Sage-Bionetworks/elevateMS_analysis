############################################################################
# ElevateMS project
# Purpose: Find Age Matched health codes(elevateMS + mpower controls)
# Author: Meghasyam Tummalacherla
############################################################################
rm(list = ls())
gc()

##############
# Required libraries
##############
library(synapser)
library(plyr)
library(dplyr)
library(MatchIt)
synapser::synLogin()

###########################################################
## Download data for age matching
###########################################################
## ** ElevateMS Data download and tidying ** ##
# Tremor activity tables from elevateMS project
tremor.tbl.id.ms = 'syn10278767' # Tremor Activity-v5
tremor.tbl.syn.ms <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id.ms))
tremor.tbl.ms <- tremor.tbl.syn.ms$asDataFrame()

all.used.ids = tremor.tbl.id.ms

# Get demographics from synapse
demo.tbl.id.ms = 'syn17115631' # elevateMS_baselineCharacteristics
demo.tbl.syn.ms <- synapser::synGet(demo.tbl.id.ms)
demo.tbl.ms <- demo.tbl.syn.ms$path %>% read.csv(sep = '\t')
all.used.ids <- c(all.used.ids, demo.tbl.id.ms)

# Get number of records(n), age, dataGroup(ms_patient/control), gender per healthCode
hc.nc.ms <- tremor.tbl.ms %>% 
  dplyr::select(healthCode, recordId) %>% 
  dplyr::left_join(demo.tbl.ms %>% # Also rename dataGroups to MS
                     dplyr::select(healthCode, MS = dataGroups, gender, age)) %>% 
  dplyr::filter(!is.na(MS), !is.na(age), !(is.na(gender))) %>% # remove NAs
  dplyr::filter(MS %in% c('ms_patient','control'),
                gender %in% c('male','female')) %>% 
  unique() %>% 
  droplevels() %>% 
  dplyr::count(healthCode, age, gender, MS) %>% 
  unique()

# High frequency filter (atleast 2 records)
hc.hfreq.ms = hc.nc.ms %>% na.omit() %>% 
  dplyr::select(healthCode, age, gender, MS, n) %>% 
  dplyr::filter(n > 1) %>% # More than 1 record ~ atleast 2 records
  unique()

## ** mpower Data download and tidying ** ##
# Tremor activity tables from mpower project
tremor.tbl.id.mpower = 'syn10676309' # Tremor Activity-v2
tremor.tbl.syn.mpower <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id.mpower))
tremor.tbl.mpower <- tremor.tbl.syn.mpower$asDataFrame()

all.used.ids = c(all.used.ids, tremor.tbl.id.mpower)

# Get demographics from synapse
demo.tbl.id.mpower = 'syn10371840' # Demographics table-v2
demo.tbl.syn.mpower <- synapser::synTableQuery(paste0("SELECT * FROM ", demo.tbl.id.mpower))
demo.tbl.mpower <- demo.tbl.syn.mpower$asDataFrame()
all.used.ids <- c(all.used.ids, demo.tbl.id.mpower)

# Get number of records(n), age, dataGroup(ms_patient/control), gender per healthCode
hc.nc.mpower <- tremor.tbl.mpower %>% 
  dplyr::select(healthCode, recordId) %>% 
  dplyr::left_join(demo.tbl.mpower %>% # Also rename inferred_diagnosis to MS
                     dplyr::select(healthCode, MS = inferred_diagnosis, gender, age)) %>% 
  dplyr::filter(!is.na(MS), !is.na(age), !(is.na(gender))) %>% # remove NAs
  dplyr::filter(MS %in% c('FALSE'), # Filter only controls
                gender %in% c('Male','Female')) %>% 
  dplyr::mutate(gender = tolower(gender)) %>% 
  dplyr::mutate(MS = 'control') %>% 
  unique() %>% 
  droplevels() %>% 
  dplyr::count(healthCode, age, gender, MS) %>% 
  unique()

# High frequency filter (atleast 2 records)
hc.hfreq.mpower = hc.nc.mpower %>% na.omit() %>% 
  dplyr::select(healthCode, age, gender, MS, n) %>% 
  dplyr::filter(n > 1) %>% # More than 1 record ~ atleast 2 records
  unique()

###########################################################
## Age Match data (elevateMS + mpower controls data - only hfreq data - atleast 2 records)
###########################################################
hc.hfreq.all <- rbind(hc.hfreq.ms, hc.hfreq.mpower) %>% unique()
# rbind the hfreq ms and mpower data

set.seed(123456789)
dat = hc.hfreq.all %>%
  dplyr::mutate(MS = factor(MS, levels = c('control','ms_patient')),
                MS = as.numeric(MS) - 1) %>%
  unique()
age.matched = matchit(MS ~ age, data = data.frame(dat), method = "exact")
dat = match.data(age.matched)

# Break ties using number of records
set.seed(123456789)
age.records.matched = plyr::ddply(dat, .(subclass), .fun = function(x){
  dat1 = x %>%
    dplyr::select(healthCode, MS, n, age, gender) %>%
    unique() 
  records.matched = matchit(MS ~ n, data = dat1)
  dat1 = match.data(records.matched)
})

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
thisFileName <- "analysis/Tremor_analysis/age_match_healthCodes.R" # location of file inside github repo
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Age Mactch HealthCodes"
activityDescription = "Age Match elevateMS data pooled with mpower controls"

# upload to Synapse, left hand features
synapse.folder.id <- "syn19120636" # synId of folder to upload your file to
OUTPUT_FILE <- "age_match_healthCodes.tsv" # name your file
write.table(tremor_features_left, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = thisFile)
unlink(OUTPUT_FILE)
