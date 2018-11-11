############################################################################
# ElevateMS project
# Purpose: Process Passive Data
# Author: Meghasyam Tummalacherla
############################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(synapser)
library(plyr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tidyr)
library(lubridate)
library(stringr)
library(sqldf)
library(parsedate)
library(githubr) 
# devtools::install_github("brian-bot/githubr")
library(data.table)
## Synapse Login
synapser::synLogin()

#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############
passive.tbl.id = 'syn10651116' # Passive data-v3
passive.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", passive.tbl.id))
passive.tbl <- passive.tbl.syn$asDataFrame()

## Convert createdOn into an understandable datetime format
passive.tbl$createdOn <- lubridate::as_datetime(passive.tbl$createdOn/1000)

## Account for timezone change, if column is in local time
# passive.tbl$createdOn <- passive.tbl$createdOn - 60*60*as.numeric(passive.tbl$createdOnTimeZone)/100

## Download required columns i,e the JSON files
columnsToDownload = c("pedometer.json.pedometerData") 

passive.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(passive.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

## Convert column format to be able to do join and merge filelocations
passive.tbl$pedometer.json.pedometerData <- as.character(passive.tbl$pedometer.json.pedometerData)

passive.tbl.meta = data.table::rbindlist(list(passive.tbl %>%
                                                dplyr::left_join(do.call(cbind, passive.json.loc))),
                                         use.names = T, fill = T) %>%
  as.data.frame

## Convert column format from factors to strings for the fileLocations
passive.tbl.meta$pedometer.fileLocation.pedometerData <- as.character(passive.tbl.meta$pedometer.fileLocation.pedometerData)

#############
# Merge all passive data into a single table
#############
passive.data.all <- apply(passive.tbl.meta,1,function(x){ 
  tryCatch({dat <- jsonlite::fromJSON(as.character(x["pedometer.fileLocation.pedometerData"]))
  dat <- dat %>% dplyr::mutate(recordId = x['recordId'])
  },
  error = function(e){ NA })
}) %>% plyr::ldply(data.frame) %>% 
  dplyr::select(numberOfSteps, floorsDescended,startDate,distance,averageActivePace,
                endDate, floorsAscended,recordId) %>% 
  na.omit() %>% 
  unique()

passive.data.all <- passive.data.all %>% 
  dplyr::inner_join(passive.tbl.meta %>% 
                     dplyr::select(healthCode, recordId)) %>% 
  unique()

#############
# Upload to Synapse
#############
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'featureExtraction/passiveData.R'
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
fileName <- paste0('allPassiveData','.csv')
write.csv(passive.data.all,file = fileName,na="")
PARENT_FOLDER = 'syn10140063'
obj = File(fileName, 
           name = fileName, 
           parentId = PARENT_FOLDER)
obj = synStore(obj,  used = passive.tbl.id, executed = thisFile)
