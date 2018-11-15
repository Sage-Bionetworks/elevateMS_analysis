############################################################################
# ElevateMS project
# Purpose: Process Walking Pedometer Data
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
walk.tbl.id = 'syn10278766' # Walking Activity-v2
walk.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", walk.tbl.id))
walk.tbl <- walk.tbl.syn$asDataFrame()

## Convert createdOn into an understandable datetime format
walk.tbl$createdOn <- lubridate::as_datetime(walk.tbl$createdOn/1000)

## Account for timezone change, if column is in local time
# walk.tbl$createdOn <- walk.tbl$createdOn - 60*60*as.numeric(walk.tbl$createdOnTimeZone)/100

## Download required columns i,e the JSON files
columnsToDownload = c("pedometer_walking_outbound.json.items") 

walk.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(walk.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

## Convert column format to be able to do join and merge filelocations
walk.tbl$pedometer_walking_outbound.json.items <- as.character(walk.tbl$pedometer_walking_outbound.json.items)

walk.tbl.meta = data.table::rbindlist(list(walk.tbl %>%
                                                dplyr::left_join(do.call(cbind, walk.json.loc))),
                                         use.names = T, fill = T) %>%
  as.data.frame

## Convert column format from factors to strings for the fileLocations
walk.tbl.meta$pedometer_walking_outbound.fileLocation.items <- as.character(walk.tbl.meta$pedometer_walking_outbound.fileLocation.items)

#############
# Merge all walk data into a single table
#############
walk.tbl.meta.noNA <- walk.tbl.meta[!is.na(walk.tbl.meta$pedometer_walking_outbound.fileLocation.items),]
walk.data.all <- apply(walk.tbl.meta.noNA,1,function(x){ 
  tryCatch({dat <- jsonlite::fromJSON(as.character(x["pedometer_walking_outbound.fileLocation.items"]))
  dat <- dat %>% dplyr::mutate(recordId = x['recordId'])
  },
  error = function(e){ NA })
}) %>% plyr::ldply(data.frame) %>% 
  dplyr::select(numberOfSteps, floorsDescended,startDate,distance,timestamp,
                recorderId,endDate, floorsAscended,recordId)

walk.data.all <- walk.data.all %>% 
  dplyr::inner_join(walk.tbl.meta %>% 
                      dplyr::select(healthCode, recordId)) %>% 
  unique()

#############
# Upload to Synapse
#############
# Github link
gtToken = 'github_token.txt';
githubr::setGithubToken(as.character(read.table(gtToken)$V1))
thisFileName <- 'featureExtraction/walkingPedometerData.R'
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# Write to Synapse
fileName <- paste0('walkingPedometerFeatures','.csv')
write.csv(passive.data.all,file = fileName,na="")
PARENT_FOLDER = 'syn10140063'
obj = File(fileName, 
           name = fileName, 
           parentId = PARENT_FOLDER)
obj = synStore(obj,  used = passive.tbl.id, executed = thisFile)
