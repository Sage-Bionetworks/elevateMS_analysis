############################################################################
# ElevateMS project
# Purpose: Extract Walk features
# Author: Abhishek Pratap, Meghasyam Tummalacherla
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


####### -- NOTE -- #######
# We will be using mpowertools for walking features since mhealthtools
# does not have a good feature extraction pipeline for walk
####### -- NOTE -- #######

library(mpowertools) 
# devtools::install_github("itismeghasyam/mpowertools")


#############
# Required functions
##############
featuresFromColumn <- function(dat,column,processingFunction, parallel = F){
  # Apply the processingFunction to each row of the column in the dataframe dat
  
  plyr::ddply(
    .data = dat,
    .variables = colnames(dat),
    .parallel = parallel,
    .fun = function(row) {
      return(processingFunction(row[column]))
    }
  ) 
}

#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############
# login to Synapse
synapser::synLogin()

# set system environment to UTC
Sys.setenv(TZ='GMT')

walk.tbl.id = 'syn10278766' # Walking Activity-v2
# walk.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", walk.tbl.id, " WHERE healthCode = 'adeca5c5-856d-49e8-b3d9-3402b961c05d'"))
walk.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", walk.tbl.id))
walk.tbl <- walk.tbl.syn$asDataFrame()

## Download required columns i,e the JSON files
columnsToDownload = c("deviceMotion_walking_outbound.json.items") 

walk.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(walk.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

## Convert column format to be able to do join and merge filelocations
walk.tbl$deviceMotion_walking_outbound.json.items <- as.character(walk.tbl$deviceMotion_walking_outbound.json.items)

walk.tbl.meta = data.table::rbindlist(list(walk.tbl %>%
                                             dplyr::left_join(do.call(cbind, walk.json.loc))),
                                      use.names = T, fill = T) %>%
  as.data.frame

## Convert column format from factors to strings for the fileLocations
walk.tbl.meta$deviceMotion_walking_outbound.fileLocation.items <- as.character(walk.tbl.meta$deviceMotion_walking_outbound.fileLocation.items)

#############
# Extract Walk features
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
doMC::registerDoMC(detectCores() - 2)

# walkJsonLocation <- walk.tbl.meta$deviceMotion_walking_outbound.fileLocation.items[1]

# extract Walk features
walk.tbl.meta.noNA.act <- walk.tbl.meta[!is.na(walk.tbl.meta$deviceMotion_walking_outbound.fileLocation.items),]

walk.tbl.meta.noNA <- walk.tbl.meta.noNA.act[1:500,]
walk_features_1 <- featuresFromColumn(
  dat = walk.tbl.meta.noNA,
  column = "deviceMotion_walking_outbound.fileLocation.items",
  processingFunction = function(walkJsonLocation){
    walkJsonLocation <- as.character(walkJsonLocation)
    walkFeatures <- mpowertools::getWalkFeatures(walkJsonLocation) 
    return(walkFeatures)  
  },
  parallel = runParallel 
)

walk.tbl.meta.noNA <- walk.tbl.meta.noNA.act[501:1000,]
walk_features_2 <- featuresFromColumn(
  dat = walk.tbl.meta.noNA,
  column = "deviceMotion_walking_outbound.fileLocation.items",
  processingFunction = function(walkJsonLocation){
    walkJsonLocation <- as.character(walkJsonLocation)
    walkFeatures <- mpowertools::getWalkFeatures(walkJsonLocation) 
    return(walkFeatures)  
  },
  parallel = runParallel 
)

walk.tbl.meta.noNA <- walk.tbl.meta.noNA.act[1001:1500,]
walk_features_3 <- featuresFromColumn(
  dat = walk.tbl.meta.noNA,
  column = "deviceMotion_walking_outbound.fileLocation.items",
  processingFunction = function(walkJsonLocation){
    walkJsonLocation <- as.character(walkJsonLocation)
    walkFeatures <- mpowertools::getWalkFeatures(walkJsonLocation) 
    return(walkFeatures)  
  },
  parallel = runParallel 
)

walk.tbl.meta.noNA <- walk.tbl.meta.noNA.act[1501:nrow(walk.tbl.meta.noNA.act),]
walk_features_4 <- featuresFromColumn(
  dat = walk.tbl.meta.noNA,
  column = "deviceMotion_walking_outbound.fileLocation.items",
  processingFunction = function(walkJsonLocation){
    walkJsonLocation <- as.character(walkJsonLocation)
    walkFeatures <- mpowertools::getWalkFeatures(walkJsonLocation) 
    return(walkFeatures)  
  },
  parallel = runParallel 
)

walk_features <- rbind(walk_features_1, walk_features_2,
                       walk_features_3, walk_features_4) %>% 
  unique()
walk_features <- walk_features %>% 
  dplyr::select(-deviceMotion_walking_outbound.fileLocation.items,
                -ROW_ID, -ROW_VERSION)  

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
thisFileName <- "featureExtraction/walkingFeatures.R" # location of file inside github repo
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract walk features"
activityDescription = "Extract walk features from walking activity-v2"

# upload to Synapse
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "WalkingFeatures.tsv" # name your file
write.table(walk_features, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = walk.tbl.id,
         executed = list(thisFile, "https://github.com/itismeghasyam/mpowertools"))
unlink(OUTPUT_FILE)
