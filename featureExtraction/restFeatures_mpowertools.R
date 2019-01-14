############################################################################
# ElevateMS project
# Purpose: Extract Rest features
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
library(mhealthtools) 
# devtools::install_github("Sage-Bionetworks/mhealthtools")

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

rest.tbl.id = 'syn10278766' # Walking Activity-v2
# rest.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", rest.tbl.id, " WHERE healthCode = 'adeca5c5-856d-49e8-b3d9-3402b961c05d'"))
rest.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", rest.tbl.id))
rest.tbl <- rest.tbl.syn$asDataFrame()

## Convert createdOn into an understandable datetime format
rest.tbl$createdOn <- lubridate::as_datetime(rest.tbl$createdOn/1000)

## Account for timezone change, if column is in local time
# rest.tbl$createdOn <- rest.tbl$createdOn - 60*60*as.numeric(rest.tbl$createdOnTimeZone)/100

## Download required columns i,e the JSON files
columnsToDownload = c("deviceMotion_walking_rest.json.items") 

rest.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(rest.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

## Convert column format to be able to do join and merge filelocations
rest.tbl$deviceMotion_walking_rest.json.items <- as.character(rest.tbl$deviceMotion_walking_rest.json.items)

rest.tbl.meta = data.table::rbindlist(list(rest.tbl %>%
                                             dplyr::left_join(do.call(cbind, rest.json.loc))),
                                      use.names = T, fill = T) %>%
  as.data.frame

## Convert column format from factors to strings for the fileLocations
rest.tbl.meta$deviceMotion_walking_rest.fileLocation.items <- as.character(rest.tbl.meta$deviceMotion_walking_rest.fileLocation.items)

#############
# Extract Rest features
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
doMC::registerDoMC(detectCores() - 2)

# restJsonLocation <- rest.tbl.meta$deviceMotion_walking_rest.fileLocation.items[1]

# extract Rest features
rest.tbl.meta.noNA.act <- rest.tbl.meta[!is.na(rest.tbl.meta$deviceMotion_walking_rest.fileLocation.items),]

rest.tbl.meta.noNA <- rest.tbl.meta.noNA.act[1:500,]
rest_features_1 <- featuresFromColumn(
  dat = rest.tbl.meta.noNA,
  column = "deviceMotion_walking_rest.fileLocation.items",
  processingFunction = function(restJsonLocation){
    restJsonLocation <- as.character(restJsonLocation)
    restFeatures <- mpowertools::getRestFeatures(restJsonLocation) 
    return(restFeatures)  
  },
  parallel = runParallel 
)

rest.tbl.meta.noNA <- rest.tbl.meta.noNA.act[501:1000,]
rest_features_2 <- featuresFromColumn(
  dat = rest.tbl.meta.noNA,
  column = "deviceMotion_walking_rest.fileLocation.items",
  processingFunction = function(restJsonLocation){
    restJsonLocation <- as.character(restJsonLocation)
    restFeatures <- mpowertools::getRestFeatures(restJsonLocation) 
    return(restFeatures)  
  },
  parallel = runParallel 
)

rest.tbl.meta.noNA <- rest.tbl.meta.noNA.act[1001:1500,]
rest_features_3 <- featuresFromColumn(
  dat = rest.tbl.meta.noNA,
  column = "deviceMotion_walking_rest.fileLocation.items",
  processingFunction = function(restJsonLocation){
    restJsonLocation <- as.character(restJsonLocation)
    restFeatures <- mpowertools::getRestFeatures(restJsonLocation) 
    return(restFeatures)  
  },
  parallel = runParallel 
)

rest.tbl.meta.noNA <- rest.tbl.meta.noNA.act[1500:nrow(rest.tbl.meta.noNA.act),]
rest_features_4 <- featuresFromColumn(
  dat = rest.tbl.meta.noNA,
  column = "deviceMotion_walking_rest.fileLocation.items",
  processingFunction = function(restJsonLocation){
    restJsonLocation <- as.character(restJsonLocation)
    restFeatures <- mpowertools::getRestFeatures(restJsonLocation) 
    return(restFeatures)  
  },
  parallel = runParallel 
)

rest_features <- rbind(rest_features_1, rest_features_2,
                       rest_features_3, rest_features_4) %>% 
  unique()
rest_features <- rest_features %>% 
  dplyr::select(-deviceMotion_walking_rest.fileLocation.items,
                -ROW_ID, -ROW_VERSION)  

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
thisFileName <- "featureExtraction/restFeatures_mpowertools.R" # location of file inside github repo
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract rest features"
activityDescription = "Extract rest features from walking activity-v2"

# upload to Synapse
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "RestFeatures.tsv" # name your file
write.table(rest_features, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = rest.tbl.id,
         executed = list(thisFile, "https://github.com/itismeghasyam/mpowertools"))
unlink(OUTPUT_FILE)
