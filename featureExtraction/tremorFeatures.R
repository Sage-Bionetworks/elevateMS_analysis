############################################################################
# ElevateMS project
# Purpose: Extract Tremor features
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
processTremorFile <- function(tremorJsonFileLocation){
  # Read the Json File and process it into mhealthtools format
  
  tremorData <-   tryCatch({
    tremor_data <- jsonlite::fromJSON(as.character(tremorJsonFileLocation))
    
    accel_data <- tremor_data$userAcceleration
    accel_data$t <- tremor_data$timestamp - tremor_data$timestamp[1]
    
    gyro_data <- tremor_data$rotationRate
    gyro_data$t <- tremor_data$timestamp - tremor_data$timestamp[1]
    
    grav_data <- tremor_data$gravity
    grav_data$t <- tremor_data$timestamp - tremor_data$timestamp[1]
    
    tremor_data <- list(accelerometer = accel_data,
                        gyroscope = gyro_data,
                        gravity = grav_data)
  }, error = function(err) {
    tremor_data <- list(accelerometer = NA,
                        gyroscope = NA,
                        gravity = NA)
    # NAs are handled in mhealthtools
  })
}

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

extractTremorFeatures <- function(dat_, column_, runParallel_){
  tremor_features <- featuresFromColumn(
    dat = dat_,
    column = column_,
    processingFunction = function(tremorJsonLocation){
      tremorData <- processTremorFile(tremorJsonLocation)
      samplingRate <- mhealthtools:::get_sampling_rate(tremorData$accelerometer)
      tremorFeatures <- mhealthtools::get_tremor_features(
        accelerometer_data = tremorData$accelerometer,
        gyroscope_data = tremorData$gyroscope,
        gravity_data = tremorData$gravity,
        funs = mhealthtools:::default_kinematic_features(samplingRate)
      ) 
      tremorFeatures <- tremorFeatures$extracted_features
      tremorFeatures <- tremorFeatures %>% 
        dplyr::select(-window_start_time,-window_end_time, -window, -error) %>% 
        unique() %>%
        tidyr::unite(sensor.measure, sensor, measurementType) %>% 
        dplyr::filter(sensor.measure == 'accelerometer_acceleration' | 
                        sensor.measure == 'gyroscope_velocity') %>% 
        tidyr::separate(sensor.measure, c('sensor','measurementType')) %>% 
        tidyr::gather(Feature, Value,-sensor, -measurementType, -axis) %>%
        dplyr::group_by(Feature, sensor, measurementType, axis) %>% 
        dplyr::summarise(iqr = stats::IQR(Value, na.rm = T),
                         md = stats::median(Value, na.rm = T)) %>% 
        tidyr::unite(feature, Feature, sensor, measurementType, axis)
      a.iqr <- data.frame(tremorFeatures$iqr) %>% 
        data.table::transpose() %>%
        `colnames<-`(paste0(tremorFeatures$feature,'_iqr'))
      a.md <- data.frame(tremorFeatures$md) %>% 
        data.table::transpose() %>%
        `colnames<-`(paste0(tremorFeatures$feature,'_md'))
      tremorFeatures <- cbind(a.iqr, a.md)
      return(tremorFeatures)  
    },
    parallel = runParallel_ 
  )
  return(tremor_features) 
}
#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############
# login to Synapse
synapser::synLogin()

# set system environment to UTC
Sys.setenv(TZ='GMT')

tremor.tbl.id = 'syn10278767' # Tremor Activity-v5
# tremor.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id, " WHERE healthCode = 'adeca5c5-856d-49e8-b3d9-3402b961c05d'"))
tremor.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id))
tremor.tbl <- tremor.tbl.syn$asDataFrame()

## Convert createdOn into an understandable datetime format
tremor.tbl$createdOn <- lubridate::as_datetime(tremor.tbl$createdOn/1000)

## Account for timezone change, if column is in local time
# tremor.tbl$createdOn <- tremor.tbl$createdOn - 60*60*as.numeric(tremor.tbl$createdOnTimeZone)/100

## Download required columns i,e the JSON files
columnsToDownload = c("ac4_motion_tremor_handToNose_right.json.items",
                      "ac4_motion_tremor_handToNose_left.json.items") 

tremor.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(tremor.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

## Convert column format to be able to do join and merge filelocations
tremor.tbl$ac4_motion_tremor_handToNose_right.json.items <- as.character(
  tremor.tbl$ac4_motion_tremor_handToNose_right.json.items)
tremor.tbl$ac4_motion_tremor_handToNose_left.json.items <- as.character(
  tremor.tbl$ac4_motion_tremor_handToNose_left.json.items)

tremor.tbl.meta = data.table::rbindlist(
  list(tremor.tbl %>%
         dplyr::left_join(do.call(cbind, tremor.json.loc[1]))),
  use.names = T, fill = T) %>%
  as.data.frame
tremor.tbl.meta = data.table::rbindlist(
  list(tremor.tbl.meta %>%
         dplyr::left_join(do.call(cbind, tremor.json.loc[2]))),
  use.names = T, fill = T) %>%
  as.data.frame

## Convert column format from factors to strings for the fileLocations
tremor.tbl.meta$ac4_motion_tremor_handToNose_right.fileLocation.items <- as.character(
  tremor.tbl.meta$ac4_motion_tremor_handToNose_right.fileLocation.items)
tremor.tbl.meta$ac4_motion_tremor_handToNose_left.fileLocation.items <- as.character(
  tremor.tbl.meta$ac4_motion_tremor_handToNose_left.fileLocation.items)

#############
# Extract Tremor features
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
doMC::registerDoMC(detectCores() - 2)

## Left Hand Features
tremor.tbl.meta.noNA.left <- tremor.tbl.meta[!is.na(tremor.tbl.meta$ac4_motion_tremor_handToNose_left.fileLocation.items),]

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[1:500,]
tremor_features_left_1 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "ac4_motion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[501:1000,]
tremor_features_left_2 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "ac4_motion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[1001:1500,]
tremor_features_left_3 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "ac4_motion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[1501:2000,]
tremor_features_left_4 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "ac4_motion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[2001:nrow(tremor.tbl.meta.noNA.left),]
tremor_features_left_5 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "ac4_motion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor_features_left <- rbind(tremor_features_left_1, tremor_features_left_2,
                              tremor_features_left_3, tremor_features_left_4,
                              tremor_features_left_5)

## right Hand Features
tremor.tbl.meta.noNA.right <- tremor.tbl.meta[!is.na(tremor.tbl.meta$ac4_motion_tremor_handToNose_right.fileLocation.items),]

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[1:500,]
tremor_features_right_1 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA, 
  column_ = "ac4_motion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[501:1000,]
tremor_features_right_2 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA, 
  column_ = "ac4_motion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[1001:1500,]
tremor_features_right_3 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA, 
  column_ = "ac4_motion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[1501:2000,]
tremor_features_right_4 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA, 
  column_ = "ac4_motion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[2001:nrow(tremor.tbl.meta.noNA.right),]
tremor_features_right_5 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA, 
  column_ = "ac4_motion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()

tremor_features_right <- rbind(tremor_features_right_1, tremor_features_right_2,
                              tremor_features_right_3, tremor_features_right_4,
                              tremor_features_right_5)

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
thisFileName <- "featureExtraction/tremorFeatures.R" # location of file inside github repo
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract tremor features"
activityDescription = "Extract tremor features from tremor activity-v5"

# upload to Synapse, left hand features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "tremorFeatures_handToNoseLeft.tsv" # name your file
write.table(tremor_features_left, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = tremor.tbl.id,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mhealthtools"))
unlink(OUTPUT_FILE)

# upload to Synapse, right hand features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "tremorFeatures_handToNoseRight.tsv" # name your file
write.table(tremor_features_right, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = tremor.tbl.id,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mhealthtools"))
unlink(OUTPUT_FILE)
