############################################################################
# ElevateMS project
# Purpose: Extract Tremor features for mpower controls in age matched sample
#          to be used later in analysis
# Author: Meghasyam Tummalacherla
############################################################################
# rm(list=ls())
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

changeMeasurementType <- function(x){
  if(as.character(x[['sensor']]) == 'accelerometer'){
    if(x[['measurementType']] == 'acceleration'){
      return('ua')
    }
    if(x[['measurementType']] == 'jerk'){
      return('uj')
    }
    if(x[['measurementType']] == 'velocity'){
      return('uv')
    }
    if(x[['measurementType']] == 'displacement'){
      return('ud')
    }
    if(x[['measurementType']] == 'acf'){
      return('uaacf')
    }
  }
  if(as.character(x[['sensor']]) == 'gyroscope'){
    if(x[['measurementType']] == 'acceleration'){
      return('uaa')
    }
    # if(x[['measurementType']] == 'jerk'){
    #   return('uj')
    # }
    if(x[['measurementType']] == 'velocity'){
      return('uav')
    }
    if(x[['measurementType']] == 'displacement'){
      return('uad')
    }
    if(x[['measurementType']] == 'acf'){
      return('uavacf')
    }
  }
}

errorTremorFeatureDataFrame <- function(flag_){
  if(flag_){
    tremorFeatures <- mhealthtools::get_tremor_features(
      accelerometer_data = mhealthtools::accelerometer_data,
      gyroscope_data = mhealthtools::gyroscope_data,
      gravity_data = mhealthtools::gravity_data,
      window_length = 512,
      window_overlap = 0.5,
      detrend = T,
      derived_kinematics = T,
      frequency_filter = c(1,25),
      IMF = 2
    )
    
    tremorFeatures <- tremorFeatures$extracted_features
    
    tremorFeatures <- plyr::ddply(
      .data = tremorFeatures,
      .variables = colnames(tremorFeatures),
      .parallel = runParallel,
      .fun = function(row) {
        return(changeMeasurementType(row[c('sensor','measurementType')]))
      }
    ) %>% 
      dplyr::select(-measurementType) %>% 
      dplyr::rename(measurementType = V1) %>% 
      dplyr::filter(IMF %in% c(1,2))
    
    aa <- tremorFeatures[1,]
    aa$skew.fr <- -88888
    return(aa)
  }else{
    return(NA)
  }
  
}


extractTremorFeatures <- function(dat_, column_, runParallel_){
  tremor_features <- featuresFromColumn(
    dat = dat_,
    column = column_,
    processingFunction = function(tremorJsonLocation){
      tremorData <- processTremorFile(tremorJsonLocation)
      samplingRate <- mhealthtools:::get_sampling_rate(tremorData$accelerometer)
      
      tremorFeatures_ <- tryCatch({
        tremorFeatures <- mhealthtools::get_tremor_features(
          accelerometer_data = tremorData$accelerometer,
          gyroscope_data = tremorData$gyroscope,
          gravity_data = tremorData$gravity,
          window_length = 256,
          window_overlap = 0.5,
          detrend = T,
          derived_kinematics = T,
          frequency_filter = c(1,25),
          IMF = 4
        )
        
        tremorFeatures <- tremorFeatures$extracted_features
        
        if(is.null(tremorFeatures)){
          tremorFeatures <- errorTremorFeatureDataFrame(TRUE)
        }else{
          tremorFeatures <- plyr::ddply(
            .data = tremorFeatures,
            .variables = colnames(tremorFeatures),
            .parallel = runParallel_,
            .fun = function(row) {
              return(changeMeasurementType(row[c('sensor','measurementType')]))
            }
          ) %>% 
            dplyr::select(-measurementType) %>% 
            dplyr::rename(measurementType = V1) %>% 
            dplyr::filter(IMF %in% c(1,2))
        }
        
        return(tremorFeatures)
        
      }, error = function(e){
        tremorFeatures <- NULL
        
        if(is.null(tremorFeatures)){
          tremorFeatures <- errorTremorFeatureDataFrame(TRUE)
        }else{
          tremorFeatures <- plyr::ddply(
            .data = tremorFeatures,
            .variables = colnames(tremorFeatures),
            .parallel = runParallel_,
            .fun = function(row) {
              return(changeMeasurementType(row[c('sensor','measurementType')]))
            }
          ) %>% 
            dplyr::select(-measurementType) %>% 
            dplyr::rename(measurementType = V1) %>% 
            dplyr::filter(IMF %in% c(1,2))
        }
        
        return(tremorFeatures)
        
      })
      
      return(tremorFeatures_)  
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

# Get age matched healthCodes from synapse
age.records.matched.id = 'syn19123754' # Age matched healthCodes
age.records.matched.syn <- synapser::synGet(age.records.matched.id)
age.records.matched <- age.records.matched.syn$path %>% read.csv(sep = '\t')

tremor.tbl.id = 'syn10676309' # Tremor Activity-v5
# Select only those healthCodes from the mpower tremor table that are present in the age matched healthcodes
tremor.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id, " WHERE healthCode IN (", 
                                                 paste0( paste0("'",age.records.matched$healthCode,"'"),collapse = ','),")"))
tremor.tbl <- tremor.tbl.syn$asDataFrame()

## Download required columns i,e the JSON files
columnsToDownload = c("deviceMotion_tremor_handToNose_left.json.items",
                      "deviceMotion_tremor_handToNose_right.json.items") 

tremor.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(tremor.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

## Convert column format to be able to do join and merge filelocations
tremor.tbl$deviceMotion_tremor_handToNose_left.json.items <- as.character(
  tremor.tbl$deviceMotion_tremor_handToNose_left.json.items)
tremor.tbl$deviceMotion_tremor_handToNose_right.json.items <- as.character(
  tremor.tbl$deviceMotion_tremor_handToNose_right.json.items)

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
tremor.tbl.meta$deviceMotion_tremor_handToNose_left.fileLocation.items <- as.character(
  tremor.tbl.meta$deviceMotion_tremor_handToNose_left.fileLocation.items)
tremor.tbl.meta$deviceMotion_tremor_handToNose_right.fileLocation.items <- as.character(
  tremor.tbl.meta$deviceMotion_tremor_handToNose_right.fileLocation.items)

#############
# Extract Tremor features
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
doMC::registerDoMC(detectCores() - 2)

# Left Hand Features
tremor.tbl.meta.noNA.left <- tremor.tbl.meta[!is.na(tremor.tbl.meta$deviceMotion_tremor_handToNose_left.fileLocation.items),] %>%
  dplyr::select(recordId, deviceMotion_tremor_handToNose_left.fileLocation.items)

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[1:500,]
tremor_features_left_1 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "deviceMotion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()
# 
tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[501:1000,]
tremor_features_left_2 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "deviceMotion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()
# 
tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.left[1001:nrow(tremor.tbl.meta.noNA.left),]
tremor_features_left_3 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "deviceMotion_tremor_handToNose_left.fileLocation.items",
  runParallel_ = runParallel)
gc()
# 

tremor_features_left <- rbind(tremor_features_left_1, tremor_features_left_2,
                              tremor_features_left_3)

# Remove the error tremor data frames by filtering on skew.fr
# (look at errorTremorFeatureDataFrame)
tremor_features_left <- tremor_features_left %>%
  dplyr::filter(skew.fr != -88888) %>% 
  dplyr::select(-deviceMotion_tremor_handToNose_left.fileLocation.items)

## right Hand Features
tremor.tbl.meta.noNA.right <- tremor.tbl.meta[!is.na(tremor.tbl.meta$deviceMotion_tremor_handToNose_right.fileLocation.items),] %>%
  dplyr::select(recordId, deviceMotion_tremor_handToNose_right.fileLocation.items)

tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[1:500,]
tremor_features_right_1 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "deviceMotion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()
# 
tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[501:1000,]
tremor_features_right_2 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "deviceMotion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()
# 
tremor.tbl.meta.noNA <- tremor.tbl.meta.noNA.right[1001:nrow(tremor.tbl.meta.noNA.right),]
tremor_features_right_3 <- extractTremorFeatures(
  dat_ = tremor.tbl.meta.noNA,
  column_ = "deviceMotion_tremor_handToNose_right.fileLocation.items",
  runParallel_ = runParallel)
gc()
# 

tremor_features_right <- rbind(tremor_features_right_1, tremor_features_right_2,
                              tremor_features_right_3)

# Remove the error tremor data frames by filtering on skew.fr
# (look at errorTremorFeatureDataFrame)
tremor_features_right <- tremor_features_right %>%
  dplyr::filter(skew.fr != -88888) %>% 
  dplyr::select(-deviceMotion_tremor_handToNose_right.fileLocation.items)
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
thisFileName <- "featureExtraction/tremorFeatures_mpower_controls.R" # location of file inside github repo
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract tremor features for age matched mpower controls"
activityDescription = "Extract tremor features from tremor activity-v5"

# upload to Synapse, left hand features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "tremorFeatures_handToNoseLeft_mpower_controls.tsv" # name your file
write.table(tremor_features_left, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = tremor.tbl.id,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mhealthtools"))
unlink(OUTPUT_FILE)

# upload to Synapse, right hand features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "tremorFeatures_handToNoseRight_mpower_controls.tsv" # name your file
write.table(tremor_features_right, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = tremor.tbl.id,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mhealthtools"))
unlink(OUTPUT_FILE)
