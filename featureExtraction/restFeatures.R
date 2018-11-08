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
processRestFile <- function(restJsonFileLocation){
  # Read the Json File and process it into mhealthtools format
  
  restData <-   tryCatch({
    rest_data <- jsonlite::fromJSON(as.character(restJsonFileLocation))
    
    accel_data <- rest_data$userAcceleration
    accel_data$t <- rest_data$timestamp - rest_data$timestamp[1]
    
    gyro_data <- rest_data$rotationRate
    gyro_data$t <- rest_data$timestamp - rest_data$timestamp[1]
    
    grav_data <- rest_data$gravity
    grav_data$t <- rest_data$timestamp - rest_data$timestamp[1]
    
    rest_data <- list(accelerometer = accel_data,
                      gyroscope = gyro_data,
                      gravity = grav_data)
  }, error = function(err) {
    rest_data <- list(accelerometer = NA,
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

#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############
# login to Synapse
synapser::synLogin()

# set system environment to UTC
Sys.setenv(TZ='GMT')

rest.tbl.id = 'syn10278766' # Walking Activity-v2
rest.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", rest.tbl.id, " WHERE healthCode = 'adeca5c5-856d-49e8-b3d9-3402b961c05d'"))
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

restJsonLocation <- rest.tbl.meta$deviceMotion_walking_rest.fileLocation.items[1]
# extract Rest features
rest_features <- featuresFromColumn(
  dat = rest.tbl.meta,
  column = "deviceMotion_walking_rest.json.items",
  processingFunction = function(restJsonLocation){
    restData <- processRestFile(restJsonLocation)
    samplingRate <- mhealthtools:::get_sampling_rate(restData$accelerometer)
    restFeatures <- mhealthtools::get_rest_features(
        accelerometer_data = restData$accelerometer,
        gyroscope_data = restData$gyroscope,
        gravity_data = restData$gravity,
        funs = mhealthtools:::default_kinematic_features(samplingRate)
    ) 
    restFeatures <- restFeatures$extracted_features
    restFeatures <- restFeatures %>% 
      dplyr::select(-window_index, -window_start_time,-window_end_time, -window) %>% 
      unique() %>% 
      tidyr::gather(Feature, Value,-sensor, -measurementType, -axis) %>%
      dplyr::group_by(Feature, sensor, measurementType, axis) %>% 
      dplyr::summarise(iqr = stats::IQR(Value, na.rm = T),
                       md = stats::median(Value, na.rm = T))
  return(restFeatures)  
  },
  parallel = runParallel 
)
  
  
  
#############
# Final Data
#############

# upload file to Synapse with provenance
# to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html

## Get commits from github 
thisFileName <- "restFeatures.R" # name of file in github
thisRepo <- getRepo(repository = "Sage-Bionetworks/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract tapping features"
activityDescription = "Extract tapping features from tapping activity v2"

# upload to Synapse
OUTPUT_FOLDER_ID <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "RestFeatures.tsv" # name your file
write.table(restFeaturesDf, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=OUTPUT_FOLDER_ID),
         activityName = activityName,
         activityDescription = activityDescription,
         used = INPUT_REST_ACTIVITY_TABLE_SYNID,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mpowertools"))
unlink(OUTPUT_FILE)




################# TEST ZONE ######################
kinetic.ftr = ftrs %>%
  tidyr::unite(rid, recordId, Assay, sensor, measurementType, IMF, axis, Window, sep = '.') %>%
  dplyr::select(-contains('EnergyInBand')) %>%
  dplyr::left_join(energy.ftr.cmbn) %>%
  tidyr::separate(rid, c('recordId', 'Assay', 'sensor', 'measurementType', 'IMF', 'axis', 'Window'), sep = '\\.') %>%
  dplyr::select(-recordId, -Assay, -axis, -Window) %>%
  tidyr::gather(Feature, Value, -healthCode, -gender, -MS, -sensor, -measurementType, -IMF) %>%
  dplyr::group_by(Feature, healthCode, gender, MS, sensor, measurementType, IMF) %>%
  dplyr::summarise(iqr = stats::IQR(Value, na.rm = T),
                   md = stats::median(Value, na.rm = T))

aa1 <- aa 
aa <- aa1 %>% 
  tidyr::gather(Feature, Value,-sensor, -measurementType, -axis) %>%
  dplyr::group_by(Feature, sensor, measurementType, axis) %>% 
  dplyr::summarise(iqr = stats::IQR(Value, na.rm = T),
                   md = stats::median(Value, na.rm = T))
  
