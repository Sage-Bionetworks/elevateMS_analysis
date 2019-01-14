# elevateMS walking feature extraction

#CLEAN START
rm(list=ls())
library("install.load")
# load all necessary libraries
install_load('synapseClient', 'plyr', 'dplyr' ,'ggplot2', 'doMC', 'jsonlite')
install_load('parallel', 'tidyr', 'lubridate' , 'stringr', 'sqldf')
# to install githubr run: library(devtools); devtools::install_github("brian-bot/githubr")
# to install mpowertools run: library(devtools); devtools::install_github("Sage-Bionetworks/mpowertools")
install_load('parsedate', 'githubr', 'mpowertools' )
# login to Synapse
synapseLogin()

# Query table of interest, walking v2
INPUT_WALKING_ACTIVITY_TABLE_SYNID = 'syn10278766'
actv_walking_syntable <- synTableQuery(paste0("SELECT * FROM ", INPUT_WALKING_ACTIVITY_TABLE_SYNID))
actv_walking <- actv_walking_syntable@values
actv_walking$idx <- rownames(actv_walking)

# timestamp shenanigans
actv_walking$createdOn <- as.POSIXct(actv_walking$createdOn, tz="UTC")
actv_walking$createdOn <- format_iso_8601(actv_walking$createdOn)

# save for later
selected_records <- actv_walking$recordId

######################
# Download JSON Files
######################
#download outbound walking json files
outbound_Walking_json_files <- synDownloadTableColumns(actv_walking_syntable, "deviceMotion_walking_outbound.json.items")
outbound_Walking_json_files <- data.frame(outbound_Walking_json_fileId =names(outbound_Walking_json_files),
                                          outbound_Walking_json_file = as.character(outbound_Walking_json_files))

outbound_Walking_json_files <- outbound_Walking_json_files %>%
  distinct(outbound_Walking_json_file, .keep_all = TRUE)

actv_walking <- merge(actv_walking,outbound_Walking_json_files, by.x="deviceMotion_walking_outbound.json.items", 
                      by.y="outbound_Walking_json_fileId", all=TRUE)

# add walking json files columns
actv_walking <- actv_walking %>% mutate(outbound_Walking_json_file = as.character(outbound_Walking_json_file))

# remove duplicates
actv_walking <- actv_walking %>%
  distinct(outbound_Walking_json_file, .keep_all = TRUE)

#############
# Feature Extraction
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
registerDoMC(detectCores() - 1)

walkFeatures <-
  ddply(
    .data = actv_walking, .variables = colnames(actv_walking),
    .fun = function(row) {
      mpowertools::getWalkFeatures(row$outbound_Walking_json_file)
    },
    .parallel = TRUE
  )

# Only keep the non-redundant data
walkingFeatures <- walkFeatures %>% filter(recordId %in% selected_records)


##### Pedo Features
outbound_pedometer_json_files <- synDownloadTableColumns(actv_walking_syntable, "pedometer_walking_outbound.json.items")
outbound_pedometer_json_files <- data.frame(outbound_pedometer_json_fileId =names(outbound_pedometer_json_files),
                                            outbound_pedometer_json_file = as.character(outbound_pedometer_json_files)) %>%
  dplyr::mutate(outbound_pedometer_json_fileId = as.character(outbound_pedometer_json_fileId),
                outbound_pedometer_json_file = as.character(outbound_pedometer_json_file))
outbound_pedometer_json_files <- outbound_pedometer_json_files %>% 
  distinct(outbound_pedometer_json_file, .keep_all = TRUE)

registerDoMC(detectCores()-2)
#extract pedometere features
pedoFeatures <- ddply(.data=outbound_pedometer_json_files, 
                      .variables=colnames(outbound_pedometer_json_files), 
                      .parallel=T,
                      .fun = function(row) { 
                        tryCatch({ mpowertools::getPedometerFeatures(row$outbound_pedometer_json_file)},
                                 error = function(err){
                                   print(paste0('Unable to process ', row$outbound_pedometer_json_file))
                                   stop(err) })  
                      })
pedoFeatures['error_pedometer_features'] = pedoFeatures$error
pedoFeatures$error <- NULL
pedoFeatures$outbound_pedometer_json_file <- NULL


dim(actv_walking)
dim(walkFeatures)


### Merge Walking and Pedometer features
walkingFeatures <- merge(walkingFeatures, pedoFeatures, all=T,
                         by.x="pedometer_walking_outbound.json.items",
                         by.y="outbound_pedometer_json_fileId")

#############
# Final Data
#############
# upload your file to Synapse with provenance
# to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html

## Get commits from github 
thisFileName <- "walkingFeatures.R"
thisRepo <- getRepo(repository = "Sage-Bionetworks/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

#the above code dint work so temp hack
thisFile <- "https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/featureExtraction/walkingFeatures.R"

# name and describe this activity
activityName = "Extract walking features"
activityDescription = "Extract walking features from walking activity v2"

# upload to Synapse
OUTPUT_FOLDER_ID <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "WalkingFeatures.tsv" # name your file
write.table(walkingFeatures, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=OUTPUT_FOLDER_ID),
         activityName = activityName,
         activityDescription = activityDescription,
         Activity(used = INPUT_WALKING_ACTIVITY_TABLE_SYNID,
                  executed = list(thisFile, "https://github.com/Sage-Bionetworks/mpowertools")))

unlink(OUTPUT_FILE)
