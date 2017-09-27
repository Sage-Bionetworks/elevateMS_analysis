# elevateMS walking feature extraction

# load all necessary libraries
library(synapseClient)
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
library(githubr) # to install githubr run: library(devtools); devtools::install_github("brian-bot/githubr")
library(mpowertools) # to install mpowertools run: library(devtools); devtools::install_github("Sage-Bionetworks/mpowertools")

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

actv_walking <- merge(actv_walking,outbound_Walking_json_files, by.x="deviceMotion_walking_outbound.json.items", by.y="outbound_Walking_json_fileId", all=TRUE)

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
registerDoMC(detectCores() - 2)

walkFeatures <-
  ddply(
    .data = actv_walking, .variables = colnames(actv_walking),
    .fun = function(row) {
      getWalkFeatures(row$outbound_Walking_json_file)
    },
    .parallel = TRUE
  )


# Only keep the non-redundant data
walkingFeatures <- walkFeatures %>% filter(recordId %in% selected_records)

## Remove unnecessary columns
# columns_to_drop <- c("accelerometer_walking_outbound.json.items",
#                      "pedometer_walking_outbound.json.items",
#                      "accelerometer_walking_rest.json.items",
#                      "deviceMotion_walking_rest.json.items",     
#                      "metadata.json.scheduledActivityGuid",
#                      "metadata.json.taskRunUUID",
#                      "metadata.json.startDate",                  
#                      "metadata.json.startDate.timezone",
#                      "metadata.json.endDate",
#                      "metadata.json.endDate.timezone",           
#                      "metadata.json.dataGroups",
#                      "metadata.json.taskIdentifier")
# cols_to_keep <- !colnames(walkingFeatures) %in% columns_to_drop
# walkingFeatures <- walkingFeatures[, cols_to_keep]

# View the data
# View(walkingFeatures)

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