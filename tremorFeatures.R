# elevateMS Tremor feature extraction 

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

# set system environment to UTC
Sys.setenv(TZ='GMT')

# Query table of interest (tremor features are from tremor v5)
INPUT_TREMOR_TABLE_SYNID = "syn10278767"
actv_tremor_syntable <- synTableQuery(paste0('SELECT * FROM ', INPUT_TREMOR_TABLE_SYNID))
actv_tremor <- actv_tremor_syntable@values
actv_tremor$idx <- rownames(actv_tremor)

# timestamp shenanigans
actv_tremor$createdOn <- as.POSIXct(actv_tremor$createdOn, tz="UTC")
actv_tremor$createdOn <- format_iso_8601(actv_tremor$createdOn)

#saving for later
selected_records <- actv_tremor$recordId

######################
# Download JSON Files
######################

nose_right <- synDownloadTableColumns(actv_tremor_syntable, "ac4_motion_tremor_handToNose_right.json.items")
nose_right <- data.frame(ac4_motion_tremor_handToNose_right.json.items=names(nose_right), nose_right_file=as.character(nose_right))
actv_tremor <- merge(actv_tremor, nose_right, by="ac4_motion_tremor_handToNose_right.json.items", all=T)

nose_left <- synDownloadTableColumns(actv_tremor_syntable, "ac4_motion_tremor_handToNose_left.json.items")
nose_left <- data.frame(ac4_motion_tremor_handToNose_left.json.items=names(nose_left), nose_left_file=as.character(nose_left))
actv_tremor <- merge(actv_tremor, nose_left, by="ac4_motion_tremor_handToNose_left.json.items", all=T)

#############
# Feature Extraction
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
registerDoMC(detectCores() - 2)

# extract hand to nose features
noseRightFeatures <- ddply(.data=actv_tremor, .variables=colnames(actv_tremor), .parallel=runParallel,
                           .fun = function(row) { tryCatch({mpowertools::getWalkFeatures(as.character(row$nose_right_file))},
                                                           error = function(err){
                                                             print(paste0('Unable to process ', row$nose_right_file))
                                                             print(err) })  
                           })

noseLeftFeatures <- ddply(.data=actv_tremor, .variables=colnames(actv_tremor), .parallel=runParallel,
                          .fun = function(row) { tryCatch({mpowertools::getWalkFeatures(as.character(row$nose_left_file))},
                                                          error = function(err){
                                                            print(paste0('Unable to process ', row$nose_left_file))
                                                            print(err) })  
                          })

# only keep the non-redundant data
noseRightFeatures <- noseRightFeatures %>% filter(recordId %in% selected_records)
noseLeftFeatures <- noseLeftFeatures %>% filter(recordId %in% selected_records)

#############
# Final Data
#############

# upload your file to Synapse with provenance
# to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html

## Get commits from github 
thisFileName <- "tremorFeatures.R"
thisRepo <- getRepo(repository = "Sage-Bionetworks/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract tremor features"
activityDescription = "Extract tremor features from tremor activity v5"

# upload to Synapse
OUTPUT_FOLDER_ID <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "TremorNoseRightFeatures.tsv" # name your file
write.table(noseRightFeatures, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=OUTPUT_FOLDER_ID),
         activityName = activityName,
         activityDescription = activityDescription,
         used = INPUT_TREMOR_ACTIVITY_TABLE_SYNID,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mpowertools"))

unlink(OUTPUT_FILE)

OUTPUT_FOLDER_ID <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "TremorNoseLeftFeatures.tsv" # name your file
write.table(noseLeftFeatures, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=OUTPUT_FOLDER_ID),
         activityName = activityName,
         activityDescription = activityDescription,
         used = INPUT_TREMOR_ACTIVITY_TABLE_SYNID,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mpowertools"))

unlink(OUTPUT_FILE)

