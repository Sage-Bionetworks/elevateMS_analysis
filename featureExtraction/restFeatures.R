# elevateMS rest feature extraction

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

# Query table of interest (rest features are from walking features v2)
INPUT_REST_ACTIVITY_TABLE_SYNID = 'syn10278766'
actv_rest_syntable <- synTableQuery(paste0("SELECT * FROM ", INPUT_REST_ACTIVITY_TABLE_SYNID))
actv_rest <- actv_rest_syntable@values
actv_rest$idx <- rownames(actv_rest)

# timestamp shenanigans
actv_rest$createdOn <- as.POSIXct(actv_rest$createdOn, tz="UTC")
actv_rest$createdOn <- format_iso_8601(actv_rest$createdOn)

#saving for later
actvRest_selected_rows <- actv_rest$idx

######################
# Download JSON Files
######################
# rest JSON files
rest_json_files <- synDownloadTableColumns(actv_rest_syntable, "deviceMotion_walking_rest.json.items")
rest_json_files <- data.frame(rest_json_fileId =names(rest_json_files),
                              rest_json_file = as.character(rest_json_files))
actv_rest <- merge(actv_rest,rest_json_files, by.x="deviceMotion_walking_rest.json.items", by.y="rest_json_fileId", all=T)


# add rest json files columns
actv_rest <- actv_rest %>% mutate(rest_json_file = as.character(rest_json_file))

#only use the selected non-redundant data
actv_rest <- actv_rest %>% filter(idx %in% actvRest_selected_rows)

#############
# Feature Extraction
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
registerDoMC(detectCores() - 2)

# extract Rest features
restFeaturesDf <- ddply(.data=actv_rest, 
                        .variables=colnames(actv_rest), 
                        .parallel=T,
                        .fun = function(row) { 
                          getRestFeatures(row$rest_json_file)  
                        })
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