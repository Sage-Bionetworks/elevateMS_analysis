# elevateMS tapping feature extraction

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

# Query table of interest, tapping activity v2
INPUT_TAPPING_ACTIVITY_TABLE_SYNID = 'syn10278765'
actv_tapping_syntable <- synTableQuery(paste0("SELECT * FROM ", INPUT_TAPPING_ACTIVITY_TABLE_SYNID))
actv_tapping <- actv_tapping_syntable@values
actv_tapping$idx <- rownames(actv_tapping)

# timestamp shenanigans
actv_tapping$createdOn <- as.POSIXct(actv_tapping$createdOn, tz="UTC")
actv_tapping$createdOn <- format_iso_8601(actv_tapping$createdOn)

# save for later
selected_records <- actv_tapping$recordId

######################
# Download JSON Files
######################
# download right hand tapping files
rightHand_tappingJsonFiles <- synDownloadTableColumns(actv_tapping_syntable, "tapping_right.json.TappingSamples")
rightHand_tappingJsonFiles <-
  data.frame(
    tapping_right_json_fileId = names(rightHand_tappingJsonFiles),
    tapping_right_json_file = as.character(rightHand_tappingJsonFiles)
  )
actv_tapping <- merge(actv_tapping,rightHand_tappingJsonFiles, by.x="tapping_right.json.TappingSamples", by.y="tapping_right_json_fileId", all=T)

#download left hand tapping files
leftHand_tappingJsonFiles <- synDownloadTableColumns(actv_tapping_syntable, "tapping_left.json.TappingSamples")
leftHand_tappingJsonFiles <- data.frame(tapping_left_json_fileId =names(leftHand_tappingJsonFiles),
                                        tapping_left_json_file = as.character(leftHand_tappingJsonFiles))
actv_tapping <- merge(actv_tapping,leftHand_tappingJsonFiles, by.x="tapping_left.json.TappingSamples", by.y="tapping_left_json_fileId", all=T)

actv_tapping <- actv_tapping %>% mutate(tapping_right_json_file = as.character(tapping_right_json_file),
                                        tapping_left_json_file = as.character(tapping_left_json_file))

# Only keep the non-redundant data
actv_tapping <- actv_tapping %>% filter( recordId %in% selected_records)

# # remove duplicates
# actv_tapping <- actv_tapping %>%
#   distinct(outbound_Tapping_json_file, .keep_all = TRUE)

#############
# Feature Extraction
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
registerDoMC(detectCores() - 2)

# extract LEFT hand tapping features
left_hand_tapping_features <-
  ddply(
    .data = actv_tapping,
    .variables = colnames(actv_tapping),
    .parallel = T,
    .fun = function(row)
      mpowertools::getTappingFeatures(row$tapping_left_json_file)
  )

left_hand_tapping_features <-
  left_hand_tapping_features %>% mutate(hand = 'left')

# extract RIGHT hand tapping features
right_hand_tapping_features <-
  ddply(
    .data = actv_tapping,
    .variables = colnames(actv_tapping),
    .parallel = T,
    .fun = function(row) {
      mpowertools::getTappingFeatures(row$tapping_right_json_file)
    }
  ) %>%
  mutate(hand = 'right')

tappingFeatures <-
  rbind(left_hand_tapping_features, right_hand_tapping_features)

# View the data
# View(tappingFeatures)

#############
# Final Data
#############

# upload file to Synapse with provenance
# to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html

## Get commits from github 
thisFileName <- "tappingFeatures.R" # name of file in github
thisRepo <- getRepo(repository = "Sage-Bionetworks/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract tapping features"
activityDescription = "Extract tapping features from tapping activity v2"

# upload to Synapse
OUTPUT_FOLDER_ID <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "TappingFeatures.tsv" # name your file
write.table(tappingFeatures, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=OUTPUT_FOLDER_ID),
         activityName = activityName,
         activityDescription = activityDescription,
         used = INPUT_TAPPING_ACTIVITY_TABLE_SYNID,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mpowertools"))
unlink(OUTPUT_FILE)
