# elevateMS tapping feature extraction
rm(list=ls())
# load all necessary libraries
library(install.load)
install_load(c('plyr', 'dplyr', 'doMC', 'jsonlite', 'parallel', 'tidyr', 'lubridate'))
install_load(c('stringr', 'sqldf', 'parsedate', 'synapser'))
library(githubr) # to install githubr run: library(devtools); devtools::install_github("brian-bot/githubr")
# to install mpowertools run: 
#library(devtools)
devtools::install_github("Sage-Bionetworks/mhealthtools")
install_load("mhealthtools")


# login to Synapse
synLogin()

# Query table of interest, tapping activity v2
INPUT_TAPPING_ACTIVITY_TABLE_SYNID = 'syn10278765'
actv_tapping_syntable <- synTableQuery(paste0("SELECT * FROM ", INPUT_TAPPING_ACTIVITY_TABLE_SYNID))
actv_tapping <- actv_tapping_syntable$asDataFrame()
actv_tapping$idx <- rownames(actv_tapping)

# timestamp shenanigans
actv_tapping$createdOn <- lubridate::as_datetime(actv_tapping$createdOn/1000)
#actv_tapping$createdOn <- format_iso_8601(actv_tapping$createdOn)

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


readTappingFile <- function(tappingJsonFile) {
  tryCatch({
    tapData <- jsonlite::fromJSON(tappingJsonFile)
    list(data = tapData, error = FALSE)
  }, error = function(err) {
    list(data = NA, error = TRUE)
  })
}

shapeTappingData <- function(tapData) {
  tapData <- dplyr::mutate(tapData, TapCoordinate = stringr::str_replace_all(TapCoordinate,"[{}}]", "")) %>%
    tidyr::separate(TapCoordinate,into = c("X", "Y"), sep = ",") %>%
    dplyr::mutate(x = as.numeric(X),y = as.numeric(Y)) %>%
    dplyr::mutate(t = TapTimeStamp,buttonid = TappedButtonId) %>%
    dplyr::select(t,x, y, buttonid)
  return(tapData)
}

createTappingFeaturesErrorResult <- function(error) {
  df <- data.frame(t(c(rep(NA, 42), error)))
  colnames(df) <- c("meanTapInter", "medianTapInter",
                    "iqrTapInter", "minTapInter", "maxTapInter",
                    "skewTapInter", "kurTapInter", "sdTapInter",
                    "madTapInter", "cvTapInter", "rangeTapInter",
                    "tkeoTapInter", "ar1TapInter", "ar2TapInter",
                    "fatigue10TapInter", "fatigue25TapInter",
                    "fatigue50TapInter", "meanDriftLeft",
                    "medianDriftLeft", "iqrDriftLeft", "minDriftLeft",
                    "maxDriftLeft", "skewDriftLeft", "kurDriftLeft",
                    "sdDriftLeft", "madDriftLeft", "cvDriftLeft",
                    "rangeDriftLeft", "meanDriftRight",
                    "medianDriftRight", "iqrDriftRight",
                    "minDriftRight", "maxDriftRight", "skewDriftRight",
                    "kurDriftRight", "sdDriftRight", "madDriftRight",
                    "cvDriftRight", "rangeDriftRight", "numberTaps",
                    "buttonNoneFreq", "corXY", "error")
  df
}


getTappingFeatures <- function(tappingJsonFile, depressThr=20, removeDups=T) {
  tapData <- readTappingFile(tappingJsonFile)
  error <- tapData$error
  tapData <- tapData$data
  if (error == T) {
    print(tappingJsonFile)
    tapFeatures <- createTappingFeaturesErrorResult("unable to read JSON file")
  } else if (is.data.frame(tapData) == F) {
    print(tappingJsonFile)
    tapFeatures <- createTappingFeaturesErrorResult("expected data frame object after reading tapping json file")
  } else if (nrow(tapData) < 5) {
    print(tappingJsonFile)
    tapFeatures <- createTappingFeaturesErrorResult("raw tapping data has less than 5 rows")
  } else {
    # shape data
    tapData <- shapeTappingData(tapData)
    mhealthtools::get_tapping_features(tapData)
  }
}

# extract LEFT hand tapping features
left_hand_tapping_features <-
  ddply(
    .data = actv_tapping,
    .variables = colnames(actv_tapping),
    .parallel = T,
    .fun = function(row) {
      getTappingFeatures(row$tapping_right_json_file)
    }
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
      getTappingFeatures(row$tapping_right_json_file)
    }
  ) %>%
  mutate(hand = 'right')

tappingFeatures <-
  rbind(left_hand_tapping_features, right_hand_tapping_features)

setdiff(colnames(left_hand_tapping_features), colnames(right_hand_tapping_features))

# View the data
View(tappingFeatures)

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
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mhealthtools"))
unlink(OUTPUT_FILE)
