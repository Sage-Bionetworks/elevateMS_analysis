############################################################################
# ElevateMS project
# Purpose: Extract Tapping features
# Author: Abhishek Pratap, Meghasyam Tummalacherla
############################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(install.load)
install.load::install_load(c('plyr', 'dplyr', 'doMC', 'jsonlite', 'parallel', 'tidyr', 'lubridate'))
install.load::install_load(c('stringr', 'sqldf', 'parsedate', 'synapser'))
library(githubr) 
# devtools::install_github("brian-bot/githubr")
library("mhealthtools")
# devtools::install_github("Sage-Bionetworks/mhealthtools")
## Synapse Login
synapser::synLogin()

#############
# Required functions
##############
processTappingFile <- function(tappingJsonFileLocation){
  tapData <-   tryCatch({
    tap_data <- jsonlite::fromJSON(as.character(tappingJsonFileLocation)) %>% 
      dplyr::mutate(TapCoordinate = stringr::str_replace_all(TapCoordinate,"[{}}]", "")) %>%
      tidyr::separate(TapCoordinate,into = c("X", "Y"), sep = ",") %>%
      dplyr::mutate(x = as.numeric(X),y = as.numeric(Y)) %>%
      dplyr::mutate(t = TapTimeStamp,buttonid = TappedButtonId) %>%
      dplyr::select(t,x, y, buttonid)
  }, error = function(err) {
    NA # NAs are handled in mhealthtools
  })
}

#############
# Download Synapse Table, and select and download required columns, figure out filepath locations
#############
tapping.tbl.id = 'syn10278765' # Tapping activity v2
tapping.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", tapping.tbl.id))
tapping.tbl <- tapping.tbl.syn$asDataFrame()

## Convert createdOn into an understandable datetime format
tapping.tbl$createdOn <- lubridate::as_datetime(tapping.tbl$createdOn/1000)

## Account for timezone change, if column is in local time
# tapping.tbl$createdOn <- tapping.tbl$createdOn - 60*60*as.numeric(tapping.tbl$createdOnTimeZone)/100

## Download required columns i,e the JSON files
columnsToDownload = c("tapping_left.json.TappingSamples",
                      "tapping_right.json.TappingSamples") 

tapping.json.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(tapping.tbl.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('.json','.fileLocation', col.name)))
})

## Convert column format to be able to do join and merge filelocations
tapping.tbl$tapping_left.json.TappingSamples <- as.character(tapping.tbl$tapping_left.json.TappingSamples)
tapping.tbl$tapping_right.json.TappingSamples <- as.character(tapping.tbl$tapping_right.json.TappingSamples)

tapping.tbl.meta = data.table::rbindlist(list(tapping.tbl %>%
                                                left_join(do.call(cbind, tapping.json.loc))),
                                         use.names = T, fill = T) %>%
  as.data.frame

#############
# Extract Tapping features
##############
if (detectCores() >= 2) {
  runParallel <- TRUE
} else {
  runParallel <- FALSE
}
doMC::registerDoMC(detectCores() - 2)

# extract LEFT hand tapping features
left_hand_tapping_features <-
  plyr::ddply(
    .data = tapping.tbl.meta,
    .variables = colnames(tapping.tbl.meta),
    .parallel = runParallel,
    .fun = function(row) {
      tapData <- processTappingFile(as.character(row$tapping_left.fileLocation.TappingSamples))
      return(mhealthtools::get_tapping_features(tapData))
    }
    ) %>% 
  dplyr::mutate(hand = 'left')

# extract RIGHT hand tapping features
right_hand_tapping_features <-
  plyr::ddply(
    .data = tapping.tbl.meta,
    .variables = colnames(tapping.tbl.meta),
    .parallel = runParallel,
    .fun = function(row) {
      tapData <- processTappingFile(as.character(row$tapping_right.fileLocation.TappingSamples))
      return(mhealthtools::get_tapping_features(tapData))
    }
  ) %>% 
  dplyr::mutate(hand = 'right')

tapping_features <- dplyr::full_join(left_hand_tapping_features, right_hand_tapping_features)

## View the data
# View(tapping_features)

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
thisFileName <- "featureExtraction/tappingFeatures.R" # location of file inside github repo
thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
                    ref="branch", 
                    refName="master")
thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Extract tapping features"
activityDescription = "Extract tapping features from tapping activity v2"

# upload to Synapse
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "TappingFeatures.tsv" # name your file
write.table(tapping_features, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synapser::synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = tapping.tbl.id,
         executed = list(thisFile, "https://github.com/Sage-Bionetworks/mhealthtools"))
unlink(OUTPUT_FILE)
