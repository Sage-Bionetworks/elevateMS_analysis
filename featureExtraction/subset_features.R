############################################################################
# ElevateMS project
# Purpose: Subset the summarized features to selected features (using mPower feature ranking)
# Author: Meghasyam Tummalacherla
############################################################################

###########################################################
## Required libraries
###########################################################
library(tidyverse)
library(synapser)
library(githubr)
synapser::synLogin()

###########################################################
## Download and set up features for elevateMS
###########################################################
summarized.ftrs.elevateMS.id = 'syn18879962'
summarized.ftrs.elevateMS = read.csv(synapser::synGet(summarized.ftrs.elevateMS.id)$path,
                                     sep = '\t')
summarized.ftrs.elevateMS.id.record = 'syn19964391'
summarized.ftrs.elevateMS.record = read.csv(synapser::synGet(summarized.ftrs.elevateMS.id.record)$path,
                                            sep = '\t')

tremor.tbl.id.MS = 'syn10278767' # Tremor Activity-v5
tremor.tbl.syn.MS <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id.MS))
tremor.tbl.MS <- tremor.tbl.syn.MS$asDataFrame()

metadata.columns <- colnames(tremor.tbl.MS)
metadata.columns <- metadata.columns[grepl('metadata',metadata.columns)]

metadata.MS <- tremor.tbl.MS %>% 
  dplyr::select('healthCode','recordId','createdOn','createdOnTimeZone',
                'dataGroups',metadata.columns) %>% 
  unique()

all.used.ids = c(summarized.ftrs.elevateMS.id, summarized.ftrs.elevateMS.id.record,
                 tremor.tbl.id.MS)

###########################################################
## Download and set up features for age matched mpower controls
###########################################################
summarized.ftrs.mpower.id = 'syn19165094'
summarized.ftrs.mpower = read.csv(synapser::synGet(summarized.ftrs.mpower.id)$path,
                                  sep = '\t')
summarized.ftrs.mpower.id.record = 'syn19964696'
summarized.ftrs.mpower.record = read.csv(synapser::synGet(summarized.ftrs.mpower.id.record)$path,
                                         sep = '\t')
tremor.tbl.id.mpower = 'syn10676309' # Tremor Activity-v5
tremor.tbl.syn.mpower <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id.mpower))
tremor.tbl.mpower <- tremor.tbl.syn.mpower$asDataFrame()

metadata.columns <- colnames(tremor.tbl.mpower)
metadata.columns <- metadata.columns[grepl('metadata',metadata.columns)]

metadata.mpower <- tremor.tbl.mpower %>% 
  dplyr::select('healthCode','recordId','createdOn','createdOnTimeZone',
                'dataGroups',metadata.columns) %>% 
  unique()

all.used.ids <- c(all.used.ids, summarized.ftrs.mpower.id, summarized.ftrs.mpower.id.record,
                  tremor.tbl.id.mpower)

##########################################################
## Download feature ranking and subset features
###########################################################
ranking.syn.id = 'syn12038526' # AllRanking.Rdata (feature ranking  postural, kinetic and rest for mpower)
load(synapser::synGet(ranking.syn.id)$path)
all.used.ids <- c(all.used.ids, ranking.syn.id)

# find important features
nFeatures = 100 # max number of features to choose per assay
kineticFeaturesIMF1 <- ranka[,3] %>% na.omit() 
kineticFeaturesIMF2 <- ranka[,4] %>% na.omit() 
maxRank <- max(c(kineticFeaturesIMF1,kineticFeaturesIMF2))
kineticFeaturesIMF1 <- kineticFeaturesIMF1[kineticFeaturesIMF1 > (maxRank-nFeatures)] %>% names()
kineticFeaturesIMF2 <- kineticFeaturesIMF2[kineticFeaturesIMF2 > (maxRank-nFeatures)] %>% names()

kineticFeaturesToSelect <- c()
for (ftr in colnames(summarized.ftrs.elevateMS)){
  ftr1 <- gsub('IMF1.','',ftr)
  # print(ftr1)
  if(ftr1 %in% kineticFeaturesIMF1){
    kineticFeaturesToSelect <- c(kineticFeaturesToSelect,ftr)
  }
  ftr1 <- gsub('IMF2.','',ftr)
  # print(ftr1)
  if(ftr1 %in% kineticFeaturesIMF2){
    kineticFeaturesToSelect <- c(kineticFeaturesToSelect,ftr)
  }
}

# metadata
metadata <- dplyr::full_join(metadata.mpower, metadata.MS) %>% 
  unique()

# Subset features
hc.summarized.features <- dplyr::full_join(summarized.ftrs.elevateMS, summarized.ftrs.mpower) %>% 
  dplyr::select(c('healthCode',kineticFeaturesToSelect)) %>% 
  dplyr::left_join(metadata %>% dplyr::select(healthCode, dataGroups)) %>%
  unique() %>% 
  na.omit()

recordId.summarized.features <- dplyr::full_join(summarized.ftrs.elevateMS.record, summarized.ftrs.mpower.record) %>% 
  dplyr::select(c('healthCode','recordId','Assay',kineticFeaturesToSelect)) %>% 
  unique() %>% 
  dplyr::left_join(metadata) %>% 
  unique()

#############
# Upload data to Synapse
#############
# # upload file to Synapse with provenance
# # to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html
# 
# ## Github link
# # Copy paste the github token string and store it as 'github_token.txt' file
# # A github token is required to access the elevateMS_analysis repository as it is private
# gtToken = '~/github_token.txt'
# setGithubToken(as.character(read.table(gtToken)$V1))
# thisFileName <- "featureExtraction/subset_features.R" # location of file inside github repo
# thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", 
#                     ref="branch", 
#                     refName="master")
# thisFile <- getPermlink(repository = thisRepo, repositoryPath=thisFileName)

# name and describe this activity
activityName = "Subset to important features"
activityDescription = "Subset the whole feature file into a smaller subset using feature ranking from mPower"

# upload to Synapse, healthCode wise summarized features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "TremorFeatures_healthCode.tsv" # name your file
write.table(hc.summarized.features, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = "https://github.com/itismeghasyam/elevateMS_analysis/blob/master/featureExtraction/subset_features.R")
unlink(OUTPUT_FILE)

# upload to Synapse, recordwise summarized features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "TremorFeatures_recordId.tsv" # name your file
write.table(recordId.summarized.features, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = "https://github.com/itismeghasyam/elevateMS_analysis/blob/master/featureExtraction/subset_features.R")
unlink(OUTPUT_FILE)

