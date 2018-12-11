# Subsetting elevateMS features to the most important features in mPower data
rm(list = ls())
gc()

## tremor feature subsetting
library(tidyverse)
library(synapser)
synapser::synLogin()

## Downlaod the important features from mpower analysis
mpower_features <- synapser::synGet('syn17088603')$path %>% read.csv(sep = '\t')

## Get kinetic features (hand to nose test)
mpower_features <- mpower_features %>% dplyr::filter(assay == 'kinetic')

## Download all the elevateMS features
tremor_features_left <- synapser::synGet('syn12104398')$path %>% read.csv(sep='\t')
tremor_features_right <- synapser::synGet('syn12104396')$path %>% read.csv(sep='\t')

## Get the covariate columns
feature_list <- c(grep('.fr', colnames(tremor_features_left)),
                  grep('.tm', colnames(tremor_features_left)),
                  grep('EnergyInBand', colnames(tremor_features_left)))

covariate_list <- setdiff(colnames(tremor_features_left),
                          colnames(tremor_features_left)[feature_list])

subsetted_feature_list <- mpower_features$Feature %>% droplevels() 

## Subsetted feature and covariate set
tremor_features_left_subset <- tremor_features_left[ ,subsetted_feature_list]
tremor_features_left_subset <- tremor_features_left_subset %>% 
  cbind(tremor_features_left$recordId)

tremor_features_right_subset <- tremor_features_right[, subsetted_feature_list]
tremor_features_right_subset <- tremor_features_right_subset %>% 
  cbind(tremor_features_right$recordId)

# upload to Synapse, left hand features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "tremorFeatures_handToNoseLeft_subset.tsv" # name your file
write.table(tremor_features_left_subset, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id))
unlink(OUTPUT_FILE)

# upload to Synapse, right hand features
synapse.folder.id <- "syn10140063" # synId of folder to upload your file to
OUTPUT_FILE <- "tremorFeatures_handToNoseRight_subset.tsv" # name your file
write.table(tremor_features_right_subset, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id))
unlink(OUTPUT_FILE)
