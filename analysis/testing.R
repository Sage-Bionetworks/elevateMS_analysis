library("synapseClient")
synapseLogin()

library(plyr)
library(dplyr)
library(jsonlite)
library(doParallel)
library(tidyr)
library("lubridate")
library("stringr")
library("tester")
library("doMC")
library("parsedate")

#devtools::install_github("Sage-Bionetworks/mpowertools")
library("mpowertools")


TAPPING_ACTIVITY_TABLE_SYNID = 'syn9765504'
actv_tapping_syntable <- synTableQuery(paste0('SELECT * FROM ', TAPPING_ACTIVITY_TABLE_SYNID))
actv_tapping <- actv_tapping_syntable@values
actv_tapping$idx <- rownames(actv_tapping)