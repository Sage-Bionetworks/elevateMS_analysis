############################################################################
# ElevateMS project
# Purpose: Score PRO Surveys
# Author: Abhishek Pratap, Meghasyam Tummalacherla
############################################################################
# rm(list=ls())
# gc()

##############
# Required libraries
##############
library(synapser)
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
library(githubr) 
# devtools::install_github("brian-bot/githubr")
library(data.table)
## Synapse Login
synapser::synLogin()

##############
# 1. NueroQOL - Congition
##############
cat('Processing NeuroQOL Cognition PROs \n')
#Suvery Norm scores
nQOL_cog_scoring_file <- "syn11223875"
nQOL_cog_scoring <- data.table::fread(synapser::synGet(nQOL_cog_scoring_file)$path, data.table=F)
nQOL_cog_scoring <- nQOL_cog_scoring %>% 
  dplyr::rename('rawScore' = 'Raw Score')

#NueroQOL - Cognition V1
nueroQOL_cog_synTable <- "syn10242477"
nQOLcog.syn <-  synapser::synTableQuery(paste("select * from", nueroQOL_cog_synTable))
nQOLcog <- nQOLcog.syn$asDataFrame()
nQOL_cog_questions_cols <- c('01_reading', '02_slow_thinking', '03_attention',
                             '04_consentration', '05_current', '06_instructions',
                             '07_daily_activities', '08_new_tasks')
nQOLcog['rawScore'] = apply(nQOLcog[, nQOL_cog_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
cat('Before merge', nrow(nQOLcog), 'rows')
nQOLcog <- merge(nQOLcog, nQOL_cog_scoring, all.x=T)
cat('After merge', nrow(nQOLcog), 'rows')

# timeStampCol = 'createdOn'
# timeZoneCol = 'createdOnTimeZone'
# nQOLcog <- insert_study_times(nQOLcog, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)

## Download required columns i,e the JSON files
columnsToDownload = c("rawData") 
cog.zip.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(nQOLcog.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Data','fileLocation', col.name)))
})
nQOLcog$rawData <- as.character(nQOLcog$rawData)
nQOLcog.meta = data.table::rbindlist(list(nQOLcog %>%
                                                dplyr::left_join(do.call(cbind, cog.zip.loc))),
                                         use.names = T, fill = T) %>%
  as.data.frame
nQOLcog.meta$rawfileLocation <- as.character(nQOLcog.meta$rawfileLocation)

## Extract duration of each test
nQOLcog.meta.noNA <- nQOLcog.meta[!is.na(nQOLcog.meta$rawfileLocation),]
nQOLcog.times <- lapply(nQOLcog.meta.noNA$rawfileLocation,
  function(rawzipLocation){
    zipfiles <- tryCatch({utils::unzip(as.character(rawzipLocation))},
                         error = function(err){
                           NA
                         })
    nQOL_cog_questions_cols <- c('01_reading', '02_slow_thinking', '03_attention',
                                 '04_consentration', '05_current', '06_instructions',
                                 '07_daily_activities', '08_new_tasks')
    times.tests <- data.frame(nQOL_cog_questions_cols) %>% 
      data.table::transpose() %>% 
      `colnames<-`(paste0(nQOL_cog_questions_cols,'_time'))
    
    for(i in seq(8)){
      times.tests[i] <- tryCatch({
        temp.json <- jsonlite::fromJSON(as.character(zipfiles[i]))
        temp.time <- as.numeric(lubridate::as_datetime(temp.json$endDate) - lubridate::as_datetime(temp.json$startDate))
      }, error = function(err){
        NA
      })
    }
    unlink(zipfiles)
    total_time <- sum(as.numeric(times.tests), na.rm = T)
    mean_time <- mean(as.numeric(times.tests), na.rm = T)
    times.tests$total_time <- total_time
    times.tests$mean_time <- mean_time
    times.tests$rawfileLocation <- rawzipLocation
    return(times.tests)
  }
) %>% dplyr::bind_rows()

nQOLcog.full <- dplyr::left_join(nQOLcog.meta, nQOLcog.times) %>% 
  dplyr::select(-rawfileLocation)

outFile = 'neuroQOL_Cognition.tsv'
PARENT_FOLDER = 'syn10140063'
write.table(nQOLcog.full, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(nQOL_cog_scoring_file, nueroQOL_cog_synTable),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
unlink(outFile)              
cat('-----------\n')


##############
#2. NueroQOL - Upper Extremity
##############
cat('Processing NeuroQOL Upper Extremity PROs \n')
#Suvery Norm scores
nQOL_uppExtremity_scoring_file <- "syn11223825"
nQOL_uppExtremity_scoring <- data.table::fread(synapser::synGet(nQOL_uppExtremity_scoring_file)$path, data.table=F)
nQOL_uppExtremity_scoring <- nQOL_uppExtremity_scoring %>% 
  dplyr::rename('rawScore' = 'Raw Score')

#NueroQOL - Cognition V1
nQOL_uppExtremity_synTable <- "syn10139320"
nQOL_uppExtremity.syn <-  synapser::synTableQuery(paste("select * from", nQOL_uppExtremity_synTable))
nQOL_uppExtremity <- nQOL_uppExtremity.syn$asDataFrame()
nQOL_uppExtremity_questions_cols <- c('01_lock', '02_brush_teeth', '03_phone',
                         '04_coins', '05_write', '06_zipper',
                         '07_wash', '08_shampoo')
nQOL_uppExtremity['rawScore'] = apply(nQOL_uppExtremity[, nQOL_uppExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
cat('Before merge', nrow(nQOL_uppExtremity), 'rows')
nQOL_uppExtremity <- merge(nQOL_uppExtremity, nQOL_uppExtremity_scoring, all.x=T)
cat('After merge', nrow(nQOL_uppExtremity), 'rows')

# timeStampCol = 'createdOn'
# timeZoneCol = 'createdOnTimeZone'
# nQOL_uppExtremity <- insert_study_times(nQOL_uppExtremity, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)

## Download required columns i,e the JSON files
columnsToDownload = c("rawData") 
cog.zip.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(nQOL_uppExtremity.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Data','fileLocation', col.name)))
})
nQOL_uppExtremity$rawData <- as.character(nQOL_uppExtremity$rawData)
nQOL_uppExtremity.meta = data.table::rbindlist(list(nQOL_uppExtremity %>%
                                            dplyr::left_join(do.call(cbind, cog.zip.loc))),
                                     use.names = T, fill = T) %>%
  as.data.frame
nQOL_uppExtremity.meta$rawfileLocation <- as.character(nQOL_uppExtremity.meta$rawfileLocation)

## Extract duration of each test
nQOL_uppExtremity.meta.noNA <- nQOL_uppExtremity.meta[!is.na(nQOL_uppExtremity.meta$rawfileLocation),]
nQOL_uppExtremity.times <- lapply(nQOL_uppExtremity.meta.noNA$rawfileLocation,
                        function(rawzipLocation){
                          zipfiles <- tryCatch({utils::unzip(as.character(rawzipLocation))},
                                               error = function(err){
                                                 NA
                                               })
                          nQOL_uppExtremity_questions_cols <- c('01_lock', '02_brush_teeth', '03_phone',
                                                                '04_coins', '05_write', '06_zipper',
                                                                '07_wash', '08_shampoo')
                          times.tests <- data.frame(nQOL_uppExtremity_questions_cols) %>% 
                            data.table::transpose() %>% 
                            `colnames<-`(paste0(nQOL_uppExtremity_questions_cols,'_time'))
                          
                          for(i in seq(8)){
                            times.tests[i] <- tryCatch({
                              temp.json <- jsonlite::fromJSON(as.character(zipfiles[i]))
                              temp.time <- as.numeric(lubridate::as_datetime(temp.json$endDate) - lubridate::as_datetime(temp.json$startDate))
                            }, error = function(err){
                              NA
                            })
                          }
                          unlink(zipfiles)
                          total_time <- sum(as.numeric(times.tests), na.rm = T)
                          mean_time <- mean(as.numeric(times.tests), na.rm = T)
                          times.tests$total_time <- total_time
                          times.tests$mean_time <- mean_time
                          times.tests$rawfileLocation <- rawzipLocation
                          return(times.tests)
                        }
) %>% dplyr::bind_rows()

nQOL_uppExtremity.full <- dplyr::left_join(nQOL_uppExtremity.meta, nQOL_uppExtremity.times) %>% 
  dplyr::select(-rawfileLocation)

outFile = 'neuroQOL_uppExtremity.tsv'
PARENT_FOLDER = 'syn10140063'
write.table(nQOL_uppExtremity.full, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(nQOL_uppExtremity_scoring_file, nQOL_uppExtremity_synTable),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
unlink(outFile) 
cat('-----------\n')


##############
#3. NueroQOL - Low Extremity
##############
cat('Processing NeuroQOL Lower Extremity PROs \n')
#Suvery Norm scores
nQOL_lowExtremity_scoring_file <- "syn11223833"
nQOL_lowExtremity_scoring <- data.table::fread(synapser::synGet(nQOL_lowExtremity_scoring_file)$path, data.table=F)
nQOL_lowExtremity_scoring <- nQOL_lowExtremity_scoring %>% 
  dplyr::rename('rawScore' = 'Raw Score') 
  
#NueroQOL - Cognition V1
nQOL_lowExtremity_synTable <- "syn10142944"
nQOL_lowExtremity.syn <-  synapser::synTableQuery(paste("select * from", nQOL_lowExtremity_synTable))
nQOL_lowExtremity <- nQOL_lowExtremity.syn$asDataFrame()
nQOL_lowExtremity_questions_cols <- c('01_toilet', '02_curbs', '03_car',
                                      '04_bed', '05_heavy_door', '06_errands',
                                      '07_off_floor', '08_walk')
nQOL_lowExtremity['rawScore'] = apply(nQOL_lowExtremity[, nQOL_lowExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
cat('Before merge', nrow(nQOL_lowExtremity), 'rows')
nQOL_lowExtremity <- merge(nQOL_lowExtremity, nQOL_lowExtremity_scoring, all.x=T)
cat('After merge', nrow(nQOL_lowExtremity), 'rows')

# timeStampCol = 'createdOn'
# timeZoneCol = 'createdOnTimeZone'
# nQOL_lowExtremity <- insert_study_times(nQOL_lowExtremity, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)

## Download required columns i,e the JSON files
columnsToDownload = c("rawData") 
cog.zip.loc = lapply(columnsToDownload, function(col.name){
  tbl.files = synapser::synDownloadTableColumns(nQOL_lowExtremity.syn, col.name) %>%
    lapply(function(x) data.frame(V1 = x)) %>% 
    data.table::rbindlist(idcol = col.name) %>% 
    plyr::rename(c('V1' = gsub('Data','fileLocation', col.name)))
})
nQOL_lowExtremity$rawData <- as.character(nQOL_lowExtremity$rawData)
nQOL_lowExtremity.meta = data.table::rbindlist(list(nQOL_lowExtremity %>%
                                                      dplyr::left_join(do.call(cbind, cog.zip.loc))),
                                               use.names = T, fill = T) %>%
  as.data.frame
nQOL_lowExtremity.meta$rawfileLocation <- as.character(nQOL_lowExtremity.meta$rawfileLocation)

## Extract duration of each test
nQOL_lowExtremity.meta.noNA <- nQOL_lowExtremity.meta[!is.na(nQOL_lowExtremity.meta$rawfileLocation),]
nQOL_lowExtremity.times <- lapply(nQOL_lowExtremity.meta.noNA$rawfileLocation,
                                  function(rawzipLocation){
                                    zipfiles <- tryCatch({utils::unzip(as.character(rawzipLocation))},
                                                         error = function(err){
                                                           NA
                                                         })
                                    nQOL_lowExtremity_questions_cols <- c('01_toilet', '02_curbs', '03_car',
                                                                          '04_bed', '05_heavy_door', '06_errands',
                                                                          '07_off_floor', '08_walk')
                                    times.tests <- data.frame(nQOL_lowExtremity_questions_cols) %>% 
                                      data.table::transpose() %>% 
                                      `colnames<-`(paste0(nQOL_lowExtremity_questions_cols,'_time'))
                                    
                                    for(i in seq(8)){
                                      times.tests[i] <- tryCatch({
                                        temp.json <- jsonlite::fromJSON(as.character(zipfiles[i]))
                                        temp.time <- as.numeric(lubridate::as_datetime(temp.json$endDate) - lubridate::as_datetime(temp.json$startDate))
                                      }, error = function(err){
                                        NA
                                      })
                                    }
                                    unlink(zipfiles)
                                    total_time <- sum(as.numeric(times.tests), na.rm = T)
                                    mean_time <- mean(as.numeric(times.tests), na.rm = T)
                                    times.tests$total_time <- total_time
                                    times.tests$mean_time <- mean_time
                                    times.tests$rawfileLocation <- rawzipLocation
                                    return(times.tests)
                                  }
) %>% dplyr::bind_rows()

nQOL_lowExtremity.full <- dplyr::left_join(nQOL_lowExtremity.meta, nQOL_lowExtremity.times) %>% 
  dplyr::select(-rawfileLocation)

outFile = 'neuroQOL_lowExtremity.tsv'
PARENT_FOLDER = 'syn10140063'
write.table(nQOL_lowExtremity.full, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(nQOL_lowExtremity_scoring_file, nQOL_lowExtremity_synTable),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
unlink(outFile) 
cat('-----------\n')


# #####
# # #3. MSIS29
# ####
# cat('Processing MSIS29 PROs \n')
# msis29_syntable <- "syn9920302"
# msis29 <-  synTableQuery(paste("select * from", msis29_syntable))
# msis29 <- msis29$asDataFrame()
# msis29['rawScore'] <- apply(msis29[, c(12:40)], 1, function(x) sum(as.numeric(x), na.rm=T))
# previous <- dim(msis29)
# cat('Before merge', previous[1], 'rows')
# timeStampCol = 'createdOn'
# timeZoneCol = 'createdOnTimeZone'
# msis29 <- insert_study_times(msis29, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
# current <- dim(msis29)
# cat('After merge',current[1], 'rows')
# outFile = 'MSIS29.tsv'
# write.table(msis29, file=outFile, sep="\t", quote=F, row.names = F)
# synStore(File(outFile, parentId = PARENT_FOLDER), 
#          used=c(msis29_syntable),
#          executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
# unlink(outFile) 
# cat('-----------\n')





