############################################################################
# ElevateMS project
# Purpose: Score PRO Surveys
# Author: Abhishek Pratap, Meghasyam Tummalacherla
############################################################################

##############
# Required libraries
##############
rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c( 'tidyverse', 'synapser', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', "bit64", "sqldf", "parsedate")
install_load('tm', 'doMC',  'SnowballC', 'wordcloud', 'RColorBrewer', "githubr")
install_load("scales", "ggsci", "jsonlite", "parallel", "stringr", "data.table")
#devtools::install_github("brian-bot/githubr")
## Synapse Login
synapser::synLogin()

score_NeuroQoLs <- function(df, normTable, questionCols ){
  tmp <- df %>% select(-ROW_ID, -ROW_VERSION, -appVersion, -phoneInfo, -uploadDate,
                       -validationErrors, -recordId) %>% 
    distinct() %>%
    gather(question, value, questionCols) %>%
    ## Fix response as Bridge/Synapse has the item response and numeric code reversed
    ## ref - http://www.healthmeasures.net/administrator/components/com_instruments/uploads/Neuro-QOL%20v1.0%20-%20Up_Extrem_Function%20SF_2-27-18.pdf
    mutate(value = plyr::revalue(factor(value), replace=c('1'=5, '2'=4, '3'=3, '4'=2, '5'=1)),
           value = as.numeric(as.character(value))) %>%
    spread(question, value) %>%
    dplyr::mutate(rawScore = rowSums(select(., questionCols), na.rm = T))
  
    merge(tmp, normTable, all.x=T)
}


##############
# 1. NueroQOL - Congition
##############
cat('Processing NeuroQOL Cognition PROs \n')
#Suvery Norm scores
nQOL_cog_scoring_file <- "syn11223875"
nQOL_cog_scoring <- data.table::fread(synapser::synGet(nQOL_cog_scoring_file)$path, data.table=F)
nQOL_cog_scoring <- nQOL_cog_scoring %>% 
  dplyr::rename('rawScore' = 'Raw Score', 'TScore' = 'T-Score')

#NueroQOL - Cognition V1
nueroQOL_cog_synTable <- "syn10242477"
nQOLcog.syn <-  synapser::synTableQuery(paste("select * from", nueroQOL_cog_synTable))
nQOLcog <- nQOLcog.syn$asDataFrame()
nQOL_cog_questions_cols <- c('01_reading', '02_slow_thinking', '03_attention',
                             '04_consentration', '05_current', '06_instructions',
                             '07_daily_activities', '08_new_tasks')
#nQOLcog['old_rawScore'] = apply(nQOLcog[, nQOL_cog_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))

#New corrected scores
nQOLcog <- nQOLcog %>% score_NeuroQoLs(nQOL_cog_scoring, nQOL_cog_questions_cols)


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
  dplyr::rename('rawScore' = 'Raw Score', 'TScore' = 'T-Score')

#NueroQOL - Cognition V1
nQOL_uppExtremity_synTable <- "syn10139320"
nQOL_uppExtremity.syn <-  synapser::synTableQuery(paste("select * from", nQOL_uppExtremity_synTable))
nQOL_uppExtremity <- nQOL_uppExtremity.syn$asDataFrame()
nQOL_uppExtremity_questions_cols <- c('01_lock', '02_brush_teeth', '03_phone',
                         '04_coins', '05_write', '06_zipper',
                         '07_wash', '08_shampoo')
#nQOL_uppExtremity['old_rawScore'] = apply(nQOL_uppExtremity[, nQOL_uppExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))

#New corrected scores
nQOL_uppExtremity <- nQOL_uppExtremity %>% score_NeuroQoLs(nQOL_uppExtremity_scoring, nQOL_uppExtremity_questions_cols)

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
  dplyr::rename('rawScore' = 'Raw Score', 'TScore' = 'T-Score') 
  
#NueroQOL - Cognition V1
nQOL_lowExtremity_synTable <- "syn10142944"
nQOL_lowExtremity.syn <-  synapser::synTableQuery(paste("select * from", nQOL_lowExtremity_synTable))
nQOL_lowExtremity <- nQOL_lowExtremity.syn$asDataFrame()
nQOL_lowExtremity_questions_cols <- c('01_toilet', '02_curbs', '03_car',
                                      '04_bed', '05_heavy_door', '06_errands',
                                      '07_off_floor', '08_walk')
#nQOL_lowExtremity['old_rawScore'] = apply(nQOL_lowExtremity[, nQOL_lowExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))


#New corrected scores
nQOL_lowExtremity <- nQOL_lowExtremity %>% score_NeuroQoLs(nQOL_lowExtremity_scoring, nQOL_lowExtremity_questions_cols)


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




# tmp <- rbind(nQOL_lowExtremity.full %>% select(rawScore, old_rawScore, TScore) %>% mutate(survey='lowExtremity'),
#       nQOLcog.full %>% select(rawScore, old_rawScore, TScore) %>% mutate(survey='cog'),
#       nQOL_uppExtremity.full %>% select(rawScore, old_rawScore, TScore) %>% mutate(survey='uppExtremity'))
# 
# ggplot(data=tmp %>% gather(type, value, rawScore, old_rawScore), aes(x=value, fill=type)) + geom_density(alpha=.7) + theme_bw()
# ggplot(data=tmp, aes(x=TScore, fill=survey)) + geom_density(alpha=.7) + theme_bw()


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





