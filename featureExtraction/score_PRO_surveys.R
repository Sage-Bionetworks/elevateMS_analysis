#### To be run from loadData.R

####
#1. NueroQOL - Congition
###
cat('Processing NeuroQOL Cognition PROs \n')
#Suvery Norm scores
nQOL_cog_scoring_file <- "syn11223875"
nQOL_cog_scoring <- fread(synGet(nQOL_cog_scoring_file)$path, data.table=F)
nQOL_cog_scoring['rawScore'] = nQOL_cog_scoring$`Raw Score`
nQOL_cog_scoring$`Raw Score` <- NULL
#NueroQOL - Cognition V1
nueroQOL_cog_synTable <- "syn10242477"
nQOLcog <-  synTableQuery(paste("select * from", nueroQOL_cog_synTable))
nQOLcog <- nQOLcog$asDataFrame()
nQOL_cog_questions_cols <- c('01_reading', '02_slow_thinking', '03_attention',
                             '04_consentration', '05_current', '06_instructions',
                             '07_daily_activities', '08_new_tasks')
nQOLcog['rawScore'] = apply(nQOLcog[, nQOL_cog_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
cat('Before merge', nrow(nQOLcog), 'rows')
nQOLcog <- merge(nQOLcog, nQOL_cog_scoring, all.x=T)
cat('After merge', nrow(nQOLcog), 'rows')
timeStampCol = 'createdOn'
timeZoneCol = 'createdOnTimeZone'
nQOLcog <- insert_study_times(nQOLcog, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
current <- dim(nQOLcog)
cat('After merge',current[1], 'rows')
outFile = 'neuroQOL_Cognition.tsv'
write.table(nQOLcog, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(nQOL_cog_scoring_file, nueroQOL_cog_synTable),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
unlink(outFile)              
cat('-----------\n')




####
#2. NueroQOL - Upper Extremity
###
cat('Processing NeuroQOL Upper Extremity PROs \n')
#Suvery Norm scores
nQOL_uppExtremity_scoring_file <- "syn11223825"
nQOL_uppExtremity_scoring <- fread(synGet(nQOL_uppExtremity_scoring_file)$path, data.table=F)
nQOL_uppExtremity_scoring['rawScore'] = nQOL_uppExtremity_scoring$`Raw Score`
nQOL_uppExtremity_scoring$`Raw Score` <- NULL
#NueroQOL - Cognition V1
nQOL_uppExtremity_synTable <- "syn10139320"
nQOL_uppExtremity <-  synTableQuery(paste("select * from", nQOL_uppExtremity_synTable))
nQOL_uppExtremity <- nQOL_uppExtremity$asDataFrame()

nQOL_uppExtremity_questions_cols <- c('01_lock', '02_brush_teeth', '03_phone',
                         '04_coins', '05_write', '06_zipper',
                         '07_wash', '08_shampoo')
nQOL_uppExtremity['rawScore'] = apply(nQOL_uppExtremity[, nQOL_uppExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
nQOL_uppExtremity <- merge(nQOL_uppExtremity, nQOL_uppExtremity_scoring, all.x=T)

previous <- dim(nQOL_uppExtremity)
cat('Before merge', previous[1], 'rows')
timeStampCol = 'createdOn'
timeZoneCol = 'createdOnTimeZone'
nQOL_uppExtremity <- insert_study_times(nQOL_uppExtremity, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
current <- dim(nQOL_uppExtremity)
cat('After merge',current[1], 'rows')

outFile = 'neuroQOL_uppExtremity.tsv'
write.table(nQOL_uppExtremity, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(nQOL_uppExtremity_scoring_file, nQOL_uppExtremity_synTable),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
unlink(outFile) 
cat('-----------\n')


####
#3. NueroQOL - Low Extremity
###
cat('Processing NeuroQOL Lower Extremity PROs \n')
#Suvery Norm scores
nQOL_lowExtremity_scoring_file <- "syn11223833"
nQOL_lowExtremity_scoring <- fread(synGet(nQOL_lowExtremity_scoring_file)$path, data.table=F)
nQOL_lowExtremity_scoring['rawScore'] = nQOL_lowExtremity_scoring$`Raw Score`
nQOL_lowExtremity_scoring$`Raw Score` <- NULL

#NueroQOL - Cognition V1
nQOL_lowExtremity_synTable <- "syn10142944"
nQOL_lowExtremity <-  synTableQuery(paste("select * from", nQOL_lowExtremity_synTable))
nQOL_lowExtremity <- nQOL_lowExtremity$asDataFrame()

nQOL_lowExtremity_questions_cols <- c('01_toilet', '02_curbs', '03_car',
                                      '04_bed', '05_heavy_door', '06_errands',
                                      '07_off_floor', '08_walk')
nQOL_lowExtremity['rawScore'] = apply(nQOL_lowExtremity[, nQOL_lowExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
nQOL_lowExtremity <- merge(nQOL_lowExtremity, nQOL_lowExtremity_scoring, all.x=T)

previous <- dim(nQOL_lowExtremity)
cat('Before merge', previous[1], 'rows')
timeStampCol = 'createdOn'
timeZoneCol = 'createdOnTimeZone'
nQOL_lowExtremity <- insert_study_times(nQOL_lowExtremity, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
current <- dim(nQOL_lowExtremity)
cat('After merge',current[1], 'rows')
outFile = 'neuroQOL_lowExtremity.tsv'
write.table(nQOL_lowExtremity, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(nQOL_lowExtremity_scoring_file, nQOL_lowExtremity_synTable),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
unlink(outFile) 
cat('-----------\n')


#####
# #3. MSIS29
####
cat('Processing MSIS29 PROs \n')
msis29_syntable <- "syn9920302"
msis29 <-  synTableQuery(paste("select * from", msis29_syntable))
msis29 <- msis29$asDataFrame()
msis29['rawScore'] <- apply(msis29[, c(12:40)], 1, function(x) sum(as.numeric(x), na.rm=T))
previous <- dim(msis29)
cat('Before merge', previous[1], 'rows')
timeStampCol = 'createdOn'
timeZoneCol = 'createdOnTimeZone'
msis29 <- insert_study_times(msis29, timeStampCol=timeStampCol, timeZoneCol=timeZoneCol, userStartDates=userStartDates)
current <- dim(msis29)
cat('After merge',current[1], 'rows')
outFile = 'MSIS29.tsv'
write.table(msis29, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(msis29_syntable),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/score_PRO_surveys.R')
unlink(outFile) 
cat('-----------\n')





