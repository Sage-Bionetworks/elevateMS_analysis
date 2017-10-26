rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra')



PARENT_FOLDER = 'syn10140063'

####
#1. NueroQOL - Congition
###
#Suvery Norm scores
nQOL_cog_scoring_file <- "syn11223875"
nQOL_cog_scoring <- fread(synGet(nQOL_cog_scoring_file)@filePath, data.table=F)
nQOL_cog_scoring['rawScore'] = nQOL_cog_scoring$`Raw Score`
nQOL_cog_scoring$`Raw Score` <- NULL

#NueroQOL - Cognition V1
nueroQOL_cog_synTable <- "syn10242477"
nQOLcog <-  synTableQuery(paste("select * from", nueroQOL_cog_synTable))
nQOLcog <- nQOLcog@values
nQOL_cog_questions_cols <- c('01_reading', '02_slow_thinking', '03_attention',
                         '04_consentration', '05_current', '06_instructions',
                         '07_daily_activities', '08_new_tasks')
nQOLcog['rawScore'] = apply(nQOLcog[, nQOL_cog_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
nQOLcog <- merge(nQOLcog, nQOL_cog_scoring, all.x=T)

outFile = 'neuroQOL_Cognition.tsv'
write.table(nQOLcog, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c(nQOL_cog_scoring_file, nueroQOL_cog_synTable),
         executed=)
              
              
####
#2. NueroQOL - Upper Extremity
###
#Suvery Norm scores
nQOL_uppExtremity_scoring_file <- "syn11223825"
nQOL_uppExtremity_scoring <- fread(synGet(nQOL_uppExtremity_scoring_file)@filePath, data.table=F)
nQOL_uppExtremity_scoring['rawScore'] = nQOL_uppExtremity_scoring$`Raw Score`
nQOL_uppExtremity_scoring$`Raw Score` <- NULL

#NueroQOL - Cognition V1
nQOL_uppExtremity_synTable <- "syn10139320"
nQOL_uppExtremity <-  synTableQuery(paste("select * from", nQOL_uppExtremity_synTable))
nQOL_uppExtremity <- nQOL_uppExtremity@values

nQOL_uppExtremity_questions_cols <- c('01_lock', '02_brush_teeth', '03_phone',
                         '04_coins', '05_write', '06_zipper',
                         '07_wash', '08_shampoo')
nQOL_uppExtremity['rawScore'] = apply(nQOL_uppExtremity[, nQOL_uppExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
nQOL_uppExtremity <- merge(nQOL_uppExtremity, nQOL_uppExtremity_scoring, all.x=T)




####
#3. NueroQOL - Low Extremity
###
#Suvery Norm scores
nQOL_lowExtremity_scoring_file <- "syn11223833"
nQOL_lowExtremity_scoring <- fread(synGet(nQOL_lowExtremity_scoring_file)@filePath, data.table=F)
nQOL_lowExtremity_scoring['rawScore'] = nQOL_lowExtremity_scoring$`Raw Score`
nQOL_lowExtremity_scoring$`Raw Score` <- NULL

#NueroQOL - Cognition V1
nQOL_lowExtremity_synTable <- "syn10142944"
nQOL_lowExtremity <-  synTableQuery(paste("select * from", nQOL_lowExtremity_synTable))
nQOL_lowExtremity <- nQOL_lowExtremity@values

nQOL_lowExtremity_questions_cols <- c('01_toilet', '02_curbs', '03_car',
                                      '04_bed', '05_heavy_door', '06_errands',
                                      '07_off_floor', '08_walk')
nQOL_lowExtremity['rawScore'] = apply(nQOL_lowExtremity[, nQOL_lowExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
nQOL_lowExtremity <- merge(nQOL_lowExtremity, nQOL_lowExtremity_scoring, all.x=T)



# 
# ####
# #3. MSIS29
# ###
# #Suvery Norm scores
# nQOL_lowExtremity_scoring_file <- "syn9920302"
# nQOL_lowExtremity_scoring <- fread(synGet(nQOL_lowExtremity_scoring_file)@filePath, data.table=F)
# nQOL_lowExtremity_scoring['rawScore'] = nQOL_lowExtremity_scoring$`Raw Score`
# nQOL_lowExtremity_scoring$`Raw Score` <- NULL
# 
# #NueroQOL - Cognition V1
# nQOL_lowExtremity_synTable <- "syn10142944"
# nQOL_lowExtremity <-  synTableQuery(paste("select * from", nQOL_lowExtremity_synTable))
# nQOL_lowExtremity <- nQOL_lowExtremity@values
# 
# nQOL_lowExtremity_questions_cols <- c('01_toilet', '02_curbs', '03_car',
#                                       '04_bed', '05_heavy_door', '06_errands',
#                                       '07_off_floor', '08_walk')
# nQOL_lowExtremity['rawScore'] = apply(nQOL_lowExtremity[, nQOL_lowExtremity_questions_cols], 1, function(x) sum(as.numeric(x), na.rm = T))
# nQOL_lowExtremity <- merge(nQOL_lowExtremity, nQOL_lowExtremity_scoring, all.x=T)
# 




