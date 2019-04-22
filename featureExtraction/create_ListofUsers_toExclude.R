rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c( 'tidyverse', 'synapser', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', "bit64")

synapser::synLogin()
initialList <- fread(synGet("syn17869480")$path) %>% filter(!is.na(healthCode))
dim(initialList)

masterTable <- "syn9758009"  #poorly named elevate-ms-appVersion
#remove all testing users and records created before Monday, August 14, 2017 12:00:00 AM (GMT)
userActivity <- synTableQuery(paste("select * from", masterTable, "WHERE dataGroups NOT LIKE '%test%' AND createdOn > 1502668800000"))
userActivity <- userActivity$asDataFrame()

#Data Manipulation
userActivity <- userActivity %>% 
  dplyr::mutate(createdOnTimeZone =  createdOnTimeZone %/% 100,
                createdOn =  lubridate::as_datetime(createdOn / 1000, tz='UTC'))
#get start dates for people 
userStartDates <- userActivity %>% dplyr::group_by(healthCode) %>% 
  dplyr::summarise(participantStartDate = min(lubridate::date(createdOn)))
dim(userStartDates)

### Keep users that enrolled after Dec 15 as the last data dump cleaned by Vanessa was around Dec 17
toKeep <- userStartDates %>% filter(participantStartDate >= lubridate::date("2018-12-10"))


###Filter out the rest with those not found in Vanessa's list 
toClean <- userStartDates %>% filter(participantStartDate < lubridate::date("2018-12-10"))
dim(toClean)
##remove these from toClean
toRemove <- setdiff(toClean$healthCode, initialList$healthCode)


write.table(data.frame(healthCode = toRemove), file = 'elevateMS_users_to_exclude.tsv', sep="\t", quote=F, row.names = F)
synStore(File('elevateMS_users_to_exclude.tsv', parentId = 'syn10140063'),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/featureExtraction/create_ListofUsers_toExclude.R',
         used = list('syn17869480', 'syn9758009'))
unlink('elevateMS_users_to_exclude.tsv')
