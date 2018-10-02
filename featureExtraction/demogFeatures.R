library(synapser)
synapser::synLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate'))

get_demographics <- function(){
  demog <-  synTableQuery(paste("select * from syn10295288"))
  demog <- demog$asDataFrame() 
  demog$ROW_ID <- NULL
  demog$ROW_VERSION <-NULL
  demog$metadata.json.dataGroups <- NULL
  colnames(demog) <- gsub('.json.answer', '',colnames(demog))
  colnames(demog)  <- gsub('metadata.json.', '',colnames(demog))
  demog <- demog %>% dplyr::select(-scheduledActivityGuid, -endDate, -endDate.timezone, -appVersion, -phoneInfo, -validationErrors, -startDate.timezone, -createdOn, -createdOnTimeZone, -recordId, -uploadDate, -externalId) %>%
    filter(dataGroups %in% c('control', 'ms_patient')) %>%
    dplyr::mutate(startDate = as.Date(lubridate::as_datetime(startDate/1000)))
  
  demog <- ddply(.data = demog, .variables = c('healthCode'), .fun = function(x){
    x <- x %>% arrange(desc(startDate))
    x[1,]
  })
  
  race <- demog %>% tidyr::gather(race, value, 7:16) %>% dplyr::filter(value == 'true') %>% 
    dplyr::mutate(race = gsub('race.','',race)) %>% select(healthCode, race) %>% group_by(healthCode) %>%
    dplyr::summarise(race = paste(race,collapse=','))
  demog <- merge(demog, race) %>% select(-c(7:16))
  
  profiles <- synTableQuery(paste("select * from syn10235463"))$asDataFrame()
  profiles <- profiles %>% dplyr::filter(healthCode %in% STUDY_HEALTHCODES) %>%
    dplyr::transmute(healthCode = healthCode, 
                     startDate = as.Date(lubridate::as_datetime(createdOn/1000)),
                     zipcode = demographics.zipcode, age = demographics.age,
                     gender = demographics.gender, height = demographics.height,
                     weight = demographics.weight, education = demographics.education,
                     health_insurance = demographics.health_insurance,
                     employment = demographics.employment, race = demographics.race,
                     userSharingScope=NA) %>%
    dplyr::group_by(healthCode) %>%
    dplyr::summarise( startDate = unique(startDate)[1], zipcode = unique(zipcode)[1],
                      age = unique(age)[1], gender = unique(gender)[1], height = unique(height)[1],
                      weight = unique(weight)[1], education = unique(education)[1],
                      health_insurance = unique(health_insurance)[1], employment=unique(employment)[1],
                      race = unique(race)[1], userSharingScope=unique(userSharingScope)[1]) %>%
    dplyr::mutate(race = stringfy(race))
  
  #Add age to demog
  demog <- merge(demog, profiles %>% dplyr::select(healthCode, age), all.x=T)
  
  demog_derived_from_profiles <- profiles %>% dplyr::filter(!healthCode %in% demog$healthCode)
  dataGroups <- userActivity %>% select(healthCode, dataGroups)
  dataGroups <- dataGroups[!duplicated(dataGroups),]
  demog_derived_from_profiles <- merge(demog_derived_from_profiles , dataGroups, all.x=T)
  
  demog_derived_from_profiles <- demog_derived_from_profiles %>% 
    dplyr::select(healthCode, age, dataGroups, userSharingScope, gender, height,
                  weight, zipcode, education, health_insurance, employment, startDate, race)
  
  demog_total <- rbind(demog, demog_derived_from_profiles %>% select(colnames(demog)))
}
