rm(list=ls())
library("install.load")
# load all necessary libraries
install_load('synapser', 'tidyverse' ,'data.table', 'jsonlite')
install_load('parallel', 'tidyr', 'lubridate' , 'stringr', 'sqldf')
synapser::synLogin()

stringfy <- function(x){
  gsub('[\\[\"\\]]','',x, perl=T) 
}


START_DATE = lubridate::ymd("2017-08-14")


demog <-  synTableQuery(paste("select * from syn10295288"))
demog <- demog$asDataFrame() 
demog$ROW_ID <- NULL
demog$ROW_VERSION <-NULL
demog$metadata.json.dataGroups <- NULL
colnames(demog) <- gsub('.json.answer', '',colnames(demog))
colnames(demog)  <- gsub('metadata.json.', '',colnames(demog))
demog <- demog %>%  filter(dataGroups %in% c('control', 'ms_patient')) %>%
  dplyr::mutate(date_of_entry = as.Date(lubridate::as_datetime(startDate/1000))) %>%
  dplyr::select(-scheduledActivityGuid, -endDate, -endDate.timezone, 
                -validationErrors, -startDate.timezone, -startDate, -appVersion, 
                -phoneInfo, -createdOn, -createdOnTimeZone, -recordId, -uploadDate) %>%
  dplyr::filter(date_of_entry >= START_DATE)

demog$rawData <- NULL
### Fix Race
race <- demog %>% tidyr::gather(race, value, 8:17) %>% dplyr::filter(value == 'true') %>% 
  dplyr::mutate(race = gsub('race.','',race)) %>% select(healthCode, race) %>% group_by(healthCode) %>%
  dplyr::summarise(race = paste(unique(race),collapse=','))
demog <- merge(demog, race) %>% select(-c(8:17))
demog <- demog %>% mutate(weight = weight/2.2) ##Pounds to Lb 


##Profiles
profiles <- synTableQuery(paste("select * from syn10235463"))$asDataFrame()
colnames(profiles) <- gsub('demographics.', '',colnames(profiles))
profiles <- profiles %>% dplyr::select(-ROW_ID, -ROW_VERSION, -recordId, -rawData) %>%
  dplyr::mutate(race = stringfy(race),
                date_of_entry = as.Date(lubridate::as_datetime(createdOn/1000))) %>%
  dplyr::filter(date_of_entry >= START_DATE)

demog_from_profiles <- profiles %>%
  select(healthCode, externalId, dataGroups, userSharingScope,
         gender, height, weight, zipcode, education, health_insurance, 
         employment, date_of_entry, race)

#join demog from two sources
demog = rbind(demog, demog_from_profiles) %>%
  dplyr::mutate(height = round(height, digits=1),
                weight = round(weight, digits=1)) %>%
  mutate_at(.funs = c(tolower),
            .vars = c('healthCode', 'externalId', 'dataGroups', 'userSharingScope',
                      'gender','zipcode', 'education',
                      'health_insurance', 'employment', 'race') ) %>%
  distinct(healthCode, externalId, dataGroups, userSharingScope,
           gender, height, weight, zipcode, education,
           health_insurance, employment, race, .keep_all=T) 

demog_long <- demog %>% gather(feature, value, c(2:11, 13))

tmp_select_val <- function(date_of_entry, value){
    x <- data.frame(date_of_entry=date_of_entry, value=value) %>% arrange(date_of_entry) %>%
      na.omit()
    if (nrow(x) == 0){
      return('NA')
    } else{
      return(as.character(x$value[nrow(x)]))
    }
}

### FOR QC    
demog_summary <- demog_long %>% 
group_by(healthCode, feature) %>% 
  dplyr::summarise(n_uniq_values = length(unique(na.omit(value))),
                 unique_values = paste(unique(na.omit(value)), collapse=","),
                value = tmp_select_val(date_of_entry, value))


#External ID's
externalIds <- fread(synGet("syn17057743")$path) %>%
  dplyr::mutate(id = gsub('-','', id)) %>%
  dplyr::rename(externalId = id) 
true_externalIds = unique(externalIds$externalId)


#### Final Demog Summary
demog_clean <- demog_summary %>% select(-n_uniq_values, -unique_values) %>% 
  spread(feature, value) %>%
  dplyr::mutate(referred_by_clinician = ifelse(externalId %in% true_externalIds , T, F)) %>%
  dplyr::filter(dataGroups %in% c('ms_patient', 'control'))

#### How many times people change their responses
demog_changes <- demog_summary %>% select(-value) %>% spread(feature, n_uniq_values)

### Get the disease characteristics
diseaseCharacteristics <- profiles %>%
  select(healthCode, initialDiagnosis, initialDiagnosisYear, currentDiagnosis, 
         date_of_entry, currentDiagnosisYear, currentDMT, firstDMTYear, msFamilyHistory,
         overallPhysicalAbility) %>%
  mutate_all(.funs = (tolower))  %>%
  distinct(healthCode, initialDiagnosis, initialDiagnosisYear, currentDiagnosis, 
           currentDiagnosisYear, currentDMT, firstDMTYear, msFamilyHistory,
           overallPhysicalAbility, .keep_all = T)


### Delete initialDiagnosis & initialDiagnosisYear
diseaseCharacteristics <- diseaseCharacteristics %>% select(-initialDiagnosis, -initialDiagnosisYear)
diseaseChar_long <- diseaseCharacteristics %>% gather(feature, value, c(2, 4:8))
diseaseChar_summary <- diseaseChar_long %>% 
  group_by(healthCode, feature) %>% 
  dplyr::summarise(n_uniq_values = length(unique(na.omit(value))),
                   unique_values = paste(unique(na.omit(value)), collapse=","),
                   value = tmp_select_val(date_of_entry, value))


#### Final Disease Char Summary
diseaseChar_clean <- diseaseChar_summary %>% 
  select(-n_uniq_values, -unique_values) %>% 
  spread(feature, value)


#### Overall baseline chars
nrow(diseaseChar_clean)
baselineChar <- merge(demog_clean, diseaseChar_clean, all=T)

### Add the Age from profiles
participant_age <- profiles %>% select(healthCode, date_of_entry, age) %>%
  filter(!is.na(age)) %>% distinct() %>%
  group_by(healthCode) %>% 
  arrange(date_of_entry) %>% 
  summarise(date_of_entry = tail(date_of_entry, n=1),
            age = tail(age,n=1)) %>%
  select(-date_of_entry)
baselineChar <- merge(baselineChar, participant_age, all=T)


## Fix Race
#Fix race
to_replace <- grepl('latino_hispanic', baselineChar$race)
baselineChar$race[to_replace] = 'latino_hispanic'

to_replace <- grepl('black_or_african', baselineChar$race)
baselineChar$race[to_replace] = 'Black_African'

to_replace <- grepl('caucasian', baselineChar$race)
baselineChar$race[to_replace] = 'caucasian'

to_replace <- grepl('asian,pacific.*', baselineChar$race, perl=T)
baselineChar$race[to_replace] = 'asian'

to_replace <- grepl('native_american|caribbean|middle_eastern', baselineChar$race)
baselineChar$race[to_replace] = 'other'


### Fix Education
to_replace_1 <- baselineChar$education %in% c('some_high_school', 'high_school_diploma_ged')
baselineChar$education[to_replace_1] = 'high_school_diploma_ged'
to_replace_2 <- baselineChar$education %in% c('some_college')
baselineChar$education[to_replace_2] = 'college_degree'


baselineChar <- baselineChar %>%
  dplyr::mutate(height = as.numeric(height),
                weight = as.numeric(weight))


##Exclude Users based on Vanessa's offline analysis
to_exclude_users <- fread(synGet("syn17870261")$path)
baselineChar <- baselineChar %>% 
  filter(! healthCode %in% to_exclude_users$healthCode) 

#### Fixing errors
baselineChar['error'] = NA
baselineChar[baselineChar == 'NA'] = NA

baselineChar <- baselineChar %>% 
  mutate( error = case_when(
    
  #1. dataGroups = NA but other MS parameters currentDiagnosis & currentDiagnosisYear indicate MS patient
  (is.na(dataGroups) | dataGroups == '')  & 
  (!is.na(currentDiagnosis) & !is.na(currentDiagnosisYear))  ~ 'Ignore|dataGroups = NA and some MS related params',
  
  #2. dataGroups = NA but other MS parameters indicate currentDMT & firstDMTYear indicate MS patient
  (is.na(dataGroups) | dataGroups == '')  & 
  (!is.na(currentDMT) & !is.na(firstDMTYear))  ~ 'Ignore|dataGroups = NA and some MS related params',
  
  
  #3. dataGroups = NA but other > 3 parameters indicate MS Params
  (dataGroups == 'control')  & 
    ( !is.na(currentDMT) | ! currentDiagnosis %in% c('no', 'notsure') | 
        !is.na(currentDiagnosisYear) | !is.na(overallPhysicalAbility))  ~ 'Ignore|dataGroups = control and some MS related params',

  #4. dataGroups = Control but other MS parameters indicate currentDMT & firstDMTYear indicate MS patient
  (dataGroups == 'control')  &  (referred_by_clinician == T | !is.na(externalId))  ~ 'dataGroups = control and referred_by_clinician =T', 
  
  (is.na(dataGroups) | dataGroups == '') ~ 'Ignore|dataGroups = NA'

))


baselineChar <- baselineChar %>% 
  mutate(dataGroups = case_when(
    error == 'Ignore|dataGroups = control and some MS related params'   ~ 'Ignore',
    error == 'Ignore|dataGroups = NA and some MS related params'    ~ 'Ignore',
    error == 'Ignore|dataGroups = NA'                                      ~  'Ignore',
    error == 'dataGroups = control and referred_by_clinician =T'  ~ 'ms_patient',
    TRUE ~ .$dataGroups
  )) 

baselineChar <- baselineChar %>% 
  mutate(YearsSinceDiagnosis =  as.numeric(lubridate::year(Sys.Date())) - as.numeric(currentDiagnosisYear),
         YearsSinceFirstDMT = as.numeric(lubridate::year(Sys.Date())) - as.numeric(firstDMTYear))

baselineChar <- baselineChar %>% 
  mutate(health_insurance = case_when(
    health_insurance %in% c('government insurance', 'medicare', 'medicaid') ~ 'government insurance',
    health_insurance %in% c('other', 'rather not say') ~ 'other',
    TRUE ~ .$health_insurance
  )) 

baselineChar <- baselineChar %>% mutate(group = case_when(
  referred_by_clinician == T & dataGroups == 'ms_patient' ~ 'MS patients(clinical referral)',
  referred_by_clinician == F & dataGroups == 'ms_patient' ~ 'MS patients',
  dataGroups == 'control' ~ 'Controls'
)) 

outFile = 'elevateMS_baselineCharacteristics.tsv'
PARENT_FOLDER = 'syn10140063'
write.table(baselineChar, file=outFile, sep="\t", quote=F, row.names = F)
synStore(File(outFile, parentId = PARENT_FOLDER), 
         used=c('syn10295288', 'syn10235463', 'syn17057743'),
         executed='https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/featureExtraction/baselineCharacteristics.R'
        )
unlink(outFile) 

