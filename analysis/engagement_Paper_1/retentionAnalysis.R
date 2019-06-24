rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapser)
synapser::synLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools')

COL_MS_PATIENT = '#DC0000FF'
COL_CONTROL = '#00A087FF'
COL_MS_PATIENT_CLINICAL_REF = '#e66101'
ls()
#source("analysis/loadData.R")
load(synGet("syn11657929")$path)

stringfy <- function(x){
  gsub('[\\[\"\\]]','',x, perl=T) 
}

userActivity <- userActivity %>% 
  inner_join(baselineChar %>% select(healthCode, referred_by_clinician))

### This is mainly to figure out the #users that have been in study for atleast 
### #weeks irrespective of the data contributed
user_time_in_study <- userActivity %>% dplyr::group_by(healthCode, dataGroups) %>%
  dplyr::mutate(currentDate = Sys.Date(),
                days_in_study =  as.numeric(currentDate - participantStartDate) + 1,
                weeks_in_study =  ((days_in_study - 1 ) %/% 7 ) + 1)

# userWeeks <- user_time_in_study %>% dplyr::group_by(dataGroups, weeks_in_study) %>%
#   dplyr::summarise(numUsers = n_distinct(healthCode))

#Users that are in the study atleast 12 Weeks
SELECTED_USERS <- user_time_in_study %>% filter(weeks_in_study >= 12) %>% .$healthCode %>% unique()
userActivity <- userActivity %>% filter(healthCode %in% SELECTED_USERS)

### Retention 
overallRetention <- userActivity %>% 
  dplyr::group_by(dataGroups, participant_week) %>%
  dplyr::summarise(uniqUsers = n_distinct(healthCode))

finalEnrollment <- userActivity %>% 
  select(healthCode, dataGroups) %>%
  distinct() %>% 
  group_by(dataGroups) %>%
  dplyr::summarise(totalNum = n())

overallRetention <- overallRetention %>% inner_join(finalEnrollment) %>%
  dplyr::mutate(percent = round( (uniqUsers /totalNum)*100, digits=2)) %>%
  filter(participant_week <= 12)

#Compliance by clinical referrals
finalEnrollment_by_referrals <- userActivity %>% 
  select(healthCode, dataGroups, referred_by_clinician) %>%
  distinct() %>% 
  group_by(dataGroups, referred_by_clinician) %>%
  dplyr::summarise(totalNum = n())
retention_by_referrals <- userActivity %>% 
  dplyr::group_by(referred_by_clinician, dataGroups, participant_week) %>%
  dplyr::summarise(uniqUsers = n_distinct(healthCode))
retention_by_referrals <- merge(finalEnrollment_by_referrals, retention_by_referrals) %>%
  dplyr::mutate(percent = round( (uniqUsers /totalNum)*100, digits=2)) %>% 
  filter(dataGroups == 'ms_patient') %>%
  filter(participant_week <= 12)

p1 <- ggplot(data=overallRetention,
             aes(x=participant_week, y=percent, color=dataGroups )) + geom_point() + geom_line()
p1 <- p1 + theme(text = element_text(size=20)) + xlab('#weeks enrolled') 
p1 <- p1 + scale_x_discrete(limits=seq(1,12,1)) + theme_bw() + scale_color_manual(values=c(COL_CONTROL, COL_MS_PATIENT))
p1 <- p1 + geom_point(data=retention_by_referrals, aes(x=participant_week, y=percent))
p1 <- p1 + geom_line(data=retention_by_referrals, aes(x=participant_week, y=percent, group=referred_by_clinician, 
                       linetype =referred_by_clinician ))
p1 <- p1 + scale_linetype_manual(values=c("dotted" , "twodash"))
p1
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/overall_retention.png", plot=p1,
       width=6, height = 4, units="in", dpi=300)


#### New retention plot
tmp_retention <- plyr::rbind.fill(overallRetention %>% 
                   filter(dataGroups == 'control') %>%    
                   select(dataGroups, participant_week, percent) %>%
                   rename(group  = dataGroups),
                 retention_by_referrals %>% 
                   select(referred_by_clinician, participant_week, percent) %>%
                   rename(group  = referred_by_clinician) %>%
                   mutate(group  = ifelse(group == T, 'MS patient(clinically referred)', 'MS patient')))
tmp_retention <- tmp_retention %>% 
  mutate(group = factor(group, levels=c('MS patient', 'MS patient(clinically referred)',
                                        'control')))

p2 <- ggplot(data=tmp_retention,
             aes(x=participant_week, y=percent, color=group)) + geom_point() + geom_line()
p2 <- p2 + theme(text = element_text(size=20)) + xlab('#weeks enrolled') 
p2 <- p2 + scale_x_discrete(limits=seq(1,12,1)) + theme_bw() 
p2 <- p2 + scale_color_manual(values=c(COL_MS_PATIENT, COL_MS_PATIENT_CLINICAL_REF, COL_CONTROL))
p2
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/overall_retention_plot_1.png", plot=p2,
       width=6, height = 4, units="in", dpi=300)



## PRO Compliance
pro_surveys <- userActivity %>%
  filter( ( originalTable %like% 'NeuroQOL' | originalTable %like% 'MSIS' | originalTable %like% 'WPAI' ) & dataGroups == 'ms_patient')
pro_surveys_retention <- pro_surveys %>% 
  group_by(dataGroups, participant_week, originalTable) %>% 
  dplyr::summarise(num = n_distinct(healthCode)) %>%
  filter(participant_week <= 12)
pro_surveys_retention <- merge(pro_surveys_retention, finalEnrollment) %>% 
  dplyr::mutate(percent = round((num/totalNum) * 100, digits=2)) %>%
  dplyr::rename(Survey=originalTable) %>%
  dplyr::mutate(Survey = gsub('-v1', '',Survey),
                Survey = gsub('-v2', '',Survey),
                Survey = gsub('29', '5',Survey))
p4 <- ggplot(data=pro_surveys_retention, aes(x=participant_week, y=percent, color=Survey ))  + geom_point() + geom_line(size=.7)
p4 <- p4 + theme_few() +  scale_color_manual(values=c( "#2EC4B6", "#E71D36", "#FF9F1C", '#95C623', '#d01c8b')) 
p4 <- p4 +  ggtitle('Weekly surveys')  + xlab('#weeks enrolled') +  theme(text = element_text(size=8))
p4 <- p4 + scale_x_discrete(limits=seq(1,12,1)) + scale_y_continuous(limits=c(1,100))
p4
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/long_surveys_retention.png", plot=p4,
       width=5, height = 3, units="in", dpi=200)


### Active Task Compliance
activeSensor_tasks <- userActivity %>%
  filter( (originalTable %like% 'Tremor' | originalTable %like% 'Tapping' | originalTable %like%  'Walking' | originalTable %like% 'Brain') & dataGroups == 'ms_patient' )
activeSensor_tasks_compliance <- activeSensor_tasks %>% 
  group_by(dataGroups, participant_week, originalTable) %>% 
  dplyr::summarise(num = n_distinct(healthCode)) %>%
  filter(participant_week <= 12)
activeSensor_tasks_compliance <- merge(activeSensor_tasks_compliance, finalEnrollment) %>%
  dplyr::rename(sensorActivity = originalTable ) %>%
  dplyr::mutate(percent = round((num/totalNum) * 100, digits=2),
                sensorActivity = gsub(' Activity.*', '',sensorActivity, perl=T),
                sensorActivity = gsub('-.*', '', sensorActivity, perl=T),
                sensorActivity =  gsub('Brain Baseline', 'Cognition',sensorActivity))
p5 <- ggplot(data=activeSensor_tasks_compliance, aes(x=participant_week, y=percent, color=sensorActivity ))  + geom_point() + geom_line(size=.7)
p5 <- p5 + theme_few() +  scale_color_manual(values=c( "#2EC4B6", "#E71D36", "#FF9F1C", "#011627")) 
p5 <- p5 +  ggtitle('Sensor tasks')  + xlab('#weeks enrolled') +  theme(text = element_text(size=8))
p5 <- p5 + scale_x_discrete(limits=seq(1,12,1)) + scale_y_continuous(limits=c(1,100))
p5
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/activeTasks_retention.png", plot=p5,
       width=5, height = 3, units="in", dpi=200)


otherShortSurveys <-  userActivity %>%
  filter( (originalTable %like% 'Daily Check' | originalTable %like%  'Symptoms Survey' | originalTable %like% 'Triggers Survey' |  originalTable %like%  'Factors Survey') & dataGroups == 'ms_patient' )
otherShortSurveys_compliance <- otherShortSurveys %>% group_by(dataGroups, participant_week, originalTable) %>% 
  dplyr::summarise(num = n_distinct(healthCode)) %>%
  filter(participant_week <= 12)
otherShortSurveys_compliance <- merge(otherShortSurveys_compliance, finalEnrollment) %>%
  dplyr::rename(otherSurveys = originalTable) %>%
  dplyr::mutate(percent = round((num/totalNum) * 100, digits=2),
                otherSurveys = gsub('-v1', '',otherSurveys, perl=T))
p6 <- ggplot(data=otherShortSurveys_compliance,  aes(x=participant_week, y=percent, color=otherSurveys ))  + geom_point() + geom_line(size=.7)
p6 <- p6 + theme_few() +  scale_color_manual(values=c( "#2EC4B6", "#E71D36", "#FF9F1C", '#011627')) 
p6 <- p6 +  ggtitle('Daily surveys')  + xlab('#weeks enrolled') +  theme(text = element_text(size=8))
p6 <- p6 + scale_x_discrete(limits=seq(1,12,1)) + scale_y_continuous(limits=c(1,100))
p6
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/otherShort_surveys_retention.png", plot=p6,
       width=5, height = 3, units="in", dpi=200)



### Num of weekly app acitivities completed by MS Cohort 
### Activities / week / participant
userActivityperWeek = userActivity %>% filter(dataGroups == 'ms_patient') %>%
  inner_join(baselineChar %>% select(healthCode, referred_by_clinician)) %>%
  group_by(healthCode, participant_week, referred_by_clinician) %>% 
  dplyr::summarise(numActivity = n()) %>% 
  as.data.frame() 
p7 <- ggplot(data=userActivityperWeek, aes(x=as.factor(participant_week), y=numActivity, color=referred_by_clinician )) + geom_boxplot() + theme_bw()
p7 <- p7 + theme_few() + scale_color_manual(values=c(COL_CONTROL, COL_MS_PATIENT)) +  theme(text = element_text(size=15))
p7 <- p7 + xlab('weeks enrolled') +  theme(text = element_text(size=12)) + scale_x_discrete(limits=seq(1,12,1)) 
p7 <- p7 + theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right")
p7 <- p7 + ylab('#Activities')
p7
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/perWeekActivity.jpeg", plot=p7,
       width=6, height = 3, units="in", dpi=200)


##### Top activities in each category
topActivities = userActivity %>% filter(dataGroups == 'ms_patient') %>%
  filter(participant_week <= 12 ) %>%
  group_by(originalTable) %>% 
  summarise(numActivity = n()) %>% 
  as.data.frame() 

topActivities['taskType'] = NA
to_replace <- grepl('Tapping|Tremor|Brain|Walking', topActivities$originalTable)
topActivities$taskType[to_replace] = 'sensor-based'
to_replace <- grepl('NeuroQOL|MSIS|WPAI', topActivities$originalTable)
topActivities$taskType[to_replace] = 'weekly-surveys'
to_replace <- grepl('Daily Check|Symptoms Survey|Triggers Survey|Factors Survey', topActivities$originalTable)
topActivities$taskType[to_replace] = 'daily-check-ins'
topActivities
totalActivities <- summary <- topActivities %>% group_by(taskType) %>%
  summarise(n = sum(numActivity))
topActivities <- topActivities %>% inner_join(totalActivities) %>%
  mutate(percent = round((numActivity/n) * 100, digits=2)) 
topActivities
            
# ggplot(data=topActivities %>% filter(!is.na(taskType)),
#        aes(x=taskType , y=percent)) + geom_bar(stat = "identity") + facet_wrap(taskType ~ .)



#missingness in baseline data
baselineChar['missingness'] = apply(baselineChar, 1, function(x) sum(is.na(x)) /length(x) )
tmp <- users_days_active %>% 
  inner_join( baselineChar %>% select(healthCode, missingness)) %>%
  mutate(missingness = ifelse(missingness <= .10, 'No', "Yes"))

p8 <- ggplot(data = tmp , aes(y=totalDaysinStudy, x=missingness  )) + geom_boxplot() + theme_bw()
p8 <- p8 + xlab('Baseline data missingness') + ylab('total days in study')
p8
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/missingness_vs_daysinStudy.png", plot=p8,
       width=5, height = 5, units="in", dpi=200)

# independent 2-group Mann-Whitney U Test 
wilcox.test(totalDaysinStudy ~ missingness, data=tmp)



# #users who were active only for first 2 days
# users_active_2days_only <- users_days_active %>% filter(numDays <= 2) %>% .$healthCode
# demog %>% filter(healthCode %in% users_active_2days_only) %>% .$dataGroups %>% table()
# #users who were active > 1 days
# users_active_gt_2day <- userActiveDays %>% filter(numDays > 2) %>% .$healthCode
# users_active_gt_2day
# totalN_selectusers <- demog %>% filter(healthCode %in% users_active_gt_2day) %>% .$dataGroups %>% table()
# totalN_selectusers <- data.frame(totalN = as.numeric(totalN_selectusers),dataGroups = names(totalN_selectusers))


# #sumamry stats on active days / user
# userActiveDays %>% group_by(dataGroupsMod) %>% dplyr::summarise(meanDays = mean(numDays, na.rm=T),
#                                                                 sdDays = sd(numDays, na.rm=T))



#Calculating compliance for users who got past the 2 day mark
#Since the study is still going on we are not dividing by the all users rather number of unique users in each week of the study
overallCompliance_selectUsers <- userActivity %>% filter(healthCode %in% users_active_gt_2day) %>%
  dplyr::group_by(dataGroups, participant_week) %>%
  dplyr::summarise(uniqUsers = n_distinct(healthCode)) %>%
  inner_join(totalN_selectusers) %>%
  dplyr::mutate(complaince = round( (uniqUsers / totalN) * 100, digits=2))
View(overallCompliance_selectUsers)









