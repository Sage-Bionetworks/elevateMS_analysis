rm(list=ls())
Sys.setenv(TZ='GMT')
options(stringsAsFactors = F)
library(synapser)
synapser::synLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools')
install_load("ggridges", "viridis")

## LOAD DATA 
load(synGet("syn19273733")$path)

userCompliance_data <- userActivity %>% 
  filter(dataGroups %in% c('control', 'ms_patient')) %>%
  inner_join(baselineChar %>% select(healthCode, referred_by_clinician, group)) %>%
  filter(!originalTable %in% c('Passive Data-v3', 'Weather-v3')) %>%
  mutate(activity = case_when(
    grepl('Tremor', originalTable) ~ 'finger-to-nose',
    grepl('Walking', originalTable) ~ 'walking',
    grepl('Daily Check', originalTable) ~ 'Daily-checkin',
    grepl('Triggers', originalTable) ~ 'triggers',
    grepl('Symptoms', originalTable) ~ 'symptoms',
    grepl('Tapping', originalTable) ~ 'tapping',
    grepl('Brain Baseline-v1', originalTable) ~ 'cognition',
    grepl('NeuroQOL|Medication|MSIS|Profile|WPAI|Relapse|Demog', originalTable) ~ 'Other-surveys'
  )) %>%
  mutate( activitytype = case_when(
    activity %in% c('finger-to-nose', 'walking', 'tapping', 'cognition') ~ 'sensor_based',
    activity %in% c('symptoms', 'triggers', 'Daily-checkin', 'Other-surveys') ~ 'survey_based' )) %>%
  ## ignoring baseline surveys and relapse
  filter(!is.na(activitytype))


## Activity specific compliance per user - MS patient only
perUser_activitySpecificCompliance_perweek <- userCompliance_data %>%
  mutate(activitytype = case_when(
    activity %in% c('finger-to-nose', 'walking', 'tapping', 'cognition') ~ 'sensor_based',
    activity %in% c('Daily-checkin') ~ 'daily_checkins',
    activity %in% c('symptoms', 'triggers') ~ 'triggers_symptoms'
  )) %>%
  dplyr::filter(!is.na(activitytype)) %>%
  dplyr::mutate(activitytype = factor(activitytype, 
                                      levels = c('daily_checkins','triggers_symptoms', 'sensor_based'))) %>%
  filter(participant_week <= 12 & dataGroups == 'ms_patient') %>% 
  dplyr::count(healthCode, participant_week, activitytype) %>%
  mutate(participant_week = as.factor(participant_week))

View(perUser_activitySpecificCompliance_perweek)

#The above group-by agg stats only calculating for weeks where there is data
# if there is no data we should add 0 for that week for that corresponding activitytype
##perUser_activitySpecificCompliance_perweek
minActivityWeek = expand.grid(healthCode = unique(perUser_activitySpecificCompliance_perweek$healthCode),
                              participant_week = as.character(seq(1:12)),
                              activitytype = unique(perUser_activitySpecificCompliance_perweek$activitytype),
                              minNumActivity = 0)
perUser_activitySpecificCompliance_perweek <- merge(minActivityWeek, perUser_activitySpecificCompliance_perweek, all.x = T) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>% select(-minNumActivity)

## QA/QC
ggplot(data=perUser_activitySpecificCompliance_perweek, aes(x=participant_week, y=n,
                                                            fill = activitytype)) + geom_boxplot()


### density of number of tasks / week done by participants by tasktype 
p1 <- ggplot(perUser_activitySpecificCompliance_perweek, aes(x = n, y = participant_week, fill = activitytype)) +
  geom_density_ridges(alpha = 0.8, scale=1) + 
  theme_ridges(font_size = 12, grid = TRUE,center_axis_labels=T) + 
  scale_fill_manual('active task type', 
                    labels =  c('daily-checkins \n survey',
                                'MS triggers/symptoms \n survey',
                                'sensor-based'),
                    values=c('#1b9e77', '#d95f02', '#7570b3')) + 
  xlab('number of active tasks completed per week') + ylab('week in study') +
  theme(legend.position = "top") + 
  geom_vline(xintercept = c(4,7), color='grey30', linetype = "dashed")
current_theme <- theme_get()
p1
# ggsave("analysis/engagement_Paper_1/FINAL_FIGS/userCompliance_by_actitivies_n_week.png", plot=print(p1), height = 5, width = 6, units="in", dpi=100)
# ggsave("analysis/engagement_Paper_1/FINAL_FIGS/userCompliance_by_actitivies_n_week.tiff", plot=print(p1), height = 5, width = 6, units="in", dpi=200)

######### Comparing the prop of activities completed by each user
perUser_perAcitivity_total <- perUser_activitySpecificCompliance_perweek %>% 
  group_by(healthCode) %>%
  mutate(total= sum(n)) %>%
  group_by(healthCode, activitytype, total) %>% 
  summarise(n = sum(n)) %>%
  mutate(percent= round((n / total)*100, digits=2))

p <- ggplot(data=perUser_perAcitivity_total, aes(x=activitytype, y=percent)) + geom_boxplot(width=0.7)
p <- p + scale_x_discrete(labels=c('daily checkin \n survey', 'MS triggers/symptoms \n survey', 'sensor-based'))
p <- p + theme_light(base_size = 13) + xlab('activity type')
p
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/perUser_activityPercent_acrossStudy.png", plot=print(p), height = 5, width = 6, units="in", dpi=300)
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/perUser_activityPercent_acrossStudy.tiff", plot=print(p), height = 5, width = 6, units="in", dpi=300)


#### Confirmation usign ANOVA
perUser_perAcitivity_total %>% group_by(activitytype) %>% summarise(median = median(percent, na.rm = T))
aov_result <- aov(percent ~ activitytype, data=perUser_perAcitivity_total)
TukeyHSD(aov_result)
summary(aov_result)



#############################
# Study wide compliance data 
#############################
users_LastWeek_inStudy <- userCompliance_data %>% 
  group_by(healthCode, dataGroups, referred_by_clinician ) %>%
  dplyr::summarise(lastWeek = max(participant_week)) %>%
  dplyr::mutate(lastWeekCapat12 = ifelse(lastWeek >= 12, 12, lastWeek))

tmpGetUsers_with_xSensorTasks_per_Week <- function(healthCode, activitytype){
  data.frame(healthCode=healthCode, activitytype=activitytype) %>%
    filter(activitytype == 'sensor_based') %>%
    dplyr::count(healthCode)  %>% 
    filter(n >= 1) %>% ### NUMBER OF TASK A PARTICIPANT MUST COMPLETE 
    .$healthCode %>% n_distinct()
}

#Get per week compliance based on 
# 1. completes atleast one active task
# 2. completes atleast one sensor-based task
perWeek_activeUsers_inStudy <- userCompliance_data %>%
  group_by(participant_week, dataGroups, referred_by_clinician) %>%
  dplyr::summarise(totalActiveUsers_oneActiveTask = n_distinct(healthCode),
                   totalActiveUsers_withOneSensorTaskperWeek = tmpGetUsers_with_xSensorTasks_per_Week(healthCode,activitytype)) %>%
  dplyr::filter(participant_week <= 12) %>%
  dplyr::rename(week = participant_week)

View(perWeek_activeUsers_inStudy)

totalUsers_byWeek = users_LastWeek_inStudy %>% as.data.frame() %>%
  dplyr::count(lastWeekCapat12, dataGroups, referred_by_clinician)

### If any week is missing mostly due to no user exiting the study in that week (possible case)
tmp <- rbind(expand.grid(dataGroups = 'ms_patient', referred_by_clinician = c(T,F),
            lastWeekCapat12 = 1:12),
            expand.grid(dataGroups = 'control', referred_by_clinician = c(F),
                        lastWeekCapat12 = 1:12))
totalUsers_byWeek <- merge(totalUsers_byWeek, tmp, all = T) %>%
  mutate(n = replace_na(n, 0))
View(totalUsers_byWeek)


## Now users who ended the study on Week 12 - should also be counted for Week 1-11. 
## Doing the rev cumsum will make that work
## this one is def complicated but makes sense :)
totalUsers_byWeek <- totalUsers_byWeek %>% 
  dplyr::rename(week = lastWeekCapat12) %>% 
  group_by(dataGroups, referred_by_clinician) %>%
  arrange(week) %>%
  dplyr::mutate(totalN = rev(cumsum(rev(n)))) %>% ### this one is def complicated but makes sense :)
  dplyr::select(-n)

View(totalUsers_byWeek)


compliance <- merge(totalUsers_byWeek, perWeek_activeUsers_inStudy, all=T) %>%
  gather(complianceType, value, c('totalActiveUsers_oneActiveTask', 'totalActiveUsers_withOneSensorTaskperWeek') ) %>%
  mutate(complianceType = gsub('totalActiveUsers_', '', complianceType),
         percent = round( (value / totalN)*100, digits=2))
View(compliance)

#Editing labels for plotting
compliance <- compliance %>%
  mutate(dataGroups = replace(dataGroups, dataGroups == 'ms_patient', 'MS patient'),
         dataGroups = replace(dataGroups, dataGroups == 'control', 'Control'),
         dataGroups = replace(dataGroups, referred_by_clinician == T, 'MS patient(clinically referred)') ) %>%
  rename(group = dataGroups) %>%
  mutate(group = factor(group, levels=c('MS patient', 'MS patient(clinically referred)',
                                        'Control'))) %>%
  mutate(lineGroup = paste0(group,'-' ,complianceType))

View(compliance  %>% filter(complianceType == 'withOneSensorTaskperWeek') %>%
       arrange(group, week))


#### This plot is showing compliance for users that remained in the study each week and completed atleast one active task
p1 <- ggplot(data=compliance %>% filter(complianceType == 'withOneSensorTaskperWeek'),
             aes(x=week, y=percent, color=group))+ geom_point(size=1) + geom_line() 
p1 <- p1 + scale_y_continuous(limits = c(0,100)) + geom_line(size=.8) + theme_light( base_size=15) + scale_x_continuous(breaks=seq(1,12,1))
p1 <- p1 + scale_color_manual(name='',values=c(COL_MS_PATIENT, COL_MS_PATIENT_CLINICAL_REF, COL_CONTROL))
p1 <- p1 + theme(legend.position="top") + xlab('weeks in study')
p1
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/userCompliance.png", plot=print(p1), height = 5, width = 6, units="in", dpi=300)
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/userCompliance.tiff", plot=print(p1), height = 5, width = 6, units="in", dpi=300)
