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


#MS Patients only
userActivity_flt <- userActivity %>% 
  filter(dataGroups %in% c('ms_patient'), participant_week <= 12) %>%
  inner_join(baselineChar %>% select(healthCode, referred_by_clinician, group)) %>%
  filter(!originalTable %in% c('Passive Data-v3', 'Weather-v3')) %>%
  mutate(activity = case_when(
    grepl('Tremor', originalTable) ~ 'Finger-to-nose',
    grepl('Walking', originalTable) ~ 'Walking',
    grepl('Daily Check', originalTable) ~ 'Daily-checkin',
    grepl('Triggers', originalTable) ~ 'Triggers',
    grepl('Symptoms', originalTable) ~ 'Symptoms',
    grepl('Tapping', originalTable) ~ 'Tapping',
    grepl('Brain Baseline-v1', originalTable) ~ 'Cognition',
    grepl('NeuroQOL', originalTable) ~ 'NeuroQOLs',
    grepl('Medication|MSIS|Profile|WPAI|Relapse|Demog', originalTable) ~ 'Other-surveys')) %>%
  dplyr::mutate( activitytype = case_when(
    activity %in% c('Finger-to-nose', 'Walking', 'Tapping', 'Cognition') ~ 'sensor_based',
    activity %in% c('Symptoms', 'Triggers', 'Daily-checkin', 'Other-surveys', 'NeuroQOLs') ~ 'survey_based' )) %>%
  ## ignoring baseline surveys and relapse
  filter(!is.na(activitytype))


colnames(userActivity_flt)

####
## Activity specific compliance per user - MS patient only
####
perUser_activitycompletion_perweek <- userActivity_flt %>%
  dplyr::mutate(activitytype = case_when(
    activity %in% c('Finger-to-nose', 'Walking', 'Tapping', 'Cognition') ~ 'sensor_based',
    activity %in% c('Daily-checkin') ~ 'daily_checkins',
    activity %in% c('Symptoms', 'Triggers') ~ 'triggers_symptoms' )) %>%
  dplyr::filter(!is.na(activitytype)) %>%
  dplyr::mutate(activitytype = factor(activitytype, 
                                      levels = c('neuroqols', 'daily_checkins','triggers_symptoms', 'sensor_based'))) %>%
  dplyr::count(healthCode, participant_week, activitytype) %>%
  mutate(participant_week = as.factor(participant_week))


#The above group-by agg stats only calculating for weeks where there is data
# if there is no data we should add 0 for that week for that corresponding activitytype
##perUser_activitySpecificCompliance_perweek
minActivityWeek = expand.grid(healthCode = unique(perUser_activitycompletion_perweek$healthCode),
                              participant_week = as.character(seq(1:12)),
                              activitytype = unique(perUser_activitycompletion_perweek$activitytype),
                              minNumActivity = 0)
perUser_activitycompletion_perweek <- merge(minActivityWeek, perUser_activitycompletion_perweek, all.x = T) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>% select(-minNumActivity)



######### Comparing the prop of activities completed by each user
perUser_perActivity_total <- perUser_activitycompletion_perweek %>% 
  group_by(healthCode) %>%
  mutate(total= sum(n)) %>%
  group_by(healthCode, activitytype, total) %>% 
  summarise(n = sum(n)) %>%
  mutate(percent= round((n / total)*100, digits=2))

p <- ggplot(data=perUser_perActivity_total, aes(x=activitytype, y=percent)) + geom_boxplot(width=0.7)
p <- p + scale_x_discrete(labels=c('Daily checkin \n survey', 'MS symptoms/triggers \n survey', 'Sensor-based active \n functional test'))
p <- p + theme_bw(base_size = 13) + xlab('Activity type') + ylab('Percentage')
p
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/perUser_activityPercent_acrossStudy.png", plot=print(p), height = 5, width = 7, units="in", dpi=300)
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/perUser_activityPercent_acrossStudy.tiff", plot=print(p), height = 5, width = 7, units="in", dpi=300)


#### Confirmation usign ANOVA
perUser_perAcitivity_total %>% group_by(activitytype) %>% summarise(median = median(percent, na.rm = T))
aov_result <- aov(percent ~ activitytype, data=perUser_perAcitivity_total)
TukeyHSD(aov_result)
summary(aov_result)



##Per Activity Summary - created based on Reviewers' Revise and Resubmit 
numActivity_perWeek <- userActivity_flt %>% 
  dplyr::group_by(activity, participant_week) %>%
  dplyr::summarise(distinctHealthCodes = n_distinct(healthCode),
                   numRecords = n()) %>%
  dplyr::mutate( metric = paste0(distinctHealthCodes, ' (', numRecords, ')')) %>%
  dplyr::select(participant_week, activity, metric) %>%
  tidyr::spread(activity, metric)
write.table(numActivity_perWeek, 
            file="analysis/Analytical_Paper_2/Figs_N_Tables/activityCompletionRatesByWeek.csv",
            sep="\t",
            row.names = F
            )


### density of number of tasks / week done by participants by tasktype 
p1 <- ggplot(perUser_activitycompletion_perweek, aes(x = n, y = participant_week, fill = activitytype)) +
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





