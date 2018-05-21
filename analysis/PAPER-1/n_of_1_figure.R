rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', "ggsci"))
install_load('ggthemes', 'gridExtra', 'devtools')
install_load("plotly", "tableone", "webshot")
install_load('zipcode', 'maps', 'viridis', 'leaflet', 'RSelenium' )

# LOAD Data
FREEZED_DATA_LOC = "syn11657929"
load(synGet(FREEZED_DATA_LOC)@filePath)

userContrib <- userActivity %>% dplyr::group_by(healthCode, dataGroups, originalTable) %>% 
  dplyr::summarise(numAct = n_distinct(participant_day))
userContrib <- userContrib %>% spread(originalTable, numAct)


#####
# N_of_1 plot for a selected user 
#####

##### SELECTED USER ID 14
userId  = '8ceb5e62-e9ef-4bf9-8902-5ef6db1cd513'

tmp_checkin <- dailyCheckins %>% filter(healthCode == userId)
tmp_triggers <- triggers %>% filter(healthCode == userId)
tmp_symptoms <- symptoms %>% filter(healthCode == userId)
tmp_tapF <- tapF %>% filter(healthCode == userId)
user_relapse <- relapses %>% filter(healthCode == userId)

tmp_nQOL_uppExtremity <-  nQOL_uppExtremity %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-uppExtremity')
tmp_nQOL_lowExtremity <-  nQOL_lowExtremity %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-lowExtremity')
tmp_nQOL_cognition <- nQOL_cognition %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-Cognition')
tmp_nQOL <- rbind(tmp_nQOL_uppExtremity[,c('healthCode', 'participant_day', 'T-Score', 'survey')],
                    tmp_nQOL_lowExtremity[,c('healthCode', 'participant_day', 'T-Score', 'survey')],
                    tmp_nQOL_cognition[,c('healthCode', 'participant_day', 'T-Score', 'survey')])
tmp_nQOL['TScore'] = tmp_nQOL['T-Score']
tmp_nQOL <- tmp_nQOL %>% dplyr::mutate(survey = gsub('nQOL-', '', survey))
  
symptoms_per_week <- tmp_symptoms %>% dplyr::group_by(healthCode, participant_week) %>% 
    dplyr::summarise(numTimesSymptomsReported = n_distinct(activity_start_timestamp_local)) %>%
    dplyr::mutate(participant_day = participant_week * 7)
  
tmp_symptoms_summary <- tmp_symptoms %>%  dplyr::group_by(healthCode, participant_day) %>% 
    dplyr::summarise(numSymptoms = length(symptom))
user_checkIns <- tmp_checkin %>% filter(healthCode == userId) %>% 
    select(healthCode, participant_day, mood_health, mood_pain, mood_mobility) %>% gather(type, dailyScore, 3:5)
  
p1 <- ggplot(data=tmp_tapF, aes(x=participant_day, y=numberTaps, color=hand)) + geom_point() + theme_bw() +
  scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
p1 <- p1 + theme(legend.position = c(0.9, 0.8)) + geom_smooth() + scale_color_manual(values=c("#3C5488FF", "#7E6148FF"))
p1 <- p1 + xlab('')
p1 <- p1 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                      color = "grey50", size=.5) + theme(text = element_text(size=12)) 
p1
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/n_of_1_numberTaps.png", plot=p1, height=3.5, width=10, units="in", dpi=300)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/n_of_1_numberTaps.tiff", plot=p1, height=3.5, width=10, units="in", dpi=300)


p3 <- ggplot(data=symptoms_per_week, aes(x=participant_day, y=numTimesSymptomsReported)) + 
  geom_line(linetype = "dashed") + geom_point() + 
  theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80, 84))
p3 <- p3 + ylab('#symptoms') +  theme(legend.position = "bottom") + scale_color_npg() + scale_fill_npg()
p3 <- p3 + xlab('day in study [1-84]')
p3 <- p3 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                          color = "grey50", size=.5) + theme(text = element_text(size=12)) 
p3 <- p3 + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/n_of_1_Relapse.png", plot=p3, height=3.5, width=10, units="in", dpi=300)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/n_of_1_Relapse.tiff", plot=p3, height=3.5, width=10, units="in", dpi=300)


# 
# p1 = 'd744c79f-d5ef-4c96-9685-59da54aec644'
# userTimeline(p1)
# 
# p2 = '1bc84bf7-99da-4ba8-887b-be4b5dbae4c7'
# userTimeline(p2)
# 
# p3 = '8ceb5e62-e9ef-4bf9-8902-5ef6db1cd513'
# p3_plot <- userTimeline(p3)
# 
# p4 = 'a45a126b-cc26-4e68-8588-7c12f7ce5dbd'
# userTimeline(p4)
# 
# p5 = '405df1ec-4879-43bb-b28c-f7bd9ea775e4'
# userTimeline(p5)
