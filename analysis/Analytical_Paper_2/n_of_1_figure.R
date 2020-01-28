rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', "ggsci"))
install_load('ggthemes', 'gridExtra', 'devtools')
install_load("plotly", "tableone", "webshot")
install_load('zipcode', 'maps', 'viridis', 'leaflet', 'RSelenium' )

###Load data freeze
load(synGet("syn19273733")$path)

userContrib <- userActivity %>% dplyr::group_by(healthCode, dataGroups, originalTable) %>% 
  dplyr::summarise(numAct = n_distinct(participant_day))
userContrib <- userContrib %>% spread(originalTable, numAct)


#####
# N_of_1 plot for a selected user 
#####
head(tmp_checkin)
userId  = 'e9995eb8-b044-4d7e-88b0-59db59a5ccab'

userId = 'c7f5a6d1-620c-4395-89f2-0e08e55d46d7'
#userId = '737a30c3-3472-42ec-9659-545fc1fdad68'

tmp_checkin <- dailyCheckins %>% filter(healthCode == userId)
tmp_triggers <- triggers %>% filter(healthCode == userId)
tmp_symptoms <- symptoms %>% filter(healthCode == userId)
tmp_tapF <- tapF %>% filter(healthCode == userId)
user_relapse <- relapses %>% filter(healthCode == userId)
tmp_weather <- weatherF_flt %>% 
  dplyr::mutate(participant_day = as.numeric(weather_date - unique(tmp_checkin$elevateMS_startDate_GMT))+1) %>%
  filter(healthCode == userId)

tmp_nQOL_uppExtremity <-  nQOL_uppExtremity %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-uppExtremity')
tmp_nQOL_lowExtremity <-  nQOL_lowExtremity %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-lowExtremity')
tmp_nQOL_cognition <- nQOL_cognition %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-Cognition')


df <- tmp_tapF %>% select(participant_day, numberTaps) %>% inner_join(tmp_weather)
ggplot(data=df, aes(x=numberTaps, y=currentTemperature, color=participant_day)) + geom_point() 


selectCols <- c('healthCode', 'participant_day', 'TScore', 'survey')
tmp_nQOL <- rbind(tmp_nQOL_uppExtremity %>% dplyr::select(selectCols),
                  tmp_nQOL_lowExtremity %>% dplyr::select(selectCols),
                  tmp_nQOL_cognition %>% dplyr::select(selectCols))
tmp_nQOL <- tmp_nQOL %>% dplyr::mutate(survey = gsub('nQOL-', '', survey))

symptoms_per_week <- tmp_symptoms %>% 
  dplyr::group_by(healthCode, participant_week) %>% 
  dplyr::summarise(numTimesSymptomsReported = n_distinct(activity_start_timestamp_local)) %>%
  dplyr::mutate(participant_day = participant_week * 7)
  
tmp_symptoms_summary <- tmp_symptoms %>%  
  dplyr::group_by(healthCode, participant_day) %>% 
  dplyr::summarise(numSymptoms = length(symptom))

user_checkIns <- tmp_checkin %>% filter(healthCode == userId) %>% 
    select(healthCode, participant_day, mood_health, mood_pain, mood_mobility) %>% 
  gather(type, dailyScore, 3:5)
  

#Feature 1
p1 <- ggplot(data=tmp_tapF, aes(x=participant_day, y=numberTaps, color=hand)) + geom_point() + theme_bw() +
  scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
p1 <- p1 + theme(legend.position = c(0.9, 0.9)) + geom_smooth() + scale_color_manual(values=c("#3C5488FF", "#7E6148FF"))
p1 <- p1 + xlab('')
p1 <- p1 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                      color = "grey50", size=.5) + theme(text = element_text(size=12)) 
p1

# Weather 
p2 <- ggplot(data=tmp_weather, aes(x=participant_day, y=currentTemperature)) + geom_point() + theme_bw() +
  scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))

p3 <- ggplot(data=tmp_nQOL, aes(x=participant_day, y=TScore, color=survey)) + geom_point() + theme_bw() + geom_line() +
  scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84)) + ylab("Neuro-QoL's") + theme(legend.position="top")
p3

p4 <- ggplot(data=tmp_checkin, aes(x=participant_day, y=as.factor(mood_health))) + geom_point() + theme_bw() + 
  scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84)) + ylab('daily checkin - health')

p5 <- ggplot(data=tmp_checkin, aes(x=participant_day, y=as.factor(mood_mobility))) + geom_point() + theme_bw() +
  scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84)) + ylab('daily checkin - mobility')

p6 <- ggplot(data=symptoms_per_week, aes(x=participant_day, y=numTimesSymptomsReported)) + 
  geom_line(linetype = "dashed") + geom_point() + 
  theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80, 84))
p6 <- p6 + ylab('#symptoms') +  theme(legend.position = "bottom") + scale_color_npg() + scale_fill_npg()
p6 <- p6 + xlab('day in study [1-84]')
p6 <- p6 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                          color = "grey50", size=.5) + theme(text = element_text(size=12)) 
p6 <- p6 + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))


cowplot::plot_grid(p1, p2, p3, p4, p6,  ncol = 1)




# ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/n_of_1_numberTaps.png", plot=p1, height=3.5, width=10, units="in", dpi=300)
# ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/n_of_1_numberTaps.tiff", plot=p1, height=3.5, width=10, units="in", dpi=300)
# 
