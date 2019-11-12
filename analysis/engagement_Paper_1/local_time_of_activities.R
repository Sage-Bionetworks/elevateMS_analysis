rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')
install_load("plotly", "tableone", "webshot")
install_load('zipcode', 'maps', 'viridis', 'leaflet', 'RSelenium' )
synapser::synLogin()

#source("analysis/loadData.R")
load(synapser::synGet("syn11657929")$path)
ls()

appTouches <- userActivity %>% 
  mutate(activitytype = case_when(
  grepl('Tremor', originalTable) ~ 'sensor-based',
  grepl('Walking', originalTable) ~ 'sensor-based',
  grepl('Brain', originalTable) ~ 'sensor-based',
  grepl('Triggers', originalTable) ~ 'sensor-based',
  grepl('Symptoms', originalTable) ~ 'survey-based',
  grepl('Tapping', originalTable) ~ 'survey-based',
  grepl('Daily Check', originalTable) ~ 'survey-based'
  #grepl('NeuroQOL|Medication|MSIS|Profile|WPAI', originalTable) ~ 'survey-based'
))  %>%
  dplyr::select(createdOn_localTime, activitytype) %>%
  dplyr::mutate(hod = lubridate::hour(createdOn_localTime)) %>%
  filter(!is.na(activitytype)) %>%
  mutate(activitytype = factor(activitytype, levels=c('sensor-based', 'survey-based')),
         tod_class = cut(hod, breaks = c(0,4, 8, 12, 16, 20, 24),
             labels = c('late-night', 'early-morning', 'morning',
                        'afternoon', 'evening', 'night')))



appTouches_percent_by_todClass <- appTouches %>% 
  filter(!is.na(tod_class)) %>%
  count(activitytype, tod_class) %>% 
  group_by(activitytype) %>%
  mutate(total = sum(n),
         percent = round( (n / total) * 100, digits=2)) %>%
  mutate(tod_class = factor(tod_class ,
                            levels=rev(c('early-morning', 'morning', 
                            'afternoon', 'evening', 'night', 'late-night'))))
  
appTouches_percent_by_todClass
  
p <- ggplot(data=appTouches_percent_by_todClass, aes(x=tod_class, y = percent, fill=activitytype)) 
p <- p + geom_bar(stat = "identity", position = "dodge", width=.8) + coord_flip()
p <- p + theme_bw(base_size = 12) + scale_fill_manual('activity type', values =c('#8491B4FF', '#B09C85FF'))
p <- p + theme(legend.position = "none") + xlab('time of day')
p
ggsave("analysis/engagement_Paper_1/FINAL_FIGS/activityCompletion_prop_byTimeofDay.jpeg", plot=p, height=2, width=3, units="in", dpi=100)
ggsave("analysis/engagement_Paper_1/FINAL_FIGS/activityCompletion_prop_byTimeofDay.tiff", plot=p, height=2, width=3, units="in", dpi=200)



+ geom_density(alpha=.6) + theme_few()  
p <- p + xlab('hour of day') + theme(legend.position = "top") + scale_fill_manual('', values =c('#8491B4FF', '#B09C85FF'))
p <- p + theme(legend.key.size = unit(0.3, "cm"), text = element_text(size=10))
p <- p + theme_light(base_size = 12) + theme(legend.position = "top")




########
### When are people interacting with App
########
p <- ggplot(data=appTouches, aes(x=hod, fill=activitytype)) + geom_density(alpha=.6) + theme_few()  
p <- p + xlab('hour of day') + theme(legend.position = "top") + scale_fill_manual('', values =c('#8491B4FF', '#B09C85FF'))
p <- p + theme(legend.key.size = unit(0.3, "cm"), text = element_text(size=10))
p <- p + theme_light(base_size = 12) + theme(legend.position = "top")
p


p <- ggplot(data=appTouches, aes(x=hod, fill=activitytype)) + geom_histogram(alpha=.6) + theme_few()  
p <- p + xlab('hour of day') + theme(legend.position = "top") + scale_fill_manual('', values =c('#8491B4FF', '#B09C85FF'))
p <- p + theme(legend.key.size = unit(0.3, "cm"), text = element_text(size=10))
p <- p + theme_light(base_size = 12) + theme(legend.position = "top")
p


appTouches
ggsave("analysis/engagement_Paper_1/FINAL_FIGS/activityCompletionTime_densityHist.jpeg", plot=p, height=5, width=7, units="in", dpi=300)
ggsave("analysis/engagement_Paper_1/FINAL_FIGS/activityCompletionTime_densityHist.tiff", plot=p, height=5, width=7, units="in", dpi=300)


### Confirming by KS test 
sensorBased <- appTouches %>% filter(activitytype == 'sensor-based')
surveyBased <- appTouches %>% filter(activitytype == 'survey-based')
ks.test(sensorBased$hod, surveyBased$hod)

