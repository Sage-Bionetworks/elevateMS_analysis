Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools')
install_load("plotly")
install_load("ggpubr")


source("analysis/loadData.R")

userContrib <- userActivity %>% dplyr::group_by(healthCode, dataGroups, originalTable) %>% dplyr::summarise(numAct = n_distinct(participant_day))
userContrib <- userContrib %>% spread(originalTable, numAct)

# annot <- userContrib[, c(1:2)]
# dataMat <- userContrib[, c(3:ncol(userContrib))]
# pheatmap::pheatmap(dataMat)
# rownames(userContrib) <- userContrib$healthCode

dailyCheckins <- getDailyCheckin()
triggers <- getTriggerData()
symptoms <- getSymptomsData() 
tappingFeatures <- getTappingF()
nQOL_uppExtremity <- get_nQOL_uppExtremity()
nQOL_lowExtremity <- get_nQOL_lowExtremity()
nQOL_cognition <- get_nQOL_cognition()
relapseSurveys <- getRelapseData()


userTimeline <- function(userId){
  daily_checkin <- dailyCheckins %>% filter(healthCode == userId)
  daily_triggers <- triggers %>% filter(healthCode == userId)
  daily_symptoms <- symptoms %>% filter(healthCode == userId)
  daily_tapScores <- tappingFeatures %>% filter(healthCode == userId)
  daily_nQOL_uppExtremity <-  nQOL_uppExtremity %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-uppExtremity')
  daily_nQOL_lowExtremity <-  nQOL_lowExtremity %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-lowExtremity')
  daily_nQOL_cognition <- nQOL_cognition %>% dplyr::mutate(healthCode = as.character(healthCode)) %>%
    dplyr::filter(healthCode == userId) %>% mutate(survey='nQOL-Cognition')
  daily_nQOL <- rbind(daily_nQOL_cognition[,c('healthCode', 'participant_day', 'T-Score', 'survey')],
                      daily_nQOL_lowExtremity[,c('healthCode', 'participant_day', 'T-Score', 'survey')],
                      daily_nQOL_uppExtremity[,c('healthCode', 'participant_day', 'T-Score', 'survey')])
  daily_nQOL['TScore'] = daily_nQOL['T-Score']
  daily_nQOL <- daily_nQOL %>% dplyr::mutate(survey = gsub('nQOL-', '', survey))
  
  symptoms_per_week <- daily_symptoms %>% dplyr::group_by(healthCode, participant_week) %>% 
         dplyr::summarise(numTimesSymptomsReported = n_distinct(activity_start_timestamp_local)) %>%
         dplyr::mutate(participant_day = participant_week * 7)
  
  daily_symptoms_summary <- daily_symptoms %>%  dplyr::group_by(healthCode, participant_day) %>% 
    dplyr::summarise(numSymptoms = length(symptom))
  user_checkIns <- dailyCheckins %>% filter(healthCode == userId) %>% 
    select(healthCode, participant_day, mood_health, mood_pain, mood_mobility) %>% gather(type, dailyScore, 3:5)
  
  # write.table(daily_checkin, file='n1_daily_checkins.tsv', sep="\t", quote=F, row.names = F)
  # write.table(daily_triggers, file='n1_daily_triggers.tsv', sep="\t", quote=F, row.names = F)
  # write.table(daily_symptoms, file='n1_daily_symptoms.tsv', sep="\t", quote=F, row.names = F)
  #ggplot(data=daily_tapScores, aes(x=participant_day, y=numberTaps, color=hand)) + geom_point() + theme_bw()
  
  p1 <- ggplot(data=daily_tapScores, aes(x=participant_day, y=numberTaps, color=hand)) + geom_point() + theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
  p1 <- p1 + theme(legend.position = "bottom")
  #p2 <- ggplot(data=daily_tapScores, aes(x=participant_day, y=activityDuration, color=hand)) + geom_point() + theme_bw() + scale_x_continuous(limits=c(1,84))
  #p2 <- ggplot(data=daily_tapScores, aes(x=participant_day, y=activityDuration, color=hand)) + geom_point() + theme_bw() + scale_x_continuous(limits=c(1,84))
  #p3 <- ggplot(data=daily_symptoms_summary, aes(x=participant_day, y=numSymptoms, color=severity)) + geom_line() + geom_point() + theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
  #p3 <- ggplot(data=daily_symptoms_summary, aes(x=participant_day, y=numSymptoms)) + geom_col() + theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
  p3 <- ggplot(data=symptoms_per_week, aes(x=participant_day, y=numTimesSymptomsReported)) + geom_line(linetype = "dashed") + geom_point() + theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
  p3 <- p3 + ylab('frequency of symptoms reported') +  theme(legend.position = "bottom")
  
  p4 <- ggplot(data=daily_nQOL, aes(x=participant_day, y=TScore, color=survey)) + geom_line() + geom_point() + theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
  p4 <- p4 + theme(legend.position = "bottom")
  p5 <- ggplot(data=user_checkIns, aes(x=participant_day, y=dailyScore, color=type)) + geom_jitter() + theme_bw() + scale_x_continuous(limits=c(1,84), breaks=c(1,10,20,30,40,50,60,70,80,84))
  p5 <- p5 +  theme(legend.position = "bottom")
  
  user_relapse <- relapseSurveys %>% filter(healthCode == userId)
  if(nrow(user_relapse) > 0){
    p1 <- p1 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                          color = "grey50", size=.5) + theme(text = element_text(size=12)) 
    p3 <- p3 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                          color = "grey50", size=.5) + theme(text = element_text(size=12)) 
    p4 <- p4 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                          color = "grey50", size=.5) + theme(text = element_text(size=12)) 
    p5 <- p5 + geom_vline(xintercept = user_relapse$participant_day, linetype="longdash", 
                          color = "grey50", size=.5) + theme(text = element_text(size=12)) 
  }
  p <- gridExtra::grid.arrange(p1,p3,p5, nrow = 3)
}



p1 = 'd744c79f-d5ef-4c96-9685-59da54aec644'
userTimeline(p1)

p2 = '1bc84bf7-99da-4ba8-887b-be4b5dbae4c7'
userTimeline(p2)

p3 = '8ceb5e62-e9ef-4bf9-8902-5ef6db1cd513'
p3_plot <- userTimeline(p3)
ggsave("analysis/PAPER-1/FINAL_FIGS/N_of_1_ID_8ceb5e62-e9ef-4bf9-8902-5ef6db1cd513.png", plot=p3_plot, width=7, height=10, dpi=150)
ggsave("analysis/PAPER-1/FINAL_FIGS/N_of_1_ID_8ceb5e62-e9ef-4bf9-8902-5ef6db1cd513.tiff", plot=p3_plot, width=7, height=10, dpi=150)

p4 = 'a45a126b-cc26-4e68-8588-7c12f7ce5dbd'
userTimeline(p4)

p5 = '405df1ec-4879-43bb-b28c-f7bd9ea775e4'
userTimeline(p5)





#####
# Triggers across users
#####
x <- triggers %>% ddply(.variables = colnames(daily_triggers),
                        .fun = function(df){
                          triggers = unlist(strsplit(df$triggers, split=','))
                          data.frame(triggers = triggers)
                        })
ALL_TRIGGERS <- unique(x$triggers)

perUserTriggers = x %>% filter( dataGroups == 'ms_patient') %>% ddply(.variables = c('healthCode'),
                                                                      .fun = function(df) {
                                                                        df = data.frame(present = ALL_TRIGGERS %in% unique(df$triggers),
                                                                                        triggers = ALL_TRIGGERS)
                                                                        df$present[df$present == T] = 1
                                                                        df$present[df$present == F] = 0
                                                                        df
                                                                      })

View(perUserTriggers)



perUserTriggers <- perUserTriggers %>% spread(triggers, present)
tmp_mat <- perUserTriggers
tmp_mat$healthCode <- NULL
pheatmap::pheatmap(t(tmp_mat), color = c('#FDF0D5', '#D81E5B'), clustering_method = "centroid")




ALL_SYMPTOMS <- unique(symptoms$symptom)
perUserSymptoms <- symptoms %>% filter( dataGroups == 'ms_patient') %>% ddply(.variables = c('healthCode'),
                                                                              .fun = function(df) {
                                                                                df = data.frame(present = ALL_SYMPTOMS %in% unique(df$symptom),
                                                                                                symptoms = ALL_SYMPTOMS)
                                                                                df$present[df$present == T] = 1
                                                                                df$present[df$present == F] = 0
                                                                                df
                                                                              })
perUserSymptoms <- perUserSymptoms %>% spread(symptoms, present)
tmp_mat <- perUserSymptoms
tmp_mat$healthCode <- NULL
pheatmap::pheatmap(t(tmp_mat), color = c('#FDF0D5', '#D81E5B'), clustering_method = "centroid")
