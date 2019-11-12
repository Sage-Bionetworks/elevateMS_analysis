rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapser)
synapser::synLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools')

source("analysis/loadData.R")


userActivity <- userActivity %>% 
  inner_join(baselineChar %>% select(healthCode, referred_by_clinician))

### Enrollment summary
enrollment <- userActivity %>% select(healthCode, study_week, dataGroups) 
enrollment <- enrollment[!duplicated(enrollment),]
enrollment <- plyr::ldply(1:max(enrollment$study_week), function(x){
  untilNow <- enrollment %>% filter(study_week < x) %>% .$healthCode %>% unique()
  thisWeek <- enrollment %>% filter(study_week == x) %>% .$healthCode %>% unique()
  newUsers_thisweek <- setdiff(thisWeek, untilNow)
  res <- enrollment %>% select(healthCode, dataGroups)
  res <- res[!duplicated(res),]
  res <- res %>% filter(healthCode %in% newUsers_thisweek)
  data.frame('week'=x,
             'ms_patients' = res %>% filter(dataGroups == 'ms_patient') %>% nrow(),
             'controls' = res %>% filter(dataGroups == 'control') %>% nrow()) %>%
    gather(type, num, 2:3)
})
enrollment <- enrollment %>% mutate(type = factor(type))
p1 <- ggplot(data=enrollment, aes(x=week, y=num, fill=type )) + geom_bar(stat="identity", width = .5) + theme_bw() + scale_x_discrete(limits=seq(0,max(enrollment$week),5))  + ylab('#people enrolled') + xlab('weeks since launch')
p1 <- p1 + theme_few() + scale_fill_manual(values=c(COL_CONTROL,COL_MS_PATIENT),  labels=c('Control', 'MS patient')) + theme(legend.position = c(0.85, 0.9))
p1 <- p1 + theme(text = element_text(size=15)) + ggtitle('#Users joining per week')
#p1 <- p1 + geom_vline(xintercept=WEEKS_SINCE_LAUNCH, colour="grey40", size=1, linetype=3)
p1
ggsave("analysis/engagement_Paper_1/FINAL_FIGS/enrollment.jpeg", plot=p1,
       width = 8, height = 4, dpi = 150, units="in")

#number of Users active each week
activeusers_per_week <- userActivity %>% select(healthCode, study_week, dataGroups) %>%
  dplyr::group_by(study_week, dataGroups) %>%
  dplyr::summarise(n = n_distinct(healthCode))
p2 <- ggplot(data=activeusers_per_week, aes(x=study_week, y=n, fill=dataGroups )) + geom_bar(stat="identity", width = .5) + theme_bw() + scale_x_discrete(limits=seq(0,max(enrollment$week),5))  + ylab('#active users') + xlab('weeks since launch')
p2 <- p2 + theme_few() + scale_fill_manual(values=c(COL_CONTROL,COL_MS_PATIENT),  labels=c('Control', 'MS patient')) + theme(legend.position = c(0.85, 0.9))
p2 <- p2 + theme(text = element_text(size=15)) + ggtitle('#Active users per week')
p2
ggsave("analysis/engagement_Paper_1/FINAL_FIGS/activeUsers.jpeg", plot=p2,
       width = 8, height = 4, dpi = 150, units="in")











# #plot all states with ggplot
# p <- ggplot()
# p <- p + geom_polygon( data=fifty_states, aes(x=long, y=lat, group = group),colour="white", fill="grey90" ) 
# p <- p + geom_jitter( data=tmp_user_region, aes(x=longitude, y=latitude, color=dataGroups), size=.8) + theme_few()
# elevateMS_USmap <- p + scale_color_manual(values=c("#A5BE00", "#FC4E07"),  labels=c('Control', 'MS patient')) + 
#   theme(legend.position = c(0.85, 0.3), legend.text = element_text(size=10), 
#         legend.title = element_text(size=0),
#         axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
#         axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
#         panel.background=element_blank(), panel.border=element_blank(), panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(), plot.background=element_blank())
# elevateMS_USmap
# ggsave("analysis/PAPER-1/FINAL_FIGS/enrollment_map.jpeg", plot=elevateMS_USmap, height=5, width=8, units="in", dpi=400)
# ggsave("analysis/PAPER-1/FINAL_FIGS/enrollment_map.tiff", plot=elevateMS_USmap, height=5, width=8, units="in", dpi=400)

# 
# 
# ### Enrollment
# enrollment <- userActivity %>% select(healthCode, study_week, dataGroups) 
# enrollment <- enrollment[!duplicated(enrollment),]
# enrollment <- plyr::ldply(1:max(enrollment$study_week), function(x){
#   untilNow <- enrollment %>% filter(study_week < x) %>% .$healthCode %>% unique()
#   thisWeek <- enrollment %>% filter(study_week == x) %>% .$healthCode %>% unique()
#   newUsers_thisweek <- setdiff(thisWeek, untilNow)
#   res <- enrollment %>% select(healthCode, dataGroups)
#   res <- res[!duplicated(res),]
#   res <- res %>% filter(healthCode %in% newUsers_thisweek)
#   data.frame('week'=x,
#              'ms_patients' = res %>% filter(dataGroups == 'ms_patient') %>% nrow(),
#              'controls' = res %>% filter(dataGroups == 'control') %>% nrow()) %>%
#     gather(type, num, 2:3)
# })
# enrollment <- enrollment %>% mutate(type = factor(type))
# p1 <- ggplot(data=enrollment, aes(x=week, y=num, fill=type )) + geom_bar(stat="identity", position="dodge", width = .7) + theme_bw() + scale_x_discrete(limits=seq(1,max(enrollment$week),2))  + ylab('participants enrolled') + xlab('week since launch')
# p1 <- p1 + theme_few() + scale_fill_manual(values=c("#A5BE00", "#FC4E07"),  labels=c('Control', 'MS patient')) + theme(legend.position = c(0.80, 0.7))
# p1 <- p1 + theme(text = element_text(size=10)) 
# p1
# ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/enrollment.jpeg", plot=p1, height=2.5, width=4, units="in", dpi=100)
# ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/enrollment.tiff", plot=p1, height=53, width=80, units="mm", dpi=100)

# 
# ### This is mainly to figure out the #users that have been in study for atleast #weeks irrespective of the data contributed
# user_time_in_study <- userActivity %>% dplyr::group_by(healthCode, dataGroups) %>%
#   dplyr::summarise(participantStartDate = min(lubridate::date(createdOn))) %>%
#   dplyr::mutate(freezeDate = FREEZE_DATE,
#                 days_in_study =  as.numeric(freezeDate - participantStartDate) + 1,
#                 weeks_in_study =  ((days_in_study - 1 ) %/% 7 ) + 1)
# tmp = as.data.frame.matrix(table(user_time_in_study$weeks_in_study, user_time_in_study$dataGroups))
# tmp['ms_patient'] = rev(cumsum(rev(tmp$ms_patient)))
# tmp['control'] = rev(cumsum(rev(tmp$control)))
# tmp['participant_week'] = rownames(tmp)
# userWeeks <- tmp %>% gather(dataGroups, totalUsersReachedThisWeek, 1:2)
# 
# #People active each week since study start
# userActiveEachWeek <- userActivity %>% dplyr::group_by(dataGroups, study_week) %>%
#   dplyr::summarise(uniqUsers = n_distinct(healthCode))
# p2 <- ggplot(data=userActiveEachWeek, aes(x=study_week, y=uniqUsers, fill=dataGroups )) + geom_bar(stat="identity", width = .5) + theme_bw() + scale_x_discrete(limits=seq(1,max(userActiveEachWeek$study_week),2))
# p2 <- p2 +  labs(col="User type\n") + scale_fill_manual(values=c("#A5BE00", "#FC4E07"), labels=c('Control', 'MS')) 
# p2 <- p2 + theme_few() + theme(legend.position = c(0.85, 0.8))
# p2 <- p2 + theme(text = element_text(size=10)) + ggtitle('') + ylab('active participants') + xlab('week since launch')
# p2
# # ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/activeUsers.jpeg", plot=p2, height=2.5, width=4, units="in", dpi=100)
# # ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/activeUsers.tiff", plot=p2, height=53, width=80, units="mm", dpi=100)








