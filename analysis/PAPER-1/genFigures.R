rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools')
install_load("plotly", "tableone")
install_load('zipcode', 'maps', 'viridis', 'leaflet' )


# LOAD Data
FREEZED_DATA_LOC = "syn11657929"
load(synGet(FREEZED_DATA_LOC)@filePath)
ls()
# Objects in loaded workspace
#FREEZE_DATE
#DAYS_SINCE_LAUNCH
#WEEKS_SINCE_LAUNCH
# triggers 
# dailyCheckins 
# relapses 
# symptoms 
# nQOL_uppExtremity 
# nQOL_lowExtremity 
# nQOL_cognition  
# tapF 
# walkF 
# restF 
# userActivity 
# demog 

stringfy <- function(x){
  gsub('[\\[\"\\]]','',x, perl=T) 
}


userActivity <- userActivity %>% dplyr::filter(lubridate::ymd_hms(createdOn)  <= Sys.Date())
STUDY_HEALTHCODES <- unique(userActivity$healthCode)
total_elevateMS_users <- n_distinct(userActivity$healthCode)
num_MS_patients <- userActivity %>% dplyr::filter(dataGroups %like% 'ms_patient') %>% .$healthCode %>% n_distinct()
num_controls <- userActivity %>% dplyr::filter(dataGroups %like% 'control') %>% .$healthCode %>% n_distinct()
MS_patients_dataContrib <- round(table(userActivity$dataGroups) / nrow(userActivity) * 100, digits=2)[2]


# Data Freeze Date: 
FREEZE_DATE
# Weeks Since Launch:   
WEEKS_SINCE_LAUNCH

#MS patients: 
num_MS_patients
# Controls:     
num_controls

#Data Contributed (MS patients): 
MS_patients_dataContrib

#Age:      
age_mean <- round(mean(demog$age, na.rm=T), digits=2)
age_sd <- round(sd(demog$age, na.rm=T), digits=2)
age_mean 
age_sd

#missingness
apply(demog, 2, function(x){
  sum(!is.na(x))
})


#Gender
round(prop.table(table(demog$gender))*100, digits=2)

#Fix race
to_replace_1 <- demog$race %in% c('Black_or_African', 'Black_or_African,Caribbean', 'Caribbean')
demog$race[to_replace_1] = 'Black/African/Caribbean'
to_replace_2 <- demog$race %in% c('Caucasian', 'Caucasian,Latino_Hispanic', 'Caucasian,Native_American')
demog$race[to_replace_2] = 'Caucasian'
to_replace_3 <- demog$race %in% c('Black_or_African,Caribbean,Latino_Hispanic', 'Latino_Hispanic')
demog$race[to_replace_3] = 'Latino/Hispanic'
to_replace_4 <- demog$race %in% c('Asian', 'Asian,Pacific_Islander')
demog$race[to_replace_4] = 'Asian'
to_replace_5 <- demog$race %in% c('Middle_Eastern', 'Native_American')
demog$race[to_replace_5] = 'Others'

prop.table(table(demog$race))

table(userActivity$originalTable)


#Create a variable list which we want in Table 1
listVars <- c("age", "gender", "height", "weight", "education", "health_insurance", "employment", "race")

#Define categorical variables
catVars <- c("gender","dataGroups", "education","health_insurance", "employment", "race")

demogTable = CreateTableOne(data=demog, vars=listVars, factorVars = catVars, strata = c("dataGroups"))
demogTableMat <- print(demogTable, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
print(demogTableMat)
## Save to a CSV file
write.csv(demogTableMat, file = "elevateMS_demog_table.csv")


#### Enrollment Map
### Participant City
data(zipcode)
states = data.frame(state = state.abb, state.name = tolower(state.name))
tmp_zipcode <- zipcode %>% inner_join(states)
tmp_zipcode['zipcode_firstThree'] = substr(tmp_zipcode$zip, 1,3)
tmp_zipcode <- tmp_zipcode %>% dplyr::group_by(state, state.name, zipcode_firstThree) %>% 
  dplyr::summarise(longitude = median(longitude), latitude = median(latitude)) %>% as.data.frame() %>%
  dplyr::mutate(state = state.name) %>% dplyr::select(-state.name)
demog['zipcode_firstThree'] = substr(demog$zipcode, 1,3)

# table(tmp_zipcode$state.name)
# install_load("fiftystater")
# data("fifty_states")
# table(fifty_states$id)
tmp_user_region <- merge(demog, tmp_zipcode, by.x="zipcode_firstThree")
#TMP FIX - needs to be fixed 
tmp_user_region <- tmp_user_region %>% filter(!state %in% c('alaska', 'hawaii'))

library(fiftystater) # https://github.com/wmurphyrd/fiftystater
data("fifty_states") # this line is optional due to lazy data loading

#plot all states with ggplot
p <- ggplot()
p <- p + geom_polygon( data=fifty_states, aes(x=long, y=lat, group = group),colour="white", fill="grey90" ) 
p <- p + geom_jitter( data=tmp_user_region, aes(x=longitude, y=latitude, color=dataGroups), size=.8) + theme_few()
elevateMS_USmap <- p + scale_color_manual(values=c("#A5BE00", "#FC4E07"),  labels=c('Control', 'MS patient')) + 
  theme(legend.position = c(0.85, 0.3), legend.text = element_text(size=10), 
        legend.title = element_text(size=0),
        axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(), panel.border=element_blank(), panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), plot.background=element_blank())
elevateMS_USmap
ggsave("analysis/PAPER-1/FINAL_FIGS/enrollment_map.jpeg", plot=elevateMS_USmap, height=5, width=8, units="in", dpi=400)
ggsave("analysis/PAPER-1/FINAL_FIGS/enrollment_map.tiff", plot=elevateMS_USmap, height=5, width=8, units="in", dpi=400)



### Enrollment
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
p1 <- ggplot(data=enrollment, aes(x=week, y=num, fill=type )) + geom_bar(stat="identity", position="dodge", width = .7) + theme_bw() + scale_x_discrete(limits=seq(1,max(enrollment$week),2))  + ylab('participants enrolled') + xlab('week since launch')
p1 <- p1 + theme_few() + scale_fill_manual(values=c("#A5BE00", "#FC4E07"),  labels=c('Control', 'MS patient')) + theme(legend.position = c(0.80, 0.7))
p1 <- p1 + theme(text = element_text(size=10)) 
p1
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/enrollment.jpeg", plot=p1, height=2.5, width=4, units="in", dpi=100)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/enrollment.tiff", plot=p1, height=53, width=80, units="mm", dpi=100)


### This is mainly to figure out the #users that have been in study for atleast #weeks irrespective of the data contributed
user_time_in_study <- userActivity %>% dplyr::group_by(healthCode, dataGroups) %>%
  dplyr::summarise(participantStartDate = min(lubridate::date(createdOn))) %>%
  dplyr::mutate(freezeDate = FREEZE_DATE,
                days_in_study =  as.numeric(freezeDate - participantStartDate) + 1,
                weeks_in_study =  ((days_in_study - 1 ) %/% 7 ) + 1)
tmp = as.data.frame.matrix(table(user_time_in_study$weeks_in_study, user_time_in_study$dataGroups))
tmp['ms_patient'] = rev(cumsum(rev(tmp$ms_patient)))
tmp['control'] = rev(cumsum(rev(tmp$control)))
tmp['participant_week'] = rownames(tmp)
userWeeks <- tmp %>% gather(dataGroups, totalUsersReachedThisWeek, 1:2)

#People active each week since study start
userActiveEachWeek <- userActivity %>% dplyr::group_by(dataGroups, study_week) %>%
  dplyr::summarise(uniqUsers = n_distinct(healthCode))
p2 <- ggplot(data=userActiveEachWeek, aes(x=study_week, y=uniqUsers, fill=dataGroups )) + geom_bar(stat="identity", width = .5) + theme_bw() + scale_x_discrete(limits=seq(1,max(userActiveEachWeek$study_week),2))
p2 <- p2 +  labs(col="User type\n") + scale_fill_manual(values=c("#A5BE00", "#FC4E07"), labels=c('Control', 'MS patient')) 
p2 <- p2 + theme_few() + theme(legend.position = c(0.85, 0.8))
p2 <- p2 + theme(text = element_text(size=10)) + ggtitle('') + ylab('active participants') + xlab('week since launch')
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/activeUsers.jpeg", plot=p2, height=2.5, width=4, units="in", dpi=100)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/activeUsers.tiff", plot=p2, height=53, width=80, units="mm", dpi=100)



#### Participant Attrition
#Since the study is still going on we are not dividing by the all users rather number of unique users in each week of the study
overallCompliance <- userActivity %>% dplyr::group_by(dataGroups, participant_week) %>%
  dplyr::summarise(uniqUsers = n_distinct(healthCode))
overallCompliance <- merge(overallCompliance, userWeeks)
overallCompliance <- overallCompliance %>% 
  dplyr::mutate(percent = round( (uniqUsers /totalUsersReachedThisWeek)*100, digits=2),
                dataGroups = plyr::revalue(dataGroups, replace=c('ms_patient' = 'MS patient')))
# p3 <- ggplot(data=overallCompliance, aes(x=participant_week, y=percent, fill=dataGroups )) + geom_bar(stat="identity", position="dodge", width = .4) + theme_bw() 
# p3 <- p3 + theme_few() + scale_fill_manual(values=c("#A5BE00", "#FC4E07")) +  theme(text = element_text(size=15))
# p3 <- p3 + xlab('weeks enrolled') +  theme(text = element_text(size=12)) + scale_x_discrete(limits=seq(1,12,1))
p3 <- ggplot(data=overallCompliance, aes(x=participant_week, y=percent, color=dataGroups )) + geom_point(size=.7) + theme_bw() + geom_line()
p3 <- p3 + theme_few() + scale_color_manual(values=c("#A5BE00", "#FC4E07")) +  theme(text = element_text(size=15))
p3 <- p3 + xlab('weeks enrolled') +  theme(text = element_text(size=12)) + scale_x_discrete(limits=seq(1,12,1)) 
p3 <- p3 + theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.title = element_blank())
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/userAttrition.jpeg", plot=p3, height=3, width=4, units="in", dpi=150)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/userAttrition.tiff", plot=p3, height=3, width=4, units="in", dpi=150)



### Activities / week / participant
userActivityperWeek = userActivity %>% group_by(healthCode, dataGroups, participant_week) %>% summarise(numActivity = n()) 
p <- ggplot(data=userActivityperWeek, aes(x=as.factor(participant_week), y=numActivity, color=dataGroups )) + geom_boxplot() + theme_bw()
p <- p + theme_few() + scale_color_manual(values=c("#A5BE00", "#FC4E07")) +  theme(text = element_text(size=15))
p <- p + xlab('weeks enrolled') +  theme(text = element_text(size=12)) + scale_x_discrete(limits=seq(1,12,1)) 
p <- p + theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.title = element_blank())
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/perWeekActivity.jpeg", plot=p, height=3, width=4, units="in", dpi=150)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/perWeekActivity.tiff", plot=p, height=3, width=4, units="in", dpi=150)



#### When are people interacting with App
appTouches <- rbind(dailyCheckins %>% select(activity_start_timestamp_local) %>% mutate(activity = 'dailyCheckin'),
                    tapF %>% select(activity_start_timestamp_local) %>% mutate(activity = 'tapping'),
                    walkF %>% select(activity_start_timestamp_local) %>% mutate(activity = 'walk'),
                    triggers %>% select(activity_start_timestamp_local) %>% mutate(activity = 'triggers'),
                    symptoms %>% select(activity_start_timestamp_local) %>% mutate(activity='symptoms'),
                    userActivity %>% filter(originalTable == 'Tremor Activity-v5') %>% 
                      mutate(activity_start_timestamp_local = createdOn_localTime) %>% select(activity_start_timestamp_local) %>%
                      mutate(activity='tremor'))
appTouches <- appTouches %>% dplyr::mutate(hod = lubridate::hour(appTouches$activity_start_timestamp_local))
appTouches$activity = factor(appTouches$activity, levels=c('tapping', 'tremor', 'walk', 'dailyCheckin', 'symptoms', 'triggers'))
p1 <- ggplot(data=appTouches, aes(x=hod, fill=activity)) + geom_density(alpha=.6) + theme_few() + facet_grid(.  ~ activity) 
p1 <- p1 + xlab('hour of day') + theme(legend.position = "none")
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/activityCompletionTime.jpeg", plot=p1, height=3, width=8, units="in", dpi=200)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/activityCompletionTime.tiff", plot=p1, height=3, width=8, units="in", dpi=200)


####Employment
employment = prop.table(table(demog$employment)) * 100
employment = data.frame(type = names(employment), percent=as.numeric(employment))
colors <- c( '#F25F5C', '#247BA0', "#70C1B3", "#FFE066", "#50514F")
p1_employment <- plot_ly(employment, labels = ~type, values = ~percent, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) 
p1_employment



tmp_df <- data.frame(dataGroups = c('control', 'ms_patient'),
                     num = c(num_controls, num_MS_patients), stringsAsFactors = F)
by_employment = demog %>% filter(!is.na(employment)) %>% inner_join(tmp_df) %>% dplyr::group_by(dataGroups, employment, num) %>% dplyr::summarise(prop =n_distinct(healthCode)) %>% mutate(prop = round((prop / num)*100, digits=2)) %>% as.data.frame()
new_levels = by_employment %>% group_by(employment) %>% summarise(prop = sum(prop)) %>% arrange(prop) %>% .$employment
by_employment  <- by_employment %>% dplyr::mutate(employment = factor(employment, levels=new_levels))
p1 <- ggplot(data=by_employment, aes(x=employment, y=prop, fill=dataGroups )) + geom_bar(stat="identity", width = .7) + theme_bw()  + ylab('percent')
p1 <- p1 + theme_few() + scale_fill_manual(values=c("#A5BE00", "#FC4E07"),  labels=c('Control', 'MS patient')) + theme(legend.position = c(0.8, 0.2))
employment_plot <- p1 + theme(text = element_text(size=10)) + ggtitle('Employment type') + coord_flip()
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/employment.jpeg", plot=employment_plot, height=3, width=4, units="in", dpi=100)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/employment.tiff", plot=employment_plot, height=3, width=4, units="in", dpi=100)



health_insurance = demog %>% filter(!is.na(health_insurance)) %>% inner_join(tmp_df) %>% dplyr::group_by(dataGroups, health_insurance, num) %>% dplyr::summarise(prop =n_distinct(healthCode)) %>% mutate(prop = round((prop / num)*100, digits=2))
levels <- health_insurance %>% filter(dataGroups == 'ms_patient') %>% arrange(desc(prop)) %>% .$health_insurance
health_insurance$health_insurance <- factor(health_insurance$health_insurance, levels=rev(levels))
p4 <- ggplot(data=health_insurance, aes(x=health_insurance, y=prop, fill=dataGroups )) + geom_bar(stat="identity", width = .7) + theme_bw()  + ylab('percent')
p4 <- p4 + theme_few() + scale_fill_manual(values=c("#A5BE00", "#FC4E07"),  labels=c('Control', 'MS patient')) + theme(legend.position = c(0.8, 0.2))
p4 <- p4 + theme(text = element_text(size=10)) + ggtitle('Health Insurance') + coord_flip() + xlab('')
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/healthInsurance.jpeg", plot=p4, height=3, width=4, units="in", dpi=100)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/healthInsurance.tiff", plot=p4, height=3, width=4, units="in", dpi=100)
