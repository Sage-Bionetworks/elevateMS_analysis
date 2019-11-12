rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')
install_load("plotly", "tableone", "webshot")
install_load('zipcode', 'maps', 'viridis', 'leaflet', 'RSelenium' )
synapser::synLogin()

#source("analysis/loadData.R")
load(synGet("syn11657929")$path)

## Total enrolled 
n_distinct(baselineChar$healthCode)

#groups
prop.table(table(baselineChar$dataGroups)) * 100

### Numbers in three groups
table(baselineChar$group)


### Potential errors in self-reported stats
### controls who report data that is indicative of MS patient 
table(baselineChar$error)
baselineChar_flt <- baselineChar %>% filter(dataGroups %in% c('control', 'ms_patient'))
table(baselineChar_flt$group)


##########
#1. Demog- comparison between case, MS and MS with clinical referral
##########
#Create a variable list which we want in Table 1
listVars <- c("age", "gender", "race", "education", "health_insurance", 
              "employment")

#Define categorical variables
catVars <- c("gender", "race", "education","health_insurance", "employment")
demogTable = CreateTableOne(data=baselineChar_flt, vars=listVars, factorVars = catVars, strata = c("group"))
demogTableMat <- print(demogTable, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
print(demogTableMat)
## Save to a CSV file
write.csv(demogTableMat, file = "analysis/engagement_Paper_1/FINAL_FIGS/elevateMS_overall_demog_table.csv")


##########
#2. Within MS - clinical vs in-the wild referral
##########
#Create a variable list which we want in Table 1
listVars <- c("currentDiagnosis", "msFamilyHistory",
              "YearsSinceDiagnosis", "currentDMT", "YearsSinceFirstDMT", "overallPhysicalAbility")
#Define categorical variables
catVars <- c( "currentDiagnosis", "currentDMT", "msFamilyHistory",
             "health_insurance", "employment", "userSharingScope", "overallPhysicalAbility")

demogTable = CreateTableOne(data=baselineChar_flt %>% filter(dataGroups == 'ms_patient'), 
                            vars=listVars, factorVars = catVars, strata = c("group"))
demogTableMat <- print(demogTable, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
print(demogTableMat)
## Save to a CSV file
write.csv(demogTableMat, file = "analysis/engagement_Paper_1/FINAL_FIGS/elevateMS_MSpatients_diseaseChars_table.csv")


####################
#Data Missingness
####################
install_load("naniar")
tmp <- baselineChar_flt %>% select(-error, -externalId, -group)
missingNess_overall <- gg_miss_var(tmp, show_pct = T) +  theme(axis.text.y = element_text(size=13)) 
missingNess_control <- gg_miss_var(tmp %>% filter(dataGroups == 'control'),
                                   show_pct = T) +  theme(axis.text.y = element_text(size=13)) 
missingNess_MS <- gg_miss_var(tmp %>% filter(dataGroups == 'ms_patient'), show_pct = T) +  theme(axis.text.y = element_text(size=13)) 
missingNess_MS_referred_cohort <- gg_miss_var(tmp %>% filter(dataGroups == 'ms_patient' & referred_by_clinician == T), show_pct = T) + theme(axis.text.y = element_text(size=13)) 
missingness <- rbind(missingNess_overall$data %>% dplyr::mutate(missingness = 'overall') ,
                     missingNess_MS$data %>% dplyr::mutate(missingness = 'MS'),
                     missingNess_MS_referred_cohort$data %>% dplyr::mutate(missingness = 'MS(referred)'),
                     missingNess_control$data %>% dplyr::mutate(missingness = 'controls'))

missingness <- missingness %>% 
  mutate(missingness = factor(missingness, levels=c('overall', 'MS', 'MS(referred)', 'controls' )),
         variable = factor(variable, levels=rev(missingNess_overall$data$variable)))
p <- ggplot(data=missingness, aes(x=variable, y=pct_miss)) + geom_bar(stat = "identity", position = "dodge", width = 0.7) 
p <- p + geom_vline(xintercept=50, linetype="dashed", color = "red") + ylab('percent')
p <- p + coord_flip() + facet_grid(. ~ missingness) + theme_bw() + theme(text = element_text(size=20))
p
ggsave("analysis/engagement_Paper_1/FINAL_FIGS/supplementary_data_missingness.png", plot=p,
       width = 15, height = 10, dpi = 300, units="in")



#### % Data contributed by MS patients
prop.table(table(userActivity$dataGroups))






### Pariticipant active for how many days?
userActiveDays= userActivity %>% group_by(healthCode, dataGroups) %>% 
  summarise(numDays = n_distinct(participant_day)) %>% 
  as.data.frame()

View(userActiveDays)

p <- ggplot(data=userActiveDays, aes(x = dataGroups, y = numDays,
                                     fill = dataGroups))
p <- p + geom_boxplot() + theme_few() + scale_fill_manual(values=c(COL_CONTROL, COL_MS_PATIENT)) +  theme(text = element_text(size=12))
p <- p + theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.title = element_blank())
p <- p + ylab('#Days') + xlab('')
p
ggsave("~/dev/elevateMS_analysis/analysis/engagement_Paper_1/FINAL_FIGS/daysActive.jpeg", plot=p, height=3, width=4, units="in", dpi=150)
ggsave("~/dev/elevateMS_analysis/analysis/engagement_Paper_1/FINAL_FIGS/daysActive.tiff", plot=p, height=3, width=4, units="in", dpi=150)


#users who were active only for first 2 days
users_active_2days_only <- userActiveDays %>% filter(numDays <= 2) %>% .$healthCode
baselineChar %>% filter(healthCode %in% users_active_2days_only) %>% .$dataGroups %>% table()

#users who were active > 1 days
users_active_gt_2day <- userActiveDays %>% filter(numDays > 2) %>% .$healthCode
totalN_selectusers <- baselineChar %>% filter(healthCode %in% users_active_gt_2day) %>% .$dataGroups %>% table()
totalN_selectusers <- data.frame(totalN = as.numeric(totalN_selectusers),dataGroups = names(totalN_selectusers))



#sumamry stats on active days / user
userActiveDays %>% group_by(dataGroupsMod) %>% dplyr::summarise(meanDays = mean(numDays, na.rm=T),
                                                                sdDays = sd(numDays, na.rm=T))
# independent 2-group Mann-Whitney U Test 
wilcox.test(numDays ~ dataGroupsMod, data=userActiveDays)

cumsum(rev(table(userActiveDays$numDays)))



