rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapser)
synapser::synLogin()
library(install.load)
# installed latest version
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/survminer", build_vignettes = FALSE)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools')


source("analysis/loadData.R")
#load(synGet("syn11657929")$path)


userActivity <- userActivity %>% 
  inner_join(baselineChar %>% select(healthCode, referred_by_clinician)) %>%
  filter(!originalTable %in% c('Passive Data-v3', 'Weather-v3')) 

elevateMS_compliance <- userActivity %>% filter(dataGroups == 'ms_patient') %>%
    filter(participant_day <=  84) %>%
    dplyr::group_by(healthCode) %>%
    dplyr::summarise(totalDaysActive = n_distinct(lubridate::date(createdOn)),
                     duration_in_study = as.numeric(max(lubridate::date(createdOn)) - min(lubridate::date(createdOn)) + 1),
                     regularity = round((totalDaysActive/84) * 100, digits=2)) 
    
elevateMS_compliance <- elevateMS_compliance %>% 
  inner_join(baselineChar %>% 
               select(healthCode, referred_by_clinician, overallPhysicalAbility, employment, currentDMT, age, gender, race)) %>%
  mutate(referred_by_clinician = factor(referred_by_clinician, levels=c(FALSE,TRUE)),
         overallPhysicalAbility = factor(overallPhysicalAbility, levels=c('normal', 'milddisability', 'moderatedisability', 'gaitdisability')),
         employment = factor(employment, levels=c('full-time', 'disabled', 'part-time', 'retired', 'unemployed')),
         currentDMT = factor(currentDMT), 
         age_group = cut(age, breaks=c(18,30,40,50,60,120)),
         gender = factor(gender),
         raceMod = ifelse(race == 'caucasian', 'caucasian', 'others')
         )


# EDA
# ggplot(data=elevateMS_compliance, aes(x=totalDaysActive, fill=referred_by_clinician)) + geom_density(alpha=.6) + theme_bw()
# ggplot(data=elevateMS_compliance, aes(x=duration_in_study, fill=referred_by_clinician)) + geom_density(alpha=.6) + theme_bw()
# ggplot(data=elevateMS_compliance, aes(x=duration_in_study, fill=overallPhysicalAbility)) + geom_density(alpha=.6) + theme_bw()

install_load(c("survival", "survminer"))
censor <- rep(1, nrow(elevateMS_compliance))
res.cox <- coxph(formula = Surv(duration_in_study, censor) ~ referred_by_clinician, data=elevateMS_compliance)
summary(res.cox)
ggforest(res.cox)

##Overall Survival
ggsurvplot(survfit(res.cox, data=elevateMS_compliance), color = "#2E9FDF",
           surv.median.line = 'hv',
           ggtheme = theme_minimal())

### Comparing Survival diff between Clinical Referral
comparator_groups <- with(elevateMS_compliance, data.frame(referred_by_clinician=levels(referred_by_clinician)))
row.names(comparator_groups) <- letters[1:nrow(comparator_groups)]
fit <- survfit(res.cox, newdata = comparator_groups, data=elevateMS_compliance)

# Option 1 
# the following works too but the output graph is not clean
ggsurvplot(fit, surv.median.line = "hv", conf.int = F)


#Option 2 
# Creating a data frame of fit values, merge it with meta data and plot using ggsurvplot_df
fit_summary <- surv_summary(fit)
comparator_groups <- comparator_groups %>% mutate(strata = rownames(comparator_groups)) 
fit_summary <- merge(fit_summary, comparator_groups)
p1 <- ggsurvplot_df(fit_summary, color='referred_by_clinician', censor=F, palette=c('#6baed6', '#08519c'),
              surv.median.line = "hv", legend.title = "Clinical Referral")
p1
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/survivalCurve_clinicalReferral.png", plot=p1,
       width=7, height = 5, units="in", dpi=150)




#### Other meta-data  - overallPhysicalAbility / PDDS
res.cox <- coxph(formula = Surv(duration_in_study, censor) ~ referred_by_clinician + overallPhysicalAbility  , data=elevateMS_compliance)
summary(res.cox)
p2 <- ggforest(res.cox)
p2
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/hazardRatios_PDDS.png", plot=p2,
       width=8, height = 6, units="in", dpi=150)


#### Other meta-data  - age group
res.cox <- coxph(formula = Surv(duration_in_study, censor) ~  referred_by_clinician + age_group , data=elevateMS_compliance)
summary(res.cox)
p3 <- ggforest(res.cox)
p3
ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/hazardRatios_ageGroups.png", plot=p3,
       width=8, height = 6, units="in", dpi=150)


# colnames(baselineChar)
# table(baselineChar$)
# res.cox <- coxph(formula = Surv(duration_in_study, censor) ~  referred_by_clinician + raceMod , data=elevateMS_compliance)
# summary(res.cox)
