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

## LOAD DATA 
load(synGet("syn19273733")$path)


userActivity <- userActivity %>% 
  filter(dataGroups %in% c('control', 'ms_patient')) %>%
  filter(!originalTable %in% c('Passive Data-v3', 'Weather-v3')) 


lastDayinStudy <- userRetention <- userActivity %>%
  dplyr::group_by(healthCode) %>%
  dplyr::summarise(lastDayinStudy = max(participant_day))

userRetention <- userActivity %>%
  dplyr::filter(participant_day <=  84) %>%
  dplyr::group_by(healthCode) %>%
  dplyr::summarise(totalDaysActive = n_distinct(lubridate::date(createdOn)),
                   duration_in_study = as.numeric(max(lubridate::date(createdOn)) - min(lubridate::date(createdOn)) + 1),
                   regularity = round((totalDaysActive/84) * 100, digits=2))

# merge with max last day in study
userRetention <- userRetention %>% inner_join(lastDayinStudy)


#merge metadata
userRetention <- userRetention %>% 
  inner_join(baselineChar %>% 
               select(healthCode, dataGroups, referred_by_clinician, health_insurance, education,
                      overallPhysicalAbility, employment, currentDMT, age, gender, race)) %>%
  mutate(referred_by_clinician = factor(referred_by_clinician, levels=c(FALSE,TRUE)),
         overallPhysicalAbility = factor(overallPhysicalAbility, levels=c('normal', 'milddisability', 'moderatedisability', 'gaitdisability')),
         employment = factor(employment, levels=c('full-time', 'disabled', 'part-time', 'retired', 'unemployed')),
         currentDMT = factor(currentDMT), 
         age_group = cut(age, breaks=c(18,30,40,50,60,120)),
         gender = factor(gender),
         raceMod = ifelse(race == 'caucasian', 'caucasian', 'others')
  ) 

userRetention <- userRetention %>% mutate(group = case_when(
  referred_by_clinician == T & dataGroups == 'ms_patient' ~ 'MS patients(clinical referral)',
  referred_by_clinician == F & dataGroups == 'ms_patient' ~ 'MS patients',
  dataGroups == 'control' ~ 'Controls'
))


#### Time to Event analysis
install_load("survival", "survminer")
### Survival difference between controls and clinical cohort 
censor <- rep(1, nrow(userRetention))
fit <- survfit(Surv(time=duration_in_study, event=censor) ~ group, data = userRetention )
summary(fit)$table
summary(fit)
p1 <- ggsurvplot(fit, pval = TRUE, conf.int = TRUE, 
                 xlab = "Days in study ", 
                 palette = c(COL_CONTROL, COL_MS_PATIENT, COL_MS_PATIENT_CLINICAL_REF),
                 risk.table.y.text.col = F, risk.table.y.text = F, legend = "top",
                 xlim = c(1,81),
                 surv.median.line = "hv", ggtheme = theme_light(base_size = 12))

p1 
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/survivalCurve_by_caseControl.png", plot=print(p1), height = 6, width = 7, units="in", dpi=300)
ggsave("analysis/Analytical_Paper_2/Figs_N_Tables/survivalCurve_by_caseControl.tiff", plot=print(p1), height = 6, width = 7, units="in", dpi=300)


# EDA
# ggplot(data=elevateMS_compliance, aes(x=totalDaysActive, fill=referred_by_clinician)) + geom_density(alpha=.6) + theme_bw()
# ggplot(data=elevateMS_compliance, aes(x=duration_in_study, fill=referred_by_clinician)) + geom_density(alpha=.6) + theme_bw()
# ggplot(data=elevateMS_compliance, aes(x=duration_in_study, fill=overallPhysicalAbility)) + geom_density(alpha=.6) + theme_bw()

head(userRetention)

censor <- rep(1, nrow(userRetention))
res.cox <- coxph(formula = Surv(duration_in_study, censor) ~ group, data=userRetention)
summary(res.cox)
ggforest(res.cox)
cox.zph(res.cox)


# res.cox <- coxph(formula = Surv(duration_in_study, censor) ~ group + gender + age , 
#                  data=userRetention %>% filter(dataGroups == 'ms_patient'))
# summary(res.cox)
# ggforest(res.cox)
# cox.zph(res.cox)



### MS patients only - demographic vars - cox-PH models
run_coxPH_model <- function(df, covariate){
  df <- df %>% as.data.frame()
  censor <- rep(1, nrow(df))
  covars <- paste('~ ', paste(covariate, collapse='  + ') )
  formula <- paste('Surv(time=duration_in_study, event=censor)', covars )
  coxph(as.formula(formula), data = df)
}

featureWise_survival  <- userRetention %>% 
  dplyr::filter(dataGroups == 'ms_patient') %>%
  dplyr::select(duration_in_study, group, overallPhysicalAbility, 
                employment,education, health_insurance, currentDMT, 
                age_group, gender, race, raceMod) %>%
  dplyr::mutate(overallPhysicalAbility = as.character(overallPhysicalAbility),
                employment = as.character(employment),
                education = as.character(education),
                health_insurance = as.character(health_insurance),
                currentDMT = as.character(currentDMT),
                age_group = as.character(age_group),
                gender = as.character(gender),
                race = as.character(race),
                raceMod = as.character(raceMod)) %>%
  tidyr::gather(feature, value, 3:11, na.rm = F) %>%
  group_by(feature) %>%
  tidyr::nest()

#Run CoxPH models
featureWise_survival <- featureWise_survival %>%
  mutate(coxPH_model = map(data, run_coxPH_model, c('group', 'value')),
         coxPH_model_test = map(coxPH_model, cox.zph),
         anova_coxPH_model = map(coxPH_model, anova), 
         )


#Result of indidivual cox models
names(featureWise_survival$coxPH_model) <- featureWise_survival$feature
featureWise_survival$coxPH_model

## Testing the assumption of CoxPH model using cox.zph test
names(featureWise_survival$coxPH_model_test) <- featureWise_survival$feature
featureWise_survival$coxPH_model_test


### Anova on top of CoxPH model
names(featureWise_survival$anova_coxPH_model) <- featureWise_survival$feature
featureWise_survival$anova_coxPH_model


### Tidy form output of CoxPH model 
x <- featureWise_survival %>% 
  dplyr::select(-data, -coxPH_model_test, -anova_coxPH_model) %>% 
  dplyr::mutate(temp = map(coxPH_model, broom::tidy)) %>% 
  unnest(temp) %>%
  select(-coxPH_model)

View(x)






#######
#OLD CODE FOLLOWS
######


##Overall Survival
# ggsurvplot(survfit(res.cox, data=elevateMS_compliance), color = "#2E9FDF",
#            surv.median.line = 'hv',
#            ggtheme = theme_minimal())

# ### Comparing Survival diff between Clinical Referral
# comparator_groups <- with(elevateMS_compliance, data.frame(referred_by_clinician=levels(referred_by_clinician)))
# row.names(comparator_groups) <- letters[1:nrow(comparator_groups)]
# fit <- survfit(res.cox, newdata = comparator_groups, data=elevateMS_compliance)

# Option 1 
# the following works too but the output graph is not clean
# ggsurvplot(fit, surv.median.line = "hv", conf.int = F)


#Option 2 
# Creating a data frame of fit values, merge it with meta data and plot using ggsurvplot_df
# fit_summary <- surv_summary(fit)
# comparator_groups <- comparator_groups %>% mutate(strata = rownames(comparator_groups)) 
# fit_summary <- merge(fit_summary, comparator_groups)
# p1 <- ggsurvplot_df(fit_summary, color='referred_by_clinician', censor=F, palette=c('#6baed6', '#08519c'),
#               surv.median.line = "hv", legend.title = "Clinical Referral")
# p1
# ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/survivalCurve_clinicalReferral.png", plot=p1,
#        width=7, height = 5, units="in", dpi=150)


#### Other meta-data  - overallPhysicalAbility / PDDS
# res.cox <- coxph(formula = Surv(duration_in_study, censor) ~ referred_by_clinician + overallPhysicalAbility  , data=elevateMS_compliance)
# summary(res.cox)
# p2 <- ggforest(res.cox)
# p2
# ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/hazardRatios_PDDS.png", plot=p2,
#        width=8, height = 6, units="in", dpi=150)


#### Other meta-data  - age group
# res.cox <- coxph(formula = Surv(duration_in_study, censor) ~  referred_by_clinician + age_group , data=elevateMS_compliance)
# summary(res.cox)
# p3 <- ggforest(res.cox)
# p3
# ggsave(file="analysis/engagement_Paper_1/FINAL_FIGS/hazardRatios_ageGroups.png", plot=p3,
#        width=8, height = 6, units="in", dpi=150)


# colnames(baselineChar)
# table(baselineChar$)
# res.cox <- coxph(formula = Surv(duration_in_study, censor) ~  referred_by_clinician + raceMod , data=elevateMS_compliance)
# summary(res.cox)
