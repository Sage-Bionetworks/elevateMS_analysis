rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c( 'tidyverse', 'synapser', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra')
install_load('tm', 'SnowballC', 'wordcloud', 'RColorBrewer')
install_load("scales", "ggsci")
synapser::synLogin()
source("analysis/loadData.R")

wapi_flt <- wapi %>% filter(dataGroups == 'ms_patient') %>%
  inner_join(baselineChar)

p1 <- ggplot(data= wapi_flt, aes(x=percent_activity_impairment_due_to_problem, fill=referred_by_clinician)) + geom_density(alpha=0.7)
p1 + theme_bw()

wapi_flt$percent_overall_work_impairment_due_to_problem


p1 <- ggplot(data= wapi_flt, aes(x=percent_overall_work_impairment_due_to_problem, fill=referred_by_clinician)) + geom_density(alpha=0.7)
p1 + theme_bw()


