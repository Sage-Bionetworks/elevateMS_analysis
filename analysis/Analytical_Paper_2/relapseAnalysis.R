rm(list=ls())
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')

###Load data freeze
load(synGet("syn19273733")$path)
ls()

relapses <- relapses %>% 
  dplyr::mutate(relapse.date = as.Date(activity_start_timestamp_local))

#How many controls report relapse
nrow(relapses %>% filter(dataGroups == 'control'))

## Cases reporting relapse
relapses.flt <- relapses %>% filter(dataGroups == 'ms_patient')
n_distinct(relapses.flt$healthCode)

mean(table(relapses.flt$healthCode))
sd(table(relapses.flt$healthCode))
median(table(relapses.flt$healthCode))
n_distinct(relapses.flt$healthCode)


p <- ggplot(data=relapses.flt, aes(y=healthCode, x=participant_day)) + geom_point()
p <- p + theme_light(base_size = 12)
p
ggsave(file="analysis/Analytical_Paper_2/FIGURES/MS_healthCodes_relapses.png", plot=p, width = 10, height = 8)


### Avg number of activities before and after all relapses
tmp_calcReplaseSummary <- function(userHealthCode, relapseDate){
  df <- userActivity %>% filter(healthCode == userHealthCode)
  before = df %>% filter(createdOn_localTime < relapseDate) %>% mutate(status = 'before')
  after = df %>% filter(createdOn_localTime > relapseDate) %>% mutate(status = 'after')
  res <- rbind.fill(before %>% select(status, originalTable), 
        after %>% select(status, originalTable)) %>%
    count(status, originalTable)
}
  
relapseSummary <- relapses.flt %>% 
  dplyr::group_by(healthCode, relapse.date) %>%
  do(relapseSummary = tmp_calcReplaseSummary(.$healthCode, .$relapse.date))
relapseSummary <- relapseSummary %>% unnest() %>%
  mutate(originalTable = gsub('-v.*', '', originalTable, perl = T))
p2 <- ggplot(data=relapseSummary, aes(x=status, y=log2(n)))  + geom_boxplot() + facet_wrap(. ~ originalTable)
p2 <- p2 + theme_minimal(base_size = 15)
p2
ggsave(file="analysis/Analytical_Paper_2/FIGURES/MS_healthCodes_relapses_activities_before_N_after.png", plot=p2, width = 10, height = 8)



