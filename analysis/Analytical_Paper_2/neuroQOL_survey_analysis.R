rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c( 'tidyverse', 'synapser', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra')
install_load('tm', 'SnowballC', 'wordcloud', 'RColorBrewer')
install_load("scales", "ggsci")
synapser::synLogin()
source("analysis/loadData.R")


colnames(baselineChar)
nQOLs <- plyr::rbind.fill(nQOL_cognition %>% select(healthCode, TScore) %>% mutate(survey = 'nQOL Cognition'),
           nQOL_uppExtremity %>% select(healthCode, TScore) %>% mutate(survey = 'nQOL Upper Extremity'),
           nQOL_lowExtremity %>% select(healthCode, TScore) %>% mutate(survey = 'nQOL Lower Extremity'))
nQOLs <- nQOLs  %>% inner_join(baselineChar %>% select(healthCode, dataGroups, referred_by_clinician)) %>%
  filter(dataGroups == 'ms_patient')
  
# p1 <- ggplot(data=nQOLs, aes(x=TScore, fill=dataGroups)) + geom_density(alpha=.8) + theme_bw() + facet_grid( . ~ survey)
# p1 <- p1 + scale_fill_manual(values=c('#ff7f00'))
# p1
# ggsave('analysis/Analytical_Paper_2/FIGURES/nQOLs_distribution.png', plot=p1, width=10, height = 4, units="in", dpi = 150)
# 
# p2 <- ggplot(data=nQOLs, aes(x=TScore, fill=referred_by_clinician)) + geom_density(alpha=.8) + theme_bw() + facet_grid( . ~ survey)
# p2 <- p2 + scale_fill_manual(values=c('#fdae61', '#0571b0'))
# ggsave('analysis/Analytical_Paper_2/FIGURES/nQOLs_byClinicalReferral_distribution.png', plot=p2, width=10, height = 4, units="in", dpi = 150)



p1 <- ggplot(data=nQOLs, aes(x=TScore, fill=dataGroups)) + geom_histogram() + theme_bw() + facet_grid( . ~ survey)
p1 <- p1 + scale_fill_manual(values=c('#ff7f00'))
p1
ggsave('analysis/Analytical_Paper_2/FIGURES/nQOLs_distribution.png', plot=p1, width=10, height = 4, units="in", dpi = 150)

p2 <- ggplot(data=nQOLs, aes(x=TScore, fill=referred_by_clinician)) + geom_histogram(position="dodge") + theme_bw() + facet_grid( . ~ survey)
p2 <- p2 + scale_fill_manual(values=c('#fdae61', '#0571b0'))
p2
ggsave('analysis/Analytical_Paper_2/FIGURES/nQOLs_byClinicalReferral_distribution.png', plot=p2, width=10, height = 4, units="in", dpi = 150)
