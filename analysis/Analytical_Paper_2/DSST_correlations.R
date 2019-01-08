rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')
install_load("ComplexHeatmap", "circlize", "RColorBrewer")
synapser::synLogin()
source("analysis/loadData.R")
source("analysis/Analytical_Paper_2/common_analysis_functions.R")


summary(dsst$percentCorrect)
### somewhat arbritrary cut-off
dsst_flt <- dsst %>% filter(percentCorrect >= 80)
dsst_flt <- dsst_flt %>% inner_join(baselineChar %>% select(healthCode, dataGroups)) %>%
  filter(dataGroups == 'ms_patient') %>%
  gather(feature, value, c(3,5:9))

dsst_flt_week_avg =  dsst_flt %>% select(healthCode, participant_week, feature, value) %>%
  group_by(healthCode, participant_week, feature) %>% 
  dplyr::summarise_all(.funs=c(mean), na.rm=T) 

## Correlations - All Weeks
cor_dsst_nQOL_cog <- tmp_calc_cor(dsst_flt_week_avg, nQOL_cognition_week_avg, use_first_common_week = F) %>% 
  mutate(survey = 'neuroQOL Cognition')

cor_dsst_nQOL_uppExt <- tmp_calc_cor(dsst_flt_week_avg, nQOL_uppExtremity_week_avg, use_first_common_week = F) %>%
  mutate(survey = 'neuroQOL Upper Extremity')

cor_dsst_nQOL_lowExt <- tmp_calc_cor(dsst_flt_week_avg, nQOL_lowExtremity_week_avg, use_first_common_week = F) %>%
  mutate(survey = 'neuroQOL Lower Extremity')

cor_dsst_allWeeks <- rbind(cor_dsst_nQOL_cog, cor_dsst_nQOL_uppExt, cor_dsst_nQOL_lowExt)
cor_dsst_allWeeks

library(grid)
p1 <- ggplot(cor_dsst_allWeeks, aes(x=cor, y=-log10(p.val.adj))) + theme_few() + geom_point(size=.7)  + facet_grid( . ~ survey )
p1 <- p1 + theme(text = element_text(size=15)) + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
p1
ggsave("analysis/Analytical_Paper_2/FIGURES/dsst_allWeeks_volcanoPlot.jpeg", plot=p1, height=4, width=10, units="in", dpi=300)
ggsave("analysis/Analytical_Paper_2/FIGURES/dsst_allWeeks_volcanoPlot.tiff", plot=p1, height=4, width=10, units="in", dpi=300)


### Select top features
View(cor_dsst_allWeeks %>% arrange(abs(p.val.adj)))

p1 <- get_scatter_plot(dsst_flt_week_avg, nQOL_cognition, x='avgTime', y='TScore')
p2 <- get_scatter_plot(dsst_flt_week_avg, nQOL_cognition, x='numCorrect' ,y ='TScore')
p3 <- grid.arrange(p1,p2, nrow=1)
p3
ggsave("analysis/Analytical_Paper_2/FIGURES/dsst_n_NeuroQOLs_top_2_features_scatterPlot_allWeeks.png", p3, height=3, width=8, units="in", dpi=300)



##### PDDS
tmp_baselineChar <- baselineChar %>% 
  filter(!is.na(overallPhysicalAbility)) %>%
  filter(dataGroups == 'ms_patient') %>%
  select(healthCode, overallPhysicalAbility)

first_dsst_week <- dsst_flt_week_avg %>%
  dplyr::group_by(healthCode) %>%
  dplyr::summarise(participant_week = min(participant_week))

dsst_flt_averaged_firstWeek <- dsst_flt_week_avg %>% inner_join(first_dsst_week) %>%
  inner_join(tmp_baselineChar) %>%
  mutate(overallPhysicalAbility = factor(overallPhysicalAbility, levels=c('normal', 'milddisability', 'moderatedisability', 'gaitdisability')))

tmp_df <- dsst_flt_averaged_firstWeek  %>%
  group_by(feature) %>%
  nest() %>%
  mutate(res = purrr::map(data, function(x){
    anova(lm(value ~ overallPhysicalAbility, data=x))
  })) %>%
  mutate(glance = res %>% map(broom::tidy))
tmp_get_anova_pval <- function(x){
  x %>% filter(term == 'overallPhysicalAbility') %>% .$p.value
}
tmp_df <- tmp_df %>% mutate(p.val = unlist(glance  %>% map( tmp_get_anova_pval ))) %>%
  select(-glance, -data, -res)

ggplot(data = dsst_flt_averaged_firstWeek %>% filter(feature == 'avgTime'),
       aes(x=overallPhysicalAbility, y=value)) + geom_boxplot(width=0.7) + theme_bw()


p <- ggboxplot(dsst_flt_averaged_firstWeek %>% filter(feature == 'avgTime'), 
               x = "overallPhysicalAbility", y = "value", 
               font.label = list(size=6, face="plain"),
               ylab='avgTime', width=.5, size=.3,
               color = "#F25F5C")
my_comparisons <- list( c("normal", "moderatedisability"), c("normal", "gaitdisability"))
p <- p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0, label.x = 1.2, label.size = 0.25) 
p <- p + theme_few()
p
ggsave("analysis/Analytical_Paper_2/FIGURES/dsst_FirstWeek_with_PDDS_boxplot.png", p, height=4, width=8, units="in", dpi=300)


