rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')
install_load("ComplexHeatmap", "circlize", "RColorBrewer", "grid")

synapser::synLogin()
source("analysis/loadData.R")
source("analysis/Analytical_Paper_2/common_analysis_functions.R")

restF <- restF %>% filter(dataGroups == 'ms_patient') %>%
  gather(feature, value, c(12:30,40)) 

restF_week_averaged =  restF %>% select(healthCode, participant_week, feature, value) %>%
  group_by(healthCode, participant_week, feature) %>% 
  dplyr::summarise_all(.funs=c(mean), na.rm=T) 

restF_averaged =  restF %>% select(healthCode, feature, value) %>%
  group_by(healthCode, feature) %>% dplyr::summarise_all(.funs=c(mean), na.rm=T) 

## Correlations- All Weeks
neurCog_allWeeks_restF <- get_neuroQOL_cors(restF_week_averaged)
p1 <- ggplot(neurCog_allWeeks_restF, aes(x=cor, y=-log10(p.val.adj))) + theme_few() + geom_point(size=.7)  + facet_grid( . ~ survey )
p1 <- p1 + scale_color_manual(values=c("#d01c8b", "#4dac26")) + theme(text = element_text(size=15)) + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
p1
ggsave("analysis/Analytical_Paper_2/FIGURES/restF_n_NeuroQOLs_corr_allWeeks_volcanoPlot.jpeg", plot=p1, height=4, width=10, units="in", dpi=300)


### Select top features
View(neurCog_allWeeks_restF %>% arrange(desc(abs(cor))))
p1 <- get_scatter_plot(restF_week_averaged, nQOL_uppExtremity_week_avg, x='postpeak', y='TScore')
p2 <- get_scatter_plot(restF_week_averaged, nQOL_uppExtremity_week_avg, x='q3AA', y='TScore')
p3 <- grid.arrange(p1,p2, nrow=1)
ggsave("analysis/Analytical_Paper_2/FIGURES/restF_top_2_feature_scatterPlot_allWeek.png", p3, height=3, width=9, units="in", dpi=150)


#################
## Correlations- First common week
#################
neurCog_firstCommonWeek_restF <- get_neuroQOL_cors(restF_week_averaged, use_first_common_week=T)
p1 <- ggplot(neurCog_firstCommonWeek_restF, aes(x=cor, y=-log10(p.val.adj))) + theme_few() + geom_point(size=.7)  + facet_grid( . ~ survey )
p1 <- p1 + scale_color_manual(values=c("#d01c8b", "#4dac26")) + theme(text = element_text(size=15)) + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
p1
ggsave("analysis/Analytical_Paper_2/FIGURES/restF_n_NeuroQOLs_corr_firstCommonWeeks_volcanoPlot.jpeg", plot=p1, height=4, width=10, units="in", dpi=300)

### Select top features
View(neurCog_firstCommonWeek_restF %>% arrange(desc(abs(cor))))
p1 <- get_scatter_plot(restF_week_averaged, nQOL_uppExtremity_week_avg, x='postpeak', y='TScore', use_first_common_week = T)
p2 <- get_scatter_plot(restF_week_averaged, nQOL_uppExtremity_week_avg, x='iqrAA', y='TScore', use_first_common_week = T)
p3 <- grid.arrange(p1,p2, nrow=1)
ggsave("analysis/Analytical_Paper_2/FIGURES/restF_top_2_feature_scatterPlot_firstCommonWeek.png", p3, height=3, width=9, units="in", dpi=150)



############
##### PDDS
############
tmp_baselineChar <- baselineChar %>% 
  filter(!is.na(overallPhysicalAbility)) %>%
  filter(dataGroups == 'ms_patient') %>%
  select(healthCode, overallPhysicalAbility)

tmp_restF_averaged <- restF_averaged %>% 
  inner_join(tmp_baselineChar) %>%
  mutate(overallPhysicalAbility = factor(overallPhysicalAbility, levels=c('normal', 'milddisability', 'moderatedisability', 'gaitdisability')))

tmp_df <- tmp_restF_averaged  %>%
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

p <- ggboxplot(tmp_restF_averaged %>% filter(feature == 'turningTime'), 
               x = "overallPhysicalAbility", y = "value", 
               font.label = list(size=6, face="plain"),
               ylab='turning time', width=.5, size=.3,
               color = "#F25F5C")
my_comparisons <- list( c("normal", "moderatedisability"), c("normal", "gaitdisability"))
p <- p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0, label.x = 1.2, label.size = 0.25) 
p <- p + theme_few()
p
ggsave("analysis/Analytical_Paper_2/FIGURES/Rest_feature_turningTime_physicalDisability_boxplot.png", p, height=4, width=8, units="in", dpi=300)
