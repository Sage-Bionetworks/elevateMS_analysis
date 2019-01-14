rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap", version = "3.8")
install_load("ComplexHeatmap", "circlize", "RColorBrewer")

synapser::synLogin()
source("analysis/loadData.R")
source("analysis/Analytical_Paper_2/common_analysis_functions.R")

tmp_tapF <- tapF %>% filter(dataGroups == 'ms_patient') %>%
  gather(feature, value, c(18:59,70)) %>%
  select(-c(1:6)) 

tapF_week_averaged =  tmp_tapF %>% select(healthCode, participant_week, hand, feature, value) %>%
  group_by(healthCode, participant_week, hand, feature) %>% 
  dplyr::summarise_all(.funs=c(mean), na.rm=T) 


## Correlations- All Weeks
neurCog_allWeeks_tapF <- get_neuroQOL_cors(tapF_week_averaged, additional_group_by='hand', use_first_common_week=F)

## Correlations- First common week
neurCog_firstCommonWeek_tapF <- get_neuroQOL_cors(tapF_week_averaged, additional_group_by='hand', use_first_common_week=T)


library(grid)
p1 <- ggplot(neurCog_allWeeks_tapF, aes(x=cor, y=-log10(p.val.adj), color=hand)) + theme_few() + geom_point(size=.7)  + facet_grid( . ~ survey )
p1 <- p1 + scale_color_manual(values=c("#d01c8b", "#4dac26")) + theme(text = element_text(size=15)) + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
p1
ggsave("analysis/Analytical_Paper_2/FIGURES/tapCorrelation_allWeeks_volcanoPlot.jpeg", plot=p1, height=4, width=10, units="in", dpi=300)
ggsave("analysis/Analytical_Paper_2/FIGURES/tapCorrelation_allWeeks_volcanoPlot.tiff", plot=p1, height=4, width=10, units="in", dpi=300)


p2 <- ggplot(neurCog_firstCommonWeek_tapF, aes(x=cor, y=-log10(p.val.adj), color=hand)) + theme_few() + geom_point(size=.7)  + facet_grid( . ~ survey )
p2 <- p2 + scale_color_manual(values=c("#d01c8b", "#4dac26")) + theme(text = element_text(size=15)) 
p2 <- p2 + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
p2
ggsave("analysis/Analytical_Paper_2/FIGURES/tap_N_NeuroQOLs_Correlation_firstCommonWeek_volcanoPlot.jpeg", plot=p2, height=4, width=10, units="in", dpi=300)
ggsave("analysis/Analytical_Paper_2/FIGURES/tap_N_NeuroQOLs_Correlation_firstCommonWeekvolcanoPlot.tiff", plot=p2, height=4, width=10, units="in", dpi=300)

p1 <- get_scatter_plot(tapF_week_averaged %>% filter(hand == 'left'), nQOL_uppExtremity_week_avg, 
                       x='numberTaps', y='TScore', use_first_common_week = F)
p2 <- get_scatter_plot(tapF_week_averaged %>% filter(hand == 'left'), nQOL_uppExtremity_week_avg, 
                       x='meanTapInter', y='TScore', use_first_common_week = F)
p3 <- grid.arrange(p2,p1, nrow=1)
ggsave("analysis/Analytical_Paper_2/FIGURES/tapping_n_NeuroQOLs_top_2_features_scatterPlot_allWeeks.png", p3, height=3, width=9, units="in", dpi=150)


p1 <- get_scatter_plot(tapF_week_averaged %>% filter(hand == 'left'), nQOL_uppExtremity_week_avg, x='numberTaps', y='TScore',
                       use_first_common_week = T)
p2 <- get_scatter_plot(tapF_week_averaged %>% filter(hand == 'left'), nQOL_uppExtremity_week_avg, x='meanTapInter', y='TScore',
                       use_first_common_week = T)
p3 <- grid.arrange(p2,p1, nrow=1)
ggsave("analysis/Analytical_Paper_2/FIGURES/tapping_n_NeuroQOLs_top_2_features_scatterPlot_firstCommonWeek.png", p3, height=3, width=9, units="in", dpi=150)



##############
##### Correlation with PDDS
##############
tmp_baselineChar <- baselineChar %>% 
  filter(!is.na(overallPhysicalAbility)) %>%
  filter(dataGroups == 'ms_patient') %>%
  select(healthCode, overallPhysicalAbility) %>%
  mutate(overallPhysicalAbility = factor(overallPhysicalAbility, levels=c('normal', 'milddisability', 'moderatedisability', 'gaitdisability')))

firstTapWeek <- tapF_week_averaged %>% 
  dplyr::group_by(healthCode) %>%
  dplyr::summarise(participant_week = min(participant_week))

tapF_averaged_firstWeek <- tapF_week_averaged %>% inner_join(firstTapWeek) %>%
  inner_join(tmp_baselineChar)

tmp_df_firstWeek <- tapF_averaged_firstWeek  %>%
  group_by(feature, hand) %>%
  nest() %>%
  mutate(res = purrr::map(data, function(x){
    anova(lm(value ~ overallPhysicalAbility, data=x))
  })) %>%
  mutate(glance = res %>% map(broom::tidy))

tmp_get_anova_pval <- function(x){
  x %>% filter(term == 'overallPhysicalAbility') %>% .$p.value
}
tmp_df_firstWeek <- tmp_df_firstWeek %>% 
  mutate(p.val = unlist(glance  %>% map( tmp_get_anova_pval ))) %>%
  select(-glance, -data, -res)
ggplot(data = tapF_averaged_firstWeek %>% filter(feature == 'numberTaps'),
       aes(x=overallPhysicalAbility, y=value, fill = hand)) + geom_boxplot(width=0.7) + theme_bw()

p <- ggboxplot(tapF_averaged_firstWeek %>% filter(feature == 'numberTaps'), 
               x = "overallPhysicalAbility", y = "value", 
               font.label = list(size=6, face="plain"),
               legend.title="hand", ylab='number of taps', width=.5, size=.3,
               color = "hand", palette = c('#247BA0', '#F25F5C'))
my_comparisons <- list( c("normal", "milddisability"), c("normal", "moderatedisability"))
p <- p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0, label.x = 1.2, label.size = 0.25) 
p <- p + theme_few()
p
ggsave("analysis/Analytical_Paper_2/FIGURES/FirstWeek_numberTaps_with_physicalDisability_boxplot.png", p, height=4, width=8, units="in", dpi=300)




# tmp_df_allWeeks <- tapF_averaged_allWeeks  %>%
#   group_by(feature, hand) %>%
#   nest() %>%
#   mutate(res = purrr::map(data, function(x){
#     anova(lm(value ~ overallPhysicalAbility, data=x))
#   })) %>%
#   mutate(glance = res %>% map(broom::tidy))
# tmp_get_anova_pval <- function(x){
#   x %>% filter(term == 'overallPhysicalAbility') %>% .$p.value
# }
# 
# tmp_df_allWeeks <- tmp_df_allWeeks %>% 
#   mutate(p.val = unlist(glance  %>% map( tmp_get_anova_pval ))) %>%
#   select(-glance, -data, -res)
# View(tmp_df_allWeeks)
# ggplot(data = tapF_averaged_allWeeks %>% filter(feature == 'numberTaps'),
#        aes(x=overallPhysicalAbility, y=value, fill = hand)) + geom_boxplot(width=0.7) + theme_bw()
# 
# p <- ggboxplot(tapF_averaged_allWeeks %>% filter(feature == 'numberTaps'), 
#                x = "overallPhysicalAbility", y = "value", 
#                font.label = list(size=6, face="plain"),
#                legend.title="hand", ylab='number of taps', width=.5, size=.3,
#                color = "hand", palette = c('#247BA0', '#F25F5C'))
# my_comparisons <- list( c("normal", "milddisability"), c("normal", "moderatedisability"))
# p <- p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
#   stat_compare_means(label.y = 0, label.x = 1.2, label.size = 0.25) 
# p <- p + theme_few()
# p
# ggsave("analysis/Analytical_Paper_2/FIGURES/numberTaps_physicalDisability_boxplot.png", p, height=4, width=8, units="in", dpi=300)
# 
# 
# 
