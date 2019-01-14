rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')
install_load("ComplexHeatmap", "circlize", "RColorBrewer", "grid")

synapser::synLogin()
source("analysis/loadData.R")
source("analysis/Analytical_Paper_2/common_analysis_functions.R")

walkF <- walkF %>% filter(dataGroups == 'ms_patient') %>%
  gather(feature, value, c(14:126,136)) 
walkF <- walkF %>% select(-c(9:15)) 

walkF_week_averaged =  walkF %>% select(healthCode, participant_week, feature, value) %>%
  group_by(healthCode, participant_week, feature) %>% 
  dplyr::summarise_all(.funs=c(mean), na.rm=T) 

## Correlations- All Weeks
neurCog_allWeeks_walkF <- get_neuroQOL_cors(walkF_week_averaged)

p1 <- ggplot(neurCog_allWeeks_walkF, aes(x=cor, y=-log10(p.val.adj))) + theme_few() + geom_point(size=.7)  + facet_grid( . ~ survey )
p1 <- p1 + scale_color_manual(values=c("#d01c8b", "#4dac26")) + theme(text = element_text(size=15)) + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
ggsave("analysis/Analytical_Paper_2/FIGURES/walkF_n_NeuroQOLs_corr_allWeeks_volcanoPlot.jpeg", plot=p1, height=4, width=10, units="in", dpi=300)

### Select top features
View(neurCog_allWeeks_walkF %>% arrange(desc(abs(cor))))
p1 <- get_scatter_plot(walkF_week_averaged, nQOL_lowExtremity, x='P0FY', y='TScore')
p2 <- get_scatter_plot(walkF_week_averaged, nQOL_lowExtremity, x='q1Z', y='TScore')
p3 <- grid.arrange(p1,p2, nrow=1)
ggsave("analysis/Analytical_Paper_2/FIGURES/walkF_top_2_feature_scatterPlot_allWeek.png", p3, height=3, width=9, units="in", dpi=150)


## Correlations- First common week
neurCog_firstCommonWeek_walkF <- get_neuroQOL_cors(walkF_week_averaged, use_first_common_week=T)
p1 <- ggplot(neurCog_firstCommonWeek_walkF, aes(x=cor, y=-log10(p.val.adj))) + theme_few() + geom_point(size=.7)  + facet_grid( . ~ survey )
p1 <- p1 + scale_color_manual(values=c("#d01c8b", "#4dac26")) + theme(text = element_text(size=15)) + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
ggsave("analysis/Analytical_Paper_2/FIGURES/walkF_n_NeuroQOLs_corr_firstCommonWeeks_volcanoPlot.jpeg", plot=p1, height=4, width=10, units="in", dpi=300)

### Select top features
View(neurCog_firstCommonWeek_walkF %>% arrange(desc(abs(cor))))
p1 <- get_scatter_plot(walkF_week_averaged, nQOL_lowExtremity, x='F0AA', y='TScore', use_first_common_week = T)
p2 <- get_scatter_plot(walkF_week_averaged, nQOL_lowExtremity, x='tlagAA', y='TScore', use_first_common_week = T)
p3 <- grid.arrange(p1,p2, nrow=1)
ggsave("analysis/Analytical_Paper_2/FIGURES/walkF_top_2_feature_scatterPlot_firstCommonWeek.png", p3, height=3, width=9, units="in", dpi=150)




##### PDDS
tmp_baselineChar <- baselineChar %>% 
  filter(!is.na(overallPhysicalAbility)) %>%
  filter(dataGroups == 'ms_patient') %>%
  select(healthCode, overallPhysicalAbility)

walkF_averaged <- walkF %>% group_by(healthCode, feature) %>% 
  dplyr::summarise(value = mean(value, na.rm = T)) %>%
  inner_join(tmp_baselineChar) %>%
  mutate(overallPhysicalAbility = factor(overallPhysicalAbility, levels=c('normal', 'milddisability', 'moderatedisability', 'gaitdisability')))

tmp_df <- walkF_averaged  %>%
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
ggplot(data = walkF_averaged %>% filter(feature == 'P0FY'),
       aes(x=overallPhysicalAbility, y=value)) + geom_boxplot(width=0.7) + theme_bw()


p <- ggboxplot(walkF_averaged %>% filter(feature == 'P0FY'), 
               x = "overallPhysicalAbility", y = "value", 
               font.label = list(size=6, face="plain"),
               ylab='Walking P0FY', width=.5, size=.3,
               color = "#F25F5C")
my_comparisons <- list( c("normal", "moderatedisability"), c("normal", "gaitdisability"))
p <- p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0, label.x = 1.2, label.size = 0.25) 
p <- p + theme_few()
p
ggsave("analysis/Analytical_Paper_2/FIGURES/Walk_feature_P0FY_physicalDisability_boxplot.png", p, height=4, width=8, units="in", dpi=300)


