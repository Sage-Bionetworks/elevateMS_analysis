rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools', 'DT', 'lattice')


FREEZED_DATA_LOC = "syn11657929"
load(synGet(FREEZED_DATA_LOC)@filePath)
ls()

tmp_mean <- function(x){
  mean(x, na.rm = T)
}

tapF <- tapF %>% select(-tapping_left.json.TappingSamples, -tapping_right.json.TappingSamples, 
                                 -recordId, -tapping_left.json.ButtonRectLeft, 
                                 -tapping_right.json.ButtonRectRight, -tapping_left.json.ButtonRectRight,
                                 -tapping_left.json.endDate, -timeZone, 
                                 -metadata.json.dataGroups, -metadata.json.endDate.timezone,
                                 -tapping_left.json.endDate.timezone, 
                                 -metadata.json.taskIdentifier, -createdOnTimeZone,
                                 -userSharingScope, -createdOn, -phoneInfo, -uploadDate, -externalId, 
                                 -validationErrors, -accelerometer_tapping_right.json.items,
                                 -accelerometer_tapping_left.json.items, -tapping_right.json.ButtonRectLeft,
                                 -tapping_right.json.endDate, -tapping_right.json.endDate.timezone,
                                 -tapping_right.json.startDate, -tapping_right.json.startDate.timezone,
                                 -tapping_left.json.startDate, -tapping_left.json.startDate.timezone,
                                 -tapping_right.json.TappingViewSize, -tapping_left.json.TappingViewSize,
                                 -metadata.json.scheduledActivityGuid, -idx, 
                                 -metadata.json.taskRunUUID, 
                                 -metadata.json.startDate.timezone, -metadata.json.startDate,
                                 -metadata.json.endDate, -tapping_right_json_file,
                                 -tapping_left_json_file)
tapF_week_averaged =  tapF %>% select( -activity_start_timestamp_GMT, -elevateMS_startDate_GMT,  -activity_start_timestamp_local, -study_week,
                                       -study_day, -error, -appVersion) %>% filter(dataGroups == 'ms_patient') %>%
  group_by(healthCode, participant_week, dataGroups, hand) %>% dplyr::summarise_all(.funs=c(tmp_mean)) %>% select(-participant_day)


get_tapF_nQOL_cors <- function(tapF, nQOL_survey){
  nQOL_survey['T_Score'] = nQOL_survey['T-Score']
  nQOL_survey['T-Score'] <- NULL
  nQOL_survey <- nQOL_survey %>% select(healthCode, T_Score, rawScore, participant_week)
  tmp_df <- merge(tapF_week_averaged, nQOL_survey)
  tmp_df <- tmp_df %>% tidyr::gather(feature, value, c(5:47) ) %>% 
    dplyr::mutate(value = as.numeric(value), T_Score = as.numeric(T_Score))
  cor_res <- tmp_df %>% dplyr::group_by(hand, feature) %>% 
    dplyr::summarise(n= length(value),
                     totalIndividuals = n_distinct(healthCode),
                     cor = cor(value, T_Score, use="complete.obs", method="spearman"),
                     p.val = cor.test(value, T_Score, use="complete.obs", method="spearman")[['p.value']])
  cor_res
}

cor_tapF_nQOL_cog <- get_tapF_nQOL_cors(tapF, nQOL_cognition) %>% group_by(hand) %>%
  dplyr::mutate(p.val.adj = p.adjust(p.val, method='fdr'), survey = 'nQOL Cognition')
cor_tapF_nQOL_uppExt <- get_tapF_nQOL_cors(tapF, nQOL_uppExtremity) %>% group_by(hand) %>%
  dplyr::mutate(p.val.adj = p.adjust(p.val, method='fdr'), survey = 'nQOL Upper Extremity')
cor_tapF_nQOL_lowExt <- get_tapF_nQOL_cors(tapF, nQOL_lowExtremity) %>% group_by(hand) %>%
  dplyr::mutate(p.val.adj = p.adjust(p.val, method='fdr'), survey = 'nQOL Lower Extremity')
cor_tapF <- rbind(cor_tapF_nQOL_cog, cor_tapF_nQOL_lowExt, cor_tapF_nQOL_uppExt)
library(grid)
p1 <- ggplot(cor_tapF, aes(x=cor, y=-log10(p.val.adj), color=hand)) + theme_few() + geom_point()  + facet_grid( . ~ survey )
p1 + scale_color_manual(values=c("#2EC4B6", "#E71D36")) + theme(text = element_text(size=15)) + xlab('correlation(spearman)') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))


p1 <- ggplot(cor_tapF %>% filter(survey == 'nQOL Upper Extremity'), 
             aes(x=cor, y=-log10(p.val.adj), color=hand)) + theme_few() + geom_point(size=1)
p1 <- p1 + scale_color_manual(values=c("#3C5488FF", "#7E6148FF")) + theme(text = element_text(size=10)) + xlab('correlation(spearman)') + 
  geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
p1
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/tapCorrelation_volcanoPlot_nQOLUpperExtremity.jpeg", plot=p1, height=3, width=4, units="in", dpi=300)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/tapCorrelation_volcanoPlot_nQOLUpperExtremity.tiff", plot=p1, height=3, width=4, units="in", dpi=300)
currentTheme <- theme_get()


get_scatter_plot <- function(features, survey, selected_feature){
  survey['T_Score'] = survey['T-Score']
  survey['T-Score'] <- NULL
  survey <- survey %>% select(healthCode, T_Score, rawScore, participant_week)
  tmp_df <- merge(features, survey) %>% filter(dataGroups == 'ms_patient')
  p <- ggscatter(tmp_df, x ='T_Score' , y = selected_feature,
            size = .3,
            add = "reg.line",  # Add regressin line
            add.params = list(color = "grey10", fill = "lightgray"),
            conf.int = TRUE, 
            cor.coef = TRUE,
            cor.method = "spearman",
            xlab = "NeuroQoL T Score",
            cor.coef.size = 4.5,
            cor.coeff.args = list(method = "pearson", fontface="italic",
                                  label.x.npc = "right", colour = "black",
                                  label.y.npc = "top"),
            ggtheme = currentTheme
  )
  p + theme_few() + theme(text = element_text(size=10)) 
}
p1 <- get_scatter_plot(tapF_week_averaged, nQOL_uppExtremity, 'numberTaps')
p1
ggsave("analysis/PAPER-1/FINAL_FIGS//tapping_top_feature_scatterPlot.png", p1, height=3, width=4, units="in", dpi=300)
ggsave("analysis/PAPER-1/FINAL_FIGS/tapping_top_feature_scatterPlot.tiff", p1, height=3, width=4, units="in", dpi=300)

