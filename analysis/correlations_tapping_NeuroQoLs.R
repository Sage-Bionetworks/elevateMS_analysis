rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools', "ggrepel")
install_load("plotly", "tableone", "webshot")
install_load('zipcode', 'maps', 'viridis', 'leaflet', 'RSelenium' )

# if ( !require(RSelenium) ) {
#   install.packages("RSelenium", repos = "https://cloud.r-project.org/")
# }
# install.packages("RSelenium")


# LOAD Data
FREEZED_DATA_LOC = "syn11657929"
load(synGet(FREEZED_DATA_LOC)@filePath)
ls()


#LIMIT Correlations to MS Participants only
tapF <- tapF %>% filter(dataGroups == 'ms_patient')
userActivity <- userActivity %>% filter(dataGroups == 'ms_patient')

#total finger tapping activities completed by FREEZE DATA (RIGHT + LEFT) hand
dim(tapF)
userActivity %>% filter(originalTable %in% c('Tapping Activity-v2')) %>%
  filter(dataGroups == 'ms_patient') %>% nrow()  ## combined 

#total neuroQOL's completed
sum(grepl('NeuroQOL', userActivity$originalTable))

#Number of users who completed the tapping and/or Neuro-QoL's
n_distinct(userActivity$healthCode[grepl('NeuroQOL', userActivity$originalTable)])

n_distinct(userActivity$healthCode[grepl('Tapping Activity-v2', userActivity$originalTable)])


tapF <- tapF %>% select(-tapping_left.json.TappingSamples, -tapping_right.json.TappingSamples, 
                                 -recordId, -tapping_left.json.ButtonRectLeft, 
                                 -tapping_right.json.ButtonRectRight, -tapping_left.json.ButtonRectRight,
                                 -tapping_left.json.endDate, -timeZone, 
                                 -metadata.json.dataGroups, -metadata.json.endDate.timezone,
                                 -tapping_left.json.endDate.timezone, -metadata.json.taskIdentifier, -createdOnTimeZone,
                                 -userSharingScope, -createdOn, -phoneInfo, -uploadDate, -externalId, 
                                 -validationErrors, -accelerometer_tapping_right.json.items,
                                 -accelerometer_tapping_left.json.items, -tapping_right.json.ButtonRectLeft,
                                 -tapping_right.json.endDate, -tapping_right.json.endDate.timezone,
                                 -tapping_right.json.startDate, -tapping_right.json.startDate.timezone,
                                 -tapping_left.json.startDate, -tapping_left.json.startDate.timezone,
                                 -tapping_right.json.TappingViewSize, -tapping_left.json.TappingViewSize,
                                 -metadata.json.scheduledActivityGuid, -idx, 
                                 -metadata.json.taskRunUUID, -metadata.json.startDate.timezone, -metadata.json.startDate,
                                 -metadata.json.endDate, -tapping_right_json_file,
                                 -tapping_left_json_file)

tmp_mean <- function(x){
  mean(x, na.rm = T)
}

tapF_week_averaged =  tapF %>% select( -activity_start_timestamp_GMT, -elevateMS_startDate_GMT,  -activity_start_timestamp_local, -study_week,
                                       -study_day, -error, -appVersion) %>% filter(dataGroups == 'ms_patient') %>%
  group_by(healthCode, participant_week, dataGroups, hand) %>% dplyr::summarise_all(.funs=c(tmp_mean)) %>% select(-participant_day)


get_tapF_nQOL_cors <- function(tapF, nQOL_survey){
  nQOL_survey['T_Score'] = nQOL_survey['T-Score']
  nQOL_survey['T-Score'] <- NULL
  nQOL_survey <- nQOL_survey %>% select(healthCode, T_Score, rawScore, participant_week)
  tmp_df <- merge(tapF_week_averaged, nQOL_survey)
  print(n_distinct(tmp_df$healthCode))
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
write.table(cor_tapF, file="Supplementary_table_1.tsv", sep="\t", row.names = F, col.names = T)
library(grid)
cor_tapF_nQOL_uppExt$label = ""
to_show_rows <- -log10(cor_tapF_nQOL_uppExt$p.val.adj) > 10
cor_tapF_nQOL_uppExt$label[to_show_rows] = cor_tapF_nQOL_uppExt$feature[to_show_rows]
p1 <- ggplot(cor_tapF_nQOL_uppExt, aes(x=cor, y=-log10(p.val.adj), color=hand)) + theme_few() + geom_point(size=.7)
p1 <- p1 + scale_color_manual(values=c("#2EC4B6", "#E71D36")) + theme(text = element_text(size=10)) + xlab('spearman correlation') + geom_hline(yintercept=2, color='grey30', size=.8) + theme(panel.spacing = unit(2, "lines"))
p1 <- p1 + geom_text_repel( aes(label = label), show.legend=F, size=2.5,
  point.padding = 0.15,
  box.padding = 0.15,
  nudge_y = 0.05)
p1
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/NeuroQoL_tapping_Correlation_volcano_plot.png", plot=p1, height=3, width=4, units="in", dpi=200)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/NeuroQoL_tapping_Correlation_volcano_plot.tiff", plot=p1, height=3, width=4, units="in", dpi=200)



#Scatter plot - number of taps and Neuro-QoL upp extreminty score
tmp_df <- merge(tapF_week_averaged, nQOL_uppExtremity)
tmp_df['Tscore'] = tmp_df['T-Score']
colnames(tmp_df)
p1 <- ggscatter(tmp_df, x ='Tscore' , y = 'numberTaps',
          size = .3,
          add = "reg.line",  # Add regressin line
          add.params = list(color = "grey10", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE,
          cor.method = "spearman",
          cor.coef.size = 4.5,
          cor.coeff.args = list(method = "spearman", fontface="italic",
                                label.x.npc = "right", colour = "black",
                                label.y.npc = "top")
) + xlab('Neuro-QoL Upper Extremity (T-Score)')
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/NeuroQoL_numberTaps_scatterPlot.png", plot=p1, height=3, width=4, units="in", dpi=200)
ggsave("~/dev/elevateMS_analysis/analysis/PAPER-1/FINAL_FIGS/NeuroQoL_numberTaps_scatterPlot.tiff", plot=p1, height=3, width=4, units="in", dpi=200)

