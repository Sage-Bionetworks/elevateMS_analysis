###########
## Summary heatmap across individuals vs features
##########
tapF_averaged =  tmp_tapF %>% select(healthCode, hand, feature, value) %>%
  group_by(healthCode, hand, feature) %>% dplyr::summarise_all(.funs=c(median), na.rm=T) 

tapF_averaged_ranks <- tapF_averaged %>% group_by(feature, hand) %>% 
  mutate(healthCode = healthCode,
         rank  = as.numeric(frank(value, na.last = "keep", ties.method = "dense"))) %>%
  select(-value) %>%
  spread(feature, rank) %>%
  filter(hand == 'right') %>% as_data_frame() %>%
  select(-hand)

tapF_averaged_ranks$hand <- NULL
tmp_m <- tapF_averaged_ranks
rownames(tmp_m) <- tapF_averaged_ranks$healthCode
tmp_m$healthCode <- NULL
tmp_m <- na.omit(tmp_m)
## Annotation
rowAnnot <- baselineChar %>% select(healthCode, age) %>%
  column_to_rownames(var='healthCode')
rowAnnot <- ComplexHeatmap::rowAnnotation(df=rowAnnot)

png("analysis/engagement_Paper_1/FINAL_FIGS/tapFeatures_summary.png", res = 200,
    height = 10, width = 12, units="in")
ComplexHeatmap::Heatmap(tmp_m, show_column_names = T, 
                        row_title = 'individuals',
                        show_row_dend = F,
                        row_names_side = 'left',
                        row_dend_side = 'right',
                        row_title_gp = gpar(fontsize=8),
                        row_names_gp = gpar(fontsize=8),
                        heatmap_legend_param = list(title = "feature rank"),
                        clustering_distance_rows="spearman",
                        col = colorRamp2(breaks = seq(0,400,by=50),
                                         colors = RColorBrewer::brewer.pal(9, "Spectral")))
dev.off()



dsst_flt_averaged =  dsst_flt %>% select(healthCode, feature, value) %>%
  group_by(healthCode, feature) %>% dplyr::summarise_all(.funs=c(mean), na.rm=T)



walkF_averaged =  tmp_walkF %>% select(healthCode, feature, value) %>%
  group_by(healthCode, feature) %>% dplyr::summarise_all(.funs=c(median), na.rm=T) 

walkF_averaged_ranks <- walkF_averaged %>% group_by(feature) %>% 
  mutate(healthCode = healthCode,
         rank  = as.numeric(frank(value, na.last = "keep", ties.method = "dense"))) %>%
  select(-value) %>%
  spread(feature, rank)

tmp_m <- walkF_averaged_ranks %>% column_to_rownames(var='healthCode')
tmp_m <- na.omit(tmp_m)

## Annotation
rowAnnot <- baselineChar %>% select(healthCode, age) %>%
  column_to_rownames(var='healthCode')
rowAnnot <- rowAnnotation(df=rowAnnot)

png("analysis/engagement_Paper_1/FINAL_FIGS/walkFeatures_summary.png", res = 200,
    height = 10, width = 15, units="in")
ComplexHeatmap::Heatmap(tmp_m, show_column_names = T, 
                        row_title = 'individuals',
                        show_row_dend = F,
                        row_names_side = 'left',
                        row_dend_side = 'right',
                        row_title_gp = gpar(fontsize=8),
                        row_names_gp = gpar(fontsize=8),
                        heatmap_legend_param = list(title = "feature rank"),
                        clustering_distance_rows="spearman",
                        col = colorRamp2(breaks = seq(0,250,by=50),
                                         colors = RColorBrewer::brewer.pal(6, "Spectral")))
dev.off()


#### Correlations
nQOL_uppExtremity <- nQOL_uppExtremity %>% select(c(2,21:31))
nQOL_lowExtremity <- nQOL_lowExtremity %>% select(c(2,21:31))
nQOL_cognition <- nQOL_cognition  %>% select(c(2,21:31))




# dsst_flt_averaged_ranks <- dsst_flt_averaged %>% group_by(feature) %>% 
#   mutate(healthCode = healthCode,
#          rank  = as.numeric(frank(value, na.last = "keep", ties.method = "dense"))) %>%
#   select(-value) %>%
#   spread(feature, rank)
# 
# tmp_m <- dsst_flt_averaged_ranks %>% column_to_rownames(var='healthCode')
# tmp_m <- na.omit(tmp_m)
# 
# 
# png("analysis/engagement_Paper_1/FINAL_FIGS/cognitionFeatures_summary.png", res = 200,
#     height = 10, width = 15, units="in")
# ComplexHeatmap::Heatmap(tmp_m, show_column_names = T, 
#                         row_title = 'individuals',
#                         show_row_dend = F,
#                         row_names_side = 'left',
#                         row_dend_side = 'right',
#                         row_title_gp = gpar(fontsize=8),
#                         row_names_gp = gpar(fontsize=8),
#                         heatmap_legend_param = list(title = "feature rank"),
#                         clustering_distance_rows="spearman",
#                         col = colorRamp2(breaks = seq(0,250,by=50),
#                                          colors = RColorBrewer::brewer.pal(6, "Spectral")))
# dev.off()
# 