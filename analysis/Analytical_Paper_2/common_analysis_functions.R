

tmp_calc_cor <- function(features, survey, use_first_common_week=F, additional_group_by=F){
  tmp_df <- merge(survey, features)
  if(use_first_common_week == T){
    ## Select the first common week where the participant completes the survey and sensor activity
    firstCommonWeek = tmp_df %>% group_by(healthCode) %>% 
      dplyr::summarise( participant_week = min(participant_week))
    tmp_df <- merge(tmp_df, firstCommonWeek)
  }
 
  group_by_cols = c('feature')
  if(additional_group_by != F){
    group_by_cols  = c(group_by_cols, additional_group_by)
  }
  tmp_df <- tmp_df %>% 
    dplyr::group_by_at(vars(group_by_cols)) %>% 
    dplyr::summarise(n= length(value),
                     totalIndividuals = n_distinct(healthCode),
                     cor = cor(value, TScore, use="complete.obs", method="spearman"),
                     p.val = cor.test(value, TScore, use="complete.obs", method="spearman")[['p.value']]) %>%
    as.data.frame()
  
  if(additional_group_by != F){
    tmp_df <- tmp_df %>% 
      dplyr::group_by_at(vars(additional_group_by)) %>%
      dplyr::mutate(p.val.adj = p.adjust(p.val, method='fdr'))
  }
  else {
    tmp_df <- tmp_df %>% dplyr::mutate(p.val.adj = p.adjust(p.val, method='fdr'))
  }
  
}


#######
#Scatter of two points selected from features and survey
######
get_scatter_plot <- function(features, survey, x,y, use_first_common_week=F){
  survey <- survey %>% select(healthCode, TScore, participant_week)
  features <- features %>% spread(feature, value)
  tmp_df <- merge(features, survey)
  
  if(use_first_common_week == T){
    ## Select the first common week where the participant completes the survey and sensor activity
    firstCommonWeek = tmp_df %>% group_by(healthCode) %>% 
      dplyr::summarise( participant_week = min(participant_week))
    tmp_df <- merge(tmp_df, firstCommonWeek)
  }
  
  p <- ggscatter(tmp_df, x=x , y=y,
                 size = .3,
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "grey10", fill = "lightgray"),
                 conf.int = TRUE, 
                 cor.coef = TRUE,
                 cor.method = "spearman",
                 xlab = x,
                 cor.coef.size = 2.5,
                 cor.coeff.args = list(method = "spearman", fontface="italic",
                                       label.x.npc = "center", colour = "black",
                                       label.y.npc = "top")
                 #ggtheme = currentTheme
  )
  p + theme_few() + theme(text = element_text(size=10)) 
}



######
# Wrapper function - to run correlation against all Neuro-QOLs
#####

get_neuroQOL_cors <- function(features, ...){
  x1 <- tmp_calc_cor(features, nQOL_cognition_week_avg, ...) %>% 
    mutate(survey = 'neuroQOL Cognition')
  x2 <- tmp_calc_cor(features, nQOL_uppExtremity_week_avg, ...) %>%
    mutate(survey = 'neuroQOL Upper Extremity')
  x3 <- tmp_calc_cor(features, nQOL_lowExtremity_week_avg, ... ) %>%
    mutate(survey = 'neuroQOL Lower Extremity')
  rbind(x1, x2, x3)
}


