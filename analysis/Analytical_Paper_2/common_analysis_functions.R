# ("devtools")
# devtools::install_github("hadley/multidplyr")


permuteCorrelations <- function(data, nPermute){
  res <- data %>% 
    modelr::permute(n=nPermute,  c('TScore'), .id = 'permute') %>%
    dplyr::mutate(cors = map(perm, ~ with(as.data.frame(.), cor(value, TScore, method="spearman", use="complete.obs"))))
  unlist(res$cors)
}



tmp_calc_cor <- function(features, survey, use_first_common_week=F, additional_group_by=F, nPermute=10000){
  
  # survey = nQOL_cognition_week_avg
  # features = tapF_week_averaged
  # use_first_common_week=F
  # additional_group_by ='hand'
  # nPermute = 10
  # library(multidplyr)
  # library(dplyr, warn.conflicts = FALSE)
  # cluster <- new_cluster(3)
  
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
  
  res <- tmp_df %>%
   dplyr::group_by_at(vars(group_by_cols)) %>%
   tidyr::nest() %>%
   dplyr::mutate(cor = map(data, ~ cor.test(.x$value, .x$TScore, use="complete.obs", method="spearman"))) %>%
   dplyr::mutate(corPermuted  = map(data, permuteCorrelations, nPermute=nPermute))  %>% ###permuted correlations
   dplyr::mutate(corRes = map(cor, broom::tidy )) %>%
   dplyr::select(-data, -cor) %>%
   tidyr::unnest(corRes)
   
  if(additional_group_by != F){
    res <- res %>% 
      dplyr::group_by_at(vars(additional_group_by)) %>%
      dplyr::mutate(p.val.adj = p.adjust(p.value, method='fdr'))
  } else {
    res <- res %>% dplyr::mutate(p.val.adj = p.adjust(p.value, method='fdr'))
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


