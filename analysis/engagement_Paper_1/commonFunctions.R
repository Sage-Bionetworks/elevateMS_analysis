calc_user_participation_stats <- function(df, mData, studyName){
  tmp <- df %>% 
    filter(participantDay <=  84) %>%
    dplyr::group_by(healthCode) %>%
    dplyr::summarise(lastWeekinStudy = max(participant_week),
                     totalDaysActive = n_distinct(lubridate::date(createdOn)),
                     duration_in_study = as.numeric(max(lubridate::date(createdOn)) - min(lubridate::date(createdOn)) + 1),
                     regularity = round((totalDaysActive/duration_in_study) * 100, digits=2)) %>%
    mutate(study = studyName) 
  tmp <- merge(tmp, mData, all.x=T)
}