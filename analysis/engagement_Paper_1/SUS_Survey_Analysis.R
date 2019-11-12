rm(list=ls())
Sys.setenv(TZ='GMT')
library(synapser)
synapser::synLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('gdata')

sus <- fread (synGet("syn18145727")$path, data.table = F)

## adding temp row id
sus['userId'] = paste0('S-', 1:nrow(sus))


sus['age'] = sus$`How old are you?` 
sus$`How old are you?` <- NULL


sus['education'] = sus$`What is the highest level of education that you have completed?`
sus$`What is the highest level of education that you have completed?` <- NULL

sus['MS'] = sus$`Do you have Multiple Sclerosis (MS)?`
sus$`Do you have Multiple Sclerosis (MS)?` <- NULL

sus['clinicalReferral'] = sus$`Were you referred by a physician to join the study?`
sus$`Were you referred by a physician to join the study?` <- NULL


sus['ethinicity'] <- sus$`What ethnicity do you identify with (select one)?`
sus$`What ethnicity do you identify with (select one)?` <- NULL


sus['race'] <- sus$`What race do you identify as (select all that apply)?`
sus$`What race do you identify as (select all that apply)?`<- NULL

sus['gender'] <- sus$`What is your sex?`
sus$`What is your sex?` <- NULL


sus['smartphone_use_ease'] = sus$`How easy is it for you to use your smartphone?	`
sus$`How easy is it for you to use your smartphone?	` <- NULL

sus$Timestamp <- NULL

sus_data <- sus %>% select(c(1:10), 'userId') %>% column_to_rownames('userId')


### SUS Scoring 
colnames(sus)

##Ref - https://measuringu.com/sus/
# For odd items: subtract one from the user response.
sus_odd <- sus_data[, c(1,3,5,7,9)] - 1

# For even-numbered items: subtract the user responses from 5
sus_even <- 5 - sus_data[, c(2,4,6,8,10)]

# This scales all values from 0 to 4 (with four being the most positive response).
sus_data_mod <- cbind(sus_even, sus_odd)

# Add up the converted responses for each user and multiply that total by 2.5. This converts the range of possible values from 0 to 100 instead of from 0 to 40.
sus_data_mod['susScore'] = apply(sus_data_mod, 1, sum) * 2.5
sus_data_mod <- sus_data_mod %>% rownames_to_column('userId')
sus <- merge(sus, sus_data_mod %>% select(userId, susScore))


#SUS Stats
mean(sus$susScore)
median(sus$susScore)
sd(sus$susScore)

m <- sus %>% select(1:11) %>% column_to_rownames('userId') %>% t() %>% as.matrix()
ComplexHeatmap::Heatmap(m)



#### Testing for association between demog and SUS Score
install_load("broom")
sus_long <- sus %>% gather(feature, val, 12:19) %>% 
  select(susScore, feature, val) %>%
  filter(!is.na(val))

tmpLm <- function(df){
  lm(as.numeric(susScore) ~ val, data=df)
}
tmpLm <- possibly(tmpLm, otherwise = NA)

x <- sus_long %>% group_by(feature) %>%
  tidyr::nest() %>%
  dplyr::mutate(lmMod = purrr::map(data, tmpLm ),
                tidied = map(lmMod, broom::tidy)) %>%
  select(feature, tidied) %>%
  unnest()
summary(lm(susScore ~ as.numeric(age), data=sus))
