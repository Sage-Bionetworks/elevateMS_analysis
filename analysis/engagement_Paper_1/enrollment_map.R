rm(list=ls())
Sys.setenv(TZ='GMT')
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr', 'synapser'))
install_load('ggthemes', 'devtools')
install_load("plotly", "tableone", "webshot")
install_load('zipcode', 'maps', 'viridis', 'leaflet', 'RSelenium' )
synapser::synLogin()
install_load('plotly')
if ( !require(RSelenium) ) {
  install.packages("RSelenium", repos = "https://cloud.r-project.org/")
}
install_load("RSelenium")
source("analysis/loadData.R")



#load the zipcode level data summarized by first three digits of zipcode
data(zipcode)
states = data.frame(state = state.abb, state.name = tolower(state.name))
tmp_zipcode <- zipcode %>% inner_join(states)
tmp_zipcode['zipcode_firstThree'] = substr(tmp_zipcode$zip, 1,3)
tmp_zipcode <- tmp_zipcode %>% dplyr::group_by(state, state.name, zipcode_firstThree) %>% 
  dplyr::summarise(longitude = median(longitude), latitude = median(latitude)) %>% as.data.frame() %>%
  dplyr::mutate(state = state.name) %>% dplyr::select(-state.name)

#create the new col to match to tmp_zipcode
baselineChar['zipcode_firstThree'] = substr(baselineChar$zipcode, 1,3)
tmp_user_region <- merge(baselineChar, tmp_zipcode)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray90"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)
p <- plot_geo(tmp_user_region, locationmode = 'USA-states') %>%
  add_markers( x = ~longitude, y = ~latitude, color=~dataGroups, opacity=.8, 
               colors=c(COL_CONTROL, COL_MS_PATIENT)) %>%
  layout(title='', geo=g) %>% layout(legend = list(x = 0.9, y = 0.9))
p  %>%
  export(file = "enrollment.svg",
         selenium = RSelenium::rsDriver(browser = "chrome"))
