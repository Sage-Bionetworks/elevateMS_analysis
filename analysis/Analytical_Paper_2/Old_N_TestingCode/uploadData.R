Sys.setenv(TZ='GMT')
library(synapseClient)
synapseLogin()
library(install.load)
install_load(c('plyr', 'tidyverse', 'data.table', 'lubridate', 'ggpubr'))
install_load('ggthemes', 'gridExtra', 'devtools', 'DT', 'lattice')


#upload analysis
parentId = "syn11315047"
synStore(File("analysis/analysis.html", parentId = parentId),
         used = c("syn9758009", "syn10295288", "syn10235463",
                  "syn10232189", "syn9872551", "syn9765702",
                  "syn9758010", "syn11336100", "syn10647801",
                  "syn10669596", "syn10648415", "syn11315757",
                  "syn11315765", "syn11315747", "syn11171602"),
         executed = "https://github.com/Sage-Bionetworks/elevateMS_analysis/blob/master/analysis/analysis.Rmd")
