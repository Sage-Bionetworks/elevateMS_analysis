#' This script is used for generating
#' walking adherence figures based on 
#' mPower paper supplementary figure 10
library(synapser)
library(tidyverse)
library(dplyr)
library(jsonlite)
library(githubr)
library(rhdf5)
library(data.table)


synLogin()
setGithubToken(
  readLines("/Users/atediarjo/git_token.txt"))


### Variables and Synapse References
SCRIPT_NAME <- "generate_walking_adherence_data.R"
PARENT_ID <- "syn22313816"
INTERMEDIARY_DATA <- "syn22313467"
OUTPUT_FIGURE <- "elevateMS_walking_adherence.pdf"
FREEZE_DATE   <- lubridate::ymd("2019-10-30")
START_DATE    <- lubridate::ymd("2017-08-14")
SYN_ID_REF <- list(baselineChar = 'syn17115631',
                   walkFeatures = 'syn10647801')
GIT_URL <- getPermlink(
  getRepo("arytontediarjo/elevateMS_analysis"), 
  repositoryPath = file.path('analysis/Analytical_Paper_2', SCRIPT_NAME))

### Helpers
filter.walk.data <- function(){
  baselineChar <- fread(synGet(SYN_ID_REF$baselineChar)$path)
  study_healthcodes <- unique(baselineChar$healthCode)
  flt.walk.data <- fread(synGet(SYN_ID_REF$walkFeatures)$path, 
                         data.table = F) %>%
    dplyr::select(-dataGroups) %>%
    dplyr::inner_join(baselineChar %>% 
                        dplyr::select(healthCode, dataGroups))%>% 
    dplyr::filter(createdOn >= START_DATE & createdOn <= FREEZE_DATE,
                  healthCode %in% study_healthcodes,
                  dataGroups == "ms_patient") 
  return(flt.walk.data)
}


main <- function(){
  ## read data for adherence checking
  dscores <- fread(synGet(INTERMEDIARY_DATA)$path) %>% 
    dplyr::filter(recordId %in% filter.walk.data()$recordId)
  
  ## Generate Figure
  tb <- table(na.omit(dscores %>% filter(vertical != "NA"))$vertical)
  pc <- 100*tb/sum(tb)
  figpath <- ""
  myylim <- c(0, nrow(dscores)/2)
  xlab <- "gravity acceleration"
  cexleg <- 2
  cm <- 1.75
  ca <- 1.5
  cl <- 1.5
  mat <- matrix(c(1, 1, 2,
                  3, 4, 5), 
                nr = 2, 
                nc = 3, 
                byrow = TRUE)
  
  title <- OUTPUT_FIGURE
  pdf(paste(figpath, title, sep = ""), width = 9, height = 6)
  layout(mat)
  par(mar = c(4, 3.75, 1, 0.5), mgp = c(2.5, 0.75, 0))
  plot(seq(3), seq(3), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  barplot(pc, ylim = c(0, max(pc) + 20), col = c("darkorange", "darkorange", "darkblue", "darkblue", "darkred", "darkred"),
          ylab = "percentage (%)", cex.names = 1.8, names.arg = c("-x", "+x", "-y", "+y", "-z", "+z"),
          cex.axis = ca, cex.lab = cl)
  legend("topleft", legend = "(b)", bty = "n", cex = cexleg)
  hist(dscores$median.x, ylim = myylim, xlab = xlab, cex.main = cm,
       main = "x-axis", col = "darkorange", cex.lab = cl, cex.axis = ca)
  legend("topleft", legend = "(c)", bty = "n", cex = cexleg)
  hist(dscores$median.y, ylim = myylim, xlab = xlab, cex.main = cm,
       main = "y-axis", col = "darkblue", cex.lab = cl, cex.axis = ca)
  legend("topleft", legend = "(d)", bty = "n", cex = cexleg)
  hist(dscores$median.z, ylim = myylim, xlab = xlab, cex.main = cm,
       main = "z-axis", col = "darkred", cex.lab = cl, cex.axis = ca)
  legend("topleft", legend = "(e)", bty = "n", cex = cexleg)
  dev.off()
  
  ## Store Features
  f <- synapser::File(title, parent=PARENT_ID)
  f$annotations <- list(substudy = "elevateMS",
                        analysis = "adherence analysis",
                        dataType = "figures")
  synStore(f, activity = Activity(
    "generate adherence figures",
    used = c(INTERMEDIARY_DATA),
    executed = GIT_URL))
  unlink(title)
}

main()
