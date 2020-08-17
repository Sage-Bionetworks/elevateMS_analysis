#' This script is used for generating
#' walking adherence data based on 
#' gravitational value queried from acceleromter

library(synapser)
library(tidyverse)
library(dplyr)
library(jsonlite)
library(githubr)
library(rhdf5)
library(purrr)
library(data.table)

synLogin()

### global variable
SCRIPT_NAME <- "generate_walking_adherence_data.R"
PARENT_ID <- "syn22311179"
OUTPUT_FILE <- "walking_adherence_data.tsv"
SYN_ID_REF <- list(
  WALKING_V1 = list(TBL = "syn9934066",
                    COL = c("deviceMotion_walking_outbound.json")),
  WALKING_V2 = list(TBL = "syn10278766", 
                    COL = c("deviceMotion_walking_outbound.json.items")))
GIT_URL <- getPermlink(
  "/Users/atediarjo/git_token.txt", 
  repositoryPath = file.path('analysis/Analytical_Paper_2', SCRIPT_NAME))


### Helpers
#' Get Direction based on mPower Walking Data
GetMainDirection <- function(metaData) {
  MainDirection <- function(dat) {
    mse1 <- mean((dat$x - 1)^2, na.rm = TRUE)
    mse2 <- mean((dat$x + 1)^2, na.rm = TRUE)
    mse3 <- mean((dat$y - 1)^2, na.rm = TRUE)
    mse4 <- mean((dat$y + 1)^2, na.rm = TRUE)
    mse5 <- mean((dat$z - 1)^2, na.rm = TRUE)
    mse6 <- mean((dat$z + 1)^2, na.rm = TRUE)
    aux <- which.min(c(mse1, mse2, mse3, mse4, mse5, mse6))
    medianAccel <- apply(dat, 2, median, na.rm = TRUE)
    if (aux == 1) {
      vertical <- "x+"
    }
    if (aux == 3) {
      vertical <- "y+"
    }
    if (aux == 5) {
      vertical <- "z+"
    }
    if (aux == 2) {
      vertical <- "x-"
    }
    if (aux == 4) {
      vertical <- "y-"
    }
    if (aux == 6) {
      vertical <- "z-"
    }
    list(vertical = vertical, 
         medianAccel = medianAccel)
  }
  nrec <- nrow(metaData)
  out <- data.frame(matrix(NA, nrec, 6))
  colnames(out) <- c("recordId", "healthCode", "vertical", 
                     "median.x", "median.y", "median.z")
  out$recordId <- metaData$recordId
  out$healthCode <- metaData$healthCode
  for (i in seq(nrec)) {
    cat(i, "\n")
    if (!is.na(metaData$jsonPath[i])) {
      dat <- jsonlite::fromJSON(as.character(metaData$jsonPath[i]))
      if("items" %in% names(dat)){
        dat <- dat$items
      }
      gdat <- dat$gravity
      aux <- MainDirection(gdat)
      out[i, "vertical"] <- aux$vertical
      out[i, 4:6] <- aux$medianAccel
    }
  }
  return(out)
}

#' function to query elevateMS V1 and elevateMS V2, join
#' filepaths, and row bind both table
get.walk.data <- function(ref){
  walk.tbl.entity <- synTableQuery(sprintf("SELECT * FROM %s", ref$TBL))
  walk.tbl.data <- walk.tbl.entity$asDataFrame() %>%
    dplyr::rename(fileHandleId := sym(ref$COL))
  if(length(record_subset) != 0){
    walk.tbl.data <- walk.tbl.data %>% 
      dplyr::filter(recordId %in% record_subset)
  }
  mapped.walk.json.files <- synDownloadTableColumns(walk.tbl.entity, ref$COL)
  mapped.walk.json.files <- data.frame(fileHandleId = names(mapped.walk.json.files),
                                       jsonPath = as.character(mapped.walk.json.files))
  walk.data <- walk.tbl.data %>% 
    dplyr::left_join(., mapped.walk.json.files, 
                     by = c("fileHandleId"))
  return(walk.data)
}

main <- function(){
  dscores <- purrr::map(SYN_ID_REF, function(x){
    get.walk.data(x)}) %>% 
    rbindlist() %>% 
    GetMainDirection(metaData = .) %>%
    write.table(., OUTPUT_DATA, 
                sep="\t", row.names=F, quote=F)
  f <- synapser::File(OUTPUT_FILE, PARENT_ID)
  f$annotations <- list(analysis = "adherence analysis",
                        substudy = "elevateMS",
                        dataType = "intermediary data")
  synStore(f, activity = Activity(
    "Run Adherence Information",
    executed = GIT_URL, 
    used = c(SYN_ID_REF$WALKING_V1$TBL,
             SYN_ID_REF$WALKING_V2$TBL)))
  unlink(OUTPUT_DATA)
}



