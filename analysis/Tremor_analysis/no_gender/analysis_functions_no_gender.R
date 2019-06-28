###### Purpose: Required functions for tremor analysis

## Get a rf model on preset parameters
getRfModel <- function(dat.train, 
                       responseNames,
                       num_trees = 1000){
  rf.mdl = ranger(dependent.variable.name = responseNames, 
                  data = dat.train, 
                  num.trees = num_trees,
                  importance = 'permutation',
                  seed = 123456,
                  classification = TRUE,
                  write.forest = TRUE)
  return(rf.mdl)
}

## Get a prediction on test data using an input rf model on preset parameters
getPrediction <- function(dat.test, rf.mdl){
  rf.prediction = predict(rf.mdl, 
                          data = dat.test,
                          predict.all = T,
                          type = 'response',
                          seed = 123456)
  return(rf.prediction)
}


## Function to classify case-control differences in each assay (balanced data)
rfModelCaseControl <- function(dat, responseNames, contFeatures, alwaysSplitVariables){
  # Get data and format data
  dat[,c(responseNames)] = lapply(dat[,c(responseNames), drop = F], factor)
  dat[,contFeatures] = lapply(dat[,contFeatures, drop = F], as.numeric)
  
  # Split training and testing data (based on healthCodes)
  hc = as.character(unique(dat$healthCode))
  hc.train = sample(hc, round(length(hc)*0.6))
  hc.test = setdiff(hc, hc.train)
  
  # Get training data
  dat.train = dat %>%
    dplyr::filter(healthCode %in% hc.train) %>%
    dplyr::select(one_of(contFeatures), 'MS') %>%
    dplyr::mutate(MS = as.numeric(MS) - 1)
  dat.train[is.na(dat.train)] = 0
  
  # Get testing data
  dat.test = dat %>%
    dplyr::filter(healthCode %in% hc.test) %>%
    dplyr::select(one_of(contFeatures), 'MS') %>%
    dplyr::mutate(MS = as.numeric(MS) - 1)
  dat.test[is.na(dat.test)] = 0
  
  tryCatch({
    # Fit a rf model
    rf.mdl = getRfModel(dat.train, responseNames)
    # Predict reponse of test data
    rf.prediction = getPrediction(dat.test, rf.mdl)
    # Get Model performance metrics like auroc(auc), acc and aupr
    mdl.performance <- getModelPerformanceMetrics(rf.prediction, dat.test)
    
    mdl.coeff = ranger::importance(rf.mdl) %>%
      rownameToFirstColumn('featureNames') %>%
      dplyr::rename(value = DF)
    
    mdl.param = rbind(
      data.frame(metricName = 'nData', value = dim(dat)[1]),
      data.frame(metricName = 'nTrain', value = dim(dat.train)[1]),
      data.frame(metricName = 'nTest', value = dim(dat.test)[1]),
      data.frame(metricName = 'nTrainControl', value = sum(dat.train[,'MS'] == 0)),
      data.frame(metricName = 'nTestControl', value = sum(dat.test[,'MS'] == 0))
    )
    
    all.results = list(param = mdl.param, performance = mdl.performance, coeff = mdl.coeff)
  }, error = function(e){
    all.results = list(param = NA, performance = NA, coeff = NA)
  })
}

## Function to display number of data points
printNumData <- function(mdls){
  mapply(function(x, y){
    x[[1]]$param %>% 
      plyr::rename(c('value'=y))
  }, mdls, names(mdls), SIMPLIFY = F) %>% 
    plyr::join_all()
}

## Function to plot and display average model performance
printModelPerformance <- function(mdl, assayName){
  tmp = purrr::imap(mdl, function(x, y){
    names(x) = paste0('perm.',1:length(x))
    tmp = purrr::imap(x, function(y,z){
      
      datTempInner <- y$performance
      if(is.na(datTempInner)){
        datTempInner <- data.frame(c('auroc','acc','aupr'),rep(NA,3))
        colnames(datTempInner) <- c('metricName','value')
      }
      return(datTempInner)
    }) %>%
      plyr::join_all(by = 'metricName')}) 
  
  ##### DATA RESHAPE IS NEEDED
  tmpa <- data.frame(rep(NA,3*(length(tmp$alternateModel)-1)),rep(NA,3*(length(tmp$alternateModel)-1)),rep(NA,3*(length(tmp$alternateModel)-1)))
  colnames(tmpa) <- c('metricName','value','modelName')
  tmpa$metricName <- rep(tmp$alternateModel$metricName,length(tmp$alternateModel)-1)
  a <- unlist(tmp$alternateModel)
  tmpa$value <- a[4:(3*length(tmp$alternateModel))]
  tmpa$modelName <- rep('alternateModel', length(tmpa$value))
  
  tmpb <- data.frame(rep(NA,3*(length(tmp$alternateModel)-1)),rep(NA,3*(length(tmp$alternateModel)-1)),rep(NA,3*(length(tmp$alternateModel)-1)))
  colnames(tmpb) <- c('metricName','value','modelName')
  tmpb$metricName <- rep(tmp$nullModel$metricName,length(tmp$alternateModel)-1)
  a <- unlist(tmp$nullModel)
  tmpb$value <- a[4:(3*length(tmp$nullModel))]
  tmpb$modelName <- rep('nullModel', length(tmpb$value))
  
  tmp <- rbind(tmpa,tmpb)
  
  p = ggplot(tmp, aes(x = metricName, y = value)) + geom_boxplot(aes(color = modelName), position = position_identity())
  p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylim(0,1) + ggtitle(assayName) + scale_color_manual(values = c('black','grey60'))
  
  tmp = dplyr::group_by(tmp, modelName, metricName) %>%
    dplyr::summarise(value = round(mean(value, na.rm = T),4)) %>%
    tidyr::spread(modelName, value) 
  return(list(plot = p, performance = tmp))
}

## Function to print top n features 
printTopFeatures <- function(mdl){
  purrr::imap(mdl, function(x, i){
    x$coeff %>%
      plyr::rename(c('value' = i))
  }) %>%
    plyr::join_all(by = 'featureNames') %>%
    tidyr::gather(iter, value, -featureNames) %>%
    dplyr::group_by(featureNames) %>%
    dplyr::summarise(value = sum(value != 0, na.rm = T)/100) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::filter(value >= 0.9) %>%
    dplyr::select(featureNames) %>%
    unlist %>% 
    paste(collapse = ', ')
}

## Get top n percent of features 
getTopNPercentOfFtrs <- function(mdl, n = 70){
  purrr::imap(mdl, function(x, i){
    x$coeff %>%
      plyr::rename(c('value' = i))
  }) %>%
    plyr::join_all() %>%
    tidyr::gather(iter, value, -featureNames) %>%
    dplyr::group_by(featureNames) %>%
    dplyr::summarise(value = sum(value != 0, na.rm = T)/100) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::filter(value >= n/100) %>%
    dplyr::select(featureNames) %>%
    unlist %>% 
    setdiff('(Intercept)')
}

## Loads the required libraries
loadRequiredLibraries <- function(load = TRUE){
  
  library(CovariateAnalysis) # get it from devtools::install_github('th1vairam/CovariateAnalysis@dev')
  library(data.table)
  library(tidyr)
  library(plyr)
  library(dplyr)
  library(synapser)
  library(knitr)
  library(githubr)
  library(ggplot2)
  library(parsedate)
  library(lubridate)
  library(ranger)
  library(ROCR)
  library(Boruta)
  library(MatchIt)
  library(caret)
  library(GA)
  library(foreach)
  library(doParallel)
  
}

## Get alternate and null model performance
getAlternateNullModelPerformance <- function(dat.train,
                                             idcol = 'healthCode',
                                             seed = 123456789, 
                                             nperm = 100,
                                             ftrs.to.evaluate = NULL,
                                             ftrs.to.remove = c('age','gender')){
  
  # input data dat.train has one column that identifies each row
  # one column named 'MS' that is the target/response variable
  # and the rest of the columns are features to evaluate
  # ftrs.to.remove is the ftrs you do not want to evaluate on
  
  set.seed(seed)
  exportPackages = c('dplyr','plyr','data.table','CovariateAnalysis', 'ranger', 'ROCR')
  if(is.null(ftrs.to.evaluate)){
    ftrs.to.evaluate <- setdiff(colnames(dat.train), idcol)
  }
  ftrs.to.evaluate <- setdiff(ftrs.to.evaluate, ftrs.to.remove)
  ftrs.to.evaluate <- setdiff(ftrs.to.evaluate, 'MS')
  
  rf.mdl.caret =  c('alternateModel','nullModel') %>%
    plyr::llply(function(mdl.tag, datTrain, nperm, ftrsNames, rfModelCaseControl){
      mdl = plyr::llply(seq(nperm), .fun = function(iperm, mdlTag, datTrain, ftrsNames, rfModelCaseControl){
        # Randomise case control label per healthCode
        if(mdlTag == 'nullModel')
          datTrain$MS = sample(datTrain$MS) 
        
        rfModelCaseControl(datTrain,  
                           responseNames = 'MS',
                           contFeatures = ftrsNames)
      }, mdl.tag, datTrain, ftrsNames, rfModelCaseControl,
      .parallel = F, 
      .paropts = list(.packages = exportPackages, 
                      .export = c('rfModelCaseControl')))
      return(mdl)
    },
    dat.train, nperm, ftrs.to.evaluate, rfModelCaseControl,
    .paropts = list(.packages = exportPackages, 
                    .export = c('rfModelCaseControl')))
  names(rf.mdl.caret) = c('alternateModel','nullModel')
  
  return(rf.mdl.caret)
}

## Get model performance metrics like auc, acc, and auroc
getModelPerformanceMetrics <- function(rf.prediction, dat.test){
  
  # Input is the output of predict(rf.mdl),
  # with the target column being named 'MS' 
  
  
  pred.probabilities = rowSums(rf.prediction$predictions == max(dat.test$MS), na.rm = T)/rf.prediction$num.trees
  
  mdl.prediction = pred.probabilities %>%
    prediction(predictions = ., labels = dat.test[,'MS'], label.ordering = c(0,1))
  
  auroc = data.frame(metricName = 'auroc', 
                     value = performance(mdl.prediction, 'auc')@y.values[[1]])
  acc = data.frame(metricName = 'acc',
                   value = max(performance(mdl.prediction, 'acc')@y.values[[1]]))
  
  rec = performance(mdl.prediction, 'rec')@y.values[[1]]
  pre = performance(mdl.prediction, 'prec')@y.values[[1]]
  
  aupr = data.frame(metricName = 'aupr', 
                    value = caTools::trapz(rec[!is.nan(rec) & !is.nan(pre)], 
                                           pre[!is.nan(rec) & !is.nan(pre)]))
  mdl.performance = rbind(auroc, acc, aupr)
  
  return(mdl.performance)
}
