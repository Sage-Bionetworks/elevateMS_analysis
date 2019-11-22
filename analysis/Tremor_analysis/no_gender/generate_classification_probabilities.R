# Generate the classification probabilities (case vs control) using RF models

###########################################################
## Required libraries and analysis functions
###########################################################
## It is assumed your working directory is where this file
source('analysis_functions_no_gender.R')
loadRequiredLibraries()
synapser::synLogin()
cl = makeCluster(detectCores()-2)
registerDoParallel(cl)

###########################################################
## Download age matched healthcodes
###########################################################
age.records.matched.id = 'syn19275827' # age matched without gender
age.records.matched.syn = synapser::synGet(age.records.matched.id)
age.records.matched = read.csv(age.records.matched.syn$path, sep = '\t')
all.used.ids = age.records.matched.id

###########################################################
## Download healthCode wise summarized features
###########################################################
summarized.ftrs.healthCode.id = 'syn21297772'
summarized.ftrs.healthCode = read.csv(synapser::synGet(summarized.ftrs.healthCode.id)$path, sep = '\t')
all.used.ids <- c(all.used.ids, summarized.ftrs.healthCode.id)

###########################################################
## Download record wise summarized features
###########################################################
summarized.ftrs.record.id = 'syn21297786'
summarized.ftrs.record = read.csv(synapser::synGet(summarized.ftrs.record.id)$path, sep = '\t')
all.used.ids <- c(all.used.ids, summarized.ftrs.record.id)

###########################################################
## Download demohgraphics data for elevateMS
###########################################################
demo.tbl.id.ms = 'syn17115631' # elevateMS_baselineCharacteristics
demo.tbl.syn.ms <- synapser::synGet(demo.tbl.id.ms)
demo.tbl.ms <- demo.tbl.syn.ms$path %>% read.csv(sep = '\t')
all.used.ids <- c(all.used.ids, demo.tbl.id.ms)

## Training data
dat.train <- summarized.ftrs.healthCode %>% 
  dplyr::filter(healthCode %in% age.records.matched$healthCode) %>% 
  dplyr::left_join(age.records.matched %>% 
                     dplyr::select(healthCode, MS, age)) %>% 
  as.data.frame() %>% 
  droplevels() %>% 
  unique()
ftrs.to.evaluate <- setdiff(colnames(summarized.ftrs.healthCode), 'healthCode')

## Test data
dat.test.hc <- summarized.ftrs.healthCode %>%
  # dplyr::filter(!(healthCode %in% age.records.matched$healthCode)) %>%
  dplyr::left_join(demo.tbl.ms %>%
                     dplyr::select(healthCode, gender, age, MS = dataGroups)) %>%
  dplyr::filter(MS %in% c('ms_patient','control'),
                gender %in% c('male','female'),
                !is.na(age), !is.na(MS), !is.na(gender)) %>% #remove NAs
  dplyr::mutate(MS = factor(MS, levels = c('control','ms_patient')),
                MS = as.numeric(MS) - 1) %>% 
  unique() %>% 
  droplevels()

dat.test.record <- summarized.ftrs.record %>%
  # dplyr::filter(!(healthCode %in% age.records.matched$healthCode)) %>%
  dplyr::left_join(demo.tbl.ms %>%
                     dplyr::select(healthCode, gender, age, MS = dataGroups)) %>%
  dplyr::filter(MS %in% c('ms_patient','control'),
                gender %in% c('male','female'),
                !is.na(age), !is.na(MS), !is.na(gender)) %>% #remove NAs
  dplyr::mutate(MS = factor(MS, levels = c('control','ms_patient')),
                MS = as.numeric(MS) - 1) %>% 
  unique() %>% 
  droplevels()

#### CASE - I (RF forest predicts classes for all 1000 trees i.e building a
#              classification forest/tree)
####

# Build rf model using train data
rf.mdl = getRfModel(dat.train, responseNames = 'MS')

# Predict probabilities for each healthCode
rf.prediction.hc = predict(rf.mdl, 
                        data = dat.test.hc,
                        predict.all = T,
                        type = 'response',
                        seed = 123456)
pred.probabilities.1.hc = rowSums(rf.prediction.hc$predictions == max(dat.test.hc$MS), na.rm = T)/rf.prediction.hc$num.trees

# Predict probabilities for each record
rf.prediction.record = predict(rf.mdl, 
                        data = dat.test.record,
                        predict.all = T,
                        type = 'response',
                        seed = 123456)
pred.probabilities.1.record = rowSums(rf.prediction.record$predictions == max(dat.test.record$MS), na.rm = T)/rf.prediction.record$num.trees

#### CASE - II (RF forest predicts a single probability using all 1000 trees, 
#               i.e the tree type is probability estimation)
####

# Build rf model using train data
rf.mdl = getRfModel(dat.train, responseNames = 'MS', prob = TRUE)

# Predict probabilities for each healthCode
rf.prediction.hc = predict(rf.mdl, 
                        data = dat.test.hc,
                        predict.all = F,
                        type = 'response',
                        seed = 123456)
pred.probabilities.2.hc = rf.prediction.hc$predictions[,1]

# Predict probabilities for each record
rf.prediction.record = predict(rf.mdl, 
                           data = dat.test.record,
                           predict.all = F,
                           type = 'response',
                           seed = 123456)
pred.probabilities.2.record= rf.prediction.record$predictions[,1]


pred.prob.hc <- data.frame(healthCode = dat.test.hc$healthCode, 
                        MS = dat.test.hc$MS,
                        prob_classification = pred.probabilities.1.hc,
                        prob_probability_estimation = pred.probabilities.2.hc)

pred.prob.record <- data.frame(healthCode = dat.test.record$healthCode,
                               recordId = dat.test.record$recordId,
                               MS = dat.test.record$MS,
                               Assay = dat.test.record$Assay,
                               prob_classification = pred.probabilities.1.record,
                               prob_probability_estimation = pred.probabilities.2.record) %>% 
  dplyr::left_join(summarized.ftrs.record %>% 
                     dplyr::select(
                       setdiff(colnames(summarized.ftrs.record), colnames(summarized.ftrs.healthCode))
                     )) %>% 
  unique()

###########################################################
## Github Parameters and Synapse
###########################################################
# Synapse parameters
parentId = 'syn19120636';
activityName = 'elevateMS Kinetic tremor case-control classification probabilities';
activityDescription = 'Classify case-control in kinetic state tremors using random forest models with median and iqr features, for Multiple Sclerosis. The training is done using controls from mpower and elevateMS, and cases from elevateMS'

# Github link
gtToken = '~/github_token.txt'
githubr::setGithubToken(as.character(read.table(gtToken)$V1))

thisFileName <- 'generate_classification_probabilities.R'

thisRepo <- getRepo(repository = "itismeghasyam/elevateMS_analysis", ref="branch", refName='master')
thisFile <- getPermlink(repository = thisRepo, repositoryPath=paste0('analysis/Tremor_analysis/no_gender/',thisFileName))

# upload to Synapse, prediction probabilities
synapse.folder.id <- parentId # synId of folder to upload your file to
OUTPUT_FILE <- "predition_probabilities_hc.tsv" # name your file
write.table(pred.prob.hc, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = thisFile)
unlink(OUTPUT_FILE)

# upload to Synapse, prediction probabilities
synapse.folder.id <- parentId # synId of folder to upload your file to
OUTPUT_FILE <- "predition_probabilities_record.tsv" # name your file
write.table(pred.prob.record, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = thisFile)
unlink(OUTPUT_FILE)
