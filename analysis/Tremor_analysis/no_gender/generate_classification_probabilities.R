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
age.records.matched.id = 'syn19123754'
age.records.matched.syn = synapser::synGet(age.records.matched.id)
age.records.matched = read.csv(age.records.matched.syn$path, sep = '\t')
all.used.ids = age.records.matched.id

###########################################################
## Download healthCode wise summarized features
###########################################################
summarized.ftrs.healthCode.id = 'syn20503415'
summarized.ftrs.healthCode = read.csv(synapser::synGet(summarized.ftrs.healthCode.id)$path, sep = '\t')
all.used.ids <- c(all.used.ids, summarized.ftrs.healthCode.id)

###########################################################
## Download record wise summarized features
###########################################################
summarized.ftrs.record.id = 'syn20503416'
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
                     dplyr::select(healthCode, MS, age, gender)) %>% 
  as.data.frame() %>% 
  droplevels() %>% 
  unique()
ftrs.to.evaluate <- setdiff(colnames(summarized.ftrs.healthCode), 'healthCode')

## Test data
dat.test <- summarized.ftrs.healthCode %>%
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
# Build rf model using train data
rf.mdl = getRfModel(dat.train, responseNames = 'MS')
rf.prediction = predict(rf.mdl, 
                        data = dat.test,
                        predict.all = T,
                        type = 'response',
                        seed = 123456)
pred.probabilities.1 = rowSums(rf.prediction$predictions == max(dat.test$MS), na.rm = T)/rf.prediction$num.trees

#### CASE - II (RF forest predicts a single probability using all 1000 trees, 
#               i.e the tree type is probability estimation)
# Build rf model using train data
rf.mdl = getRfModel(dat.train, responseNames = 'MS', prob = TRUE)
rf.prediction = predict(rf.mdl, 
                        data = dat.test,
                        predict.all = F,
                        type = 'response',
                        seed = 123456)
pred.probabilities.2 = rf.prediction$predictions[,1]

pred.prob <- data.frame(healthCode = dat.test$healthCode, 
                        MS = dat.test$MS,
                        prob_classification = pred.probabilities.1,
                        prob_probability_estimation = pred.probabilities.2)

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
OUTPUT_FILE <- "predition_probabilities.tsv" # name your file
write.table(pred.prob, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids,
         executed = thisFile)
unlink(OUTPUT_FILE)
