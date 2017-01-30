library(randomForest)
library(OpenML)
library(mlr)
library(batchtools)

dir = "/home/probst/Paper/ntree_randomForest/experiments"
source(paste0(dir,"/code/ntree_defs.R"))

unlink("ntree", recursive = TRUE)
regis = makeExperimentRegistry("ntree", 
                               packages = c("mlr", "OpenML", "randomForest"), 
                               source = paste0(dir, "/code/ntree_defs.R"),
                               work.dir = paste0(dir, "/results"),
                               conf.file = paste0(dir, "/code/.batchtools.conf.R")
)
regis$cluster.functions = makeClusterFunctionsMulticore()

# add selected OML datasets as problems
for (did in OMLDATASETS) {
  data = list(did = did)
  addProblem(name = as.character(did), data = data)
}


addAlgorithm("eval", fun = function(job, data, instance, lrn.id, ...) {
  par.vals = list(...)
  oml.dset = getOMLDataSet(data$did)
  task = convertOMLDataSetToMlr(oml.dset)
  type = getTaskType(task)
  target = task$task.desc$target
  
  iters = 1000
  T = 2000
  run = matrix(NA, iters, T)  
  set.seed(105)
  for(i in 1:iters){
    print(paste(i))
    if (type == "classif") {
      data = droplevels(oml.dset$data) # drop not existing levels
      if(is.factor(data[, target]) == FALSE)
        data[, target] = as.factor(data[, target])
      run[i,] = randomForest(as.formula(paste0(target, "~.")), data = data, ntree = T, nodesize = 1)$err.rate[,1]
    } else {
      run[i,] = randomForest(as.formula(paste0(target, "~.")), data = oml.dset$data, ntree = T, nodesize = 1)$mse
    }
  }
  run
})

# Random maximin design
set.seed(130)
ades = data.frame()
for (lid in LEARNERIDS) {
ades = data.frame(t(c(1,2)))
}
addExperiments(algo.designs = list(eval = ades))

summarizeExperiments()
ids = chunkIds(findNotDone(), chunk.size = 10)

submitJobs(ids)
submitJobs(2)

#submitJobs(ids, resources = list(chunk.ncpus = 1)) 
getStatus()
getErrorMessages()
findErrors()

min(getJobStatus()$started, na.rm = T)
max(getJobStatus()$done, na.rm = T) # 7 Minuten -> one night

#lrn.id = "randomForest"
#par.vals = as.list(ades[1,-1])
#data$did = OMLDATASETS[187]

results = reduceResultsDataTable(ids = findDone(), fun = function(r) as.list(r), reg = regis, fill = TRUE)
res = reduceResultsList(ids = findDone(), reg = regis)

