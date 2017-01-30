# R-Code for the Brier-Score
# Vergleiche Brier Score für randomForest nach 50 und nach 1000 Bäumen

library(mlr)
library(OpenML)
lrn = list(makeLearner("classif.randomForest", id = "50_rf", par.vals = list(ntree = 50), predict.type = "prob"), 
  makeLearner("classif.randomForest", id = "1000_rf", par.vals = list(ntree = 1000), predict.type = "prob"))
rdesc = makeResampleDesc(method = "RepCV", predict = "test", reps = 100, folds = 5)
configureMlr(on.learner.error = "warn", show.learner.output = FALSE)

dir =  "/home/probst/Paper/Ntree_RandomForest/experiments"
load(paste(dir,"/results/clas.RData", sep = ""))
tasks = rbind(clas_small)
OMLDATASETS = tasks$data.id[!(tasks$task.id %in% c(1054, 1071, 1065))] # Cannot guess task.type from data! for these 3

bmr = list()

for(i in 1:length(OMLDATASETS)) {
  print(i)
  oml.dset = getOMLDataSet(OMLDATASETS[i])
  task = convertOMLDataSetToMlr(oml.dset)
  bmr[[i]] = benchmark(lrn, task, resamplings = rdesc, measures = list(acc, ber, multiclass.brier, logloss, multiclass.au1u), 
    keep.pred = FALSE, models = FALSE, show.info = FALSE)
  print(bmr[[i]])
}

i = 29 # passt
i = 54
i = 147
i = 182
# logloss
i = 126
i = 129
# AUC
i = 14
i = 19
i = 62 # AUC -> AUC scheint schlechter werden zu können!
i = 95
# 


bmr[[1]]$results
leer = logical(length(bmr))
for(i in 1: length(bmr))
  leer[i] = getBMRAggrPerformances(bmr[[i]])[[1]][[2]][4]  < getBMRAggrPerformances(bmr[[i]])[[1]][[1]][4]
which(!leer)

save(bmr, file = paste0(dir,"/results/bmr_brier_score.RData"))
