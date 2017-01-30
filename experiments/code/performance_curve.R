# General function for calculating the performance measure curve for randomForest
library(OpenML)
library(randomForest)
library(matrixStats)
library(mlr)

task = getOMLTask(task.id = 3791, verbosity=0) 
taskmlr = convertOMLTaskToMlr(task)
mod = randomForest(binaryClass ~., data = task$input$data.set$data, ntree = 100, keep.inbag = TRUE)
mod$votes
mod$inbag
preds = predict(mod, newdata = task$input$data.set$data, predict.all = TRUE)
# These are not OOB-Predictions!; change randomForest for getting them
truth = mod$y

performance_curve = function(preds, truth, performance_measure, inbag) {
  # in binary case it can be useful to calculate it faster via cumsum or rowCumsums
  # implement it in C++ to make it faster; Combine it with OOB Prediction of randomForest (go inside randomForest)
  # or use the inbag information!!!!
  # first R Version
  
  # ntree = ncol(preds$individual)
  # nobs = nrow(preds$individual)
  # num_levels = nlevels(preds$aggr)
  # prob_list = vector("list", ntree) 
  # for(j in 1:ntree) {
  #   prob_matrix = matrix(NA, 2)
  #   for(i in 1:nobs) {
  #     frequence(preds$individual[i, 1:j])
  #   }
  # }
  
  ntree = ncol(preds$individual)  
  nobs = nrow(preds$individual)
  num_levels = nlevels(preds$aggr)
  pred_levels = levels(preds$aggr)
  prob_array = array(data = NA, dim = c(nobs, ntree, num_levels), dimnames = list(NULL, NULL, pred_levels))
  
  for(i in 1:length(pred_levels)) {
    predis = (preds$individual == pred_levels[i]) * 1
    predis = predis * ((inbag == 0) * 1) # only use observations that are out of bag
    predis = rowCumsums(predis)
    prob_array[, , i] = predis * (1 / rowCumsums((inbag == 0) * 1)) # divide by the number of observations that are out of bag
    #prob_array[, , i] = predis %*% diag(1/(1:ntree))
  }
  
  result = apply(prob_array, 2, function(x) performance_measure(x, truth))
  return(result)
}

curve_points = performance_curve(preds, truth, performance_measure = measureMulticlassBrier, inbag = mod$inbag)
plot(curve_points, main = "Performance over trees", type = "l", xlab = "ntree", ylab = "brier score")
curve_points = performance_curve(preds, truth, performance_measure = measureAU1P, inbag = mod$inbag)
plot(curve_points, main = "Performance over trees", type = "l", xlab = "ntree", ylab = "auc")

measureACCURACY = function(probabilities, truth) { 
    # Work needs to be done here
}
  
# use the inbag information!!!! (of randomForest)



# Generalize to any measure of mlr
lrn = makeLearner("classif.lda", predict.type = "prob")
n = getTaskSize(sonar.task)
mod = train(lrn, task = sonar.task, subset = seq(1, n, by = 2))
pred = predict(mod, task = sonar.task, subset = seq(2, n, by = 2))
## Performance for the default threshold 0.5
performance(pred, measures = list(fpr, fnr, mmce))




OOBCurve = function(preds, truth, measures, inbag, taskmlr) {
  ntree = ncol(preds$individual)
  nobs = nrow(preds$individual)
  num_levels = nlevels(preds$aggr)
  pred_levels = levels(preds$aggr)
  prob_array = array(data = NA, dim = c(nobs, ntree, num_levels), dimnames = list(NULL, NULL, pred_levels))
  for(i in 1:length(pred_levels)) {
    predis = (preds$individual == pred_levels[i]) * 1
    predis = predis * ((inbag == 0) * 1) # only use observations that are out of bag
    predis = rowCumsums(predis)
    prob_array[, , i] = predis * (1 / rowCumsums((inbag == 0) * 1)) # divide by the number of observations that are out of bag
    #prob_array[, , i] = predis %*% diag(1/(1:ntree))
  }
  calculateMlrMeasure = function(x, measures) {
    mlrpred = makePrediction(task.desc = taskmlr$mlr.task$task.desc, row.names = names(truth), id = names(truth), truth = truth,
      predict.type = "prob", predict.threshold = 0.5, y = x, time = NA)
    performance(mlrpred, measures)
  }
  result = apply(prob_array, 2, function(x) calculateMlrMeasure(x, measures))
  return(result)
}



# (maybe also do that for xgboost)



