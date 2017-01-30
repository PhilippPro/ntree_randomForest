library(mlr)
library(OpenML)
library(batchtools)
library(plyr)

dir =  "/home/probst/Paper/Ntree_RandomForest/experiments"
setwd(paste0(dir,"/results"))
options(batchtools.progress = FALSE)

regis = loadRegistry("ntree")
res = reduceResultsList(ids = findDone(), reg = regis)

pdf(paste(dir,"/results/graphics/clas_ntree.pdf", sep = ""), width = 6, height = 6)
par(mfrow = c(2, 2))
for (i in 1:193) {
  print(i)
  mean_oob = apply(res[[i]], 2, mean)
  plot(mean_oob, type = "l", main = i, xlab = "ntree", ylab = "oob mmce")
}
dev.off()

pdf(paste(dir,"/results/graphics/reg_ntree.pdf", sep = ""), width = 6, height = 6)
par(mfrow = c(2, 2))
for (i in 194:length(res)) {
  print(i)
  mean_oob = apply(res[[i]], 2, mean)
  plot(mean_oob, type = "l", main = i, xlab = "ntree", ylab = "oob mse")
}
dev.off()

length(res)
dim(res[[1]])

# Analysiere die Grafiken...

# Difference from bottom to the end
diffs = numeric(193)
diffs2 = numeric(193)
for (i in 1:193) {
  print(i)
  mean_oob = apply(res[[i]], 2, mean)
  diffs[i] = mean_oob[2000] - min(mean_oob)
  diffs2[i] = min(mean_oob[21:2000]) - min(mean_oob[1:20])
}
par(mfrow = c(1, 1))
boxplot(diffs)
sum(diffs > 0.005) 
sum(diffs > 0.005) / length(diffs2)

bigger = function(x) {
  sum(diffs > x)
}
plot(seq(0, 0.05, 0.001), sapply(seq(0, 0.05, 0.001), bigger), type = "l", xlab = "threshold", ylab = "number of differences bigger than threshold")

boxplot(diffs2)
sum(diffs2 > 0)
sum(diffs2 < 0)
sum(diffs2[1:100] > 0)
sum(diffs2[1:100] < 0)
sum(diffs2[96:193] > 0)
sum(diffs2[97:193] < 0)
sum(diffs2 > 0) / length(diffs2)
# Bei kleinen Datensätzen häufiger
# in ca. 3.6 % der Fälle

diffs = numeric(length(res) - 193)
diffs2 = numeric(length(res) - 193)
for (i in 194:length(res)) {
  print(i)
  mean_oob = apply(res[[i]], 2, mean)
  diffs[i-193] = mean_oob[2000] - min(mean_oob)
  diffs2[i-193] = mean_oob[2000] - min(mean_oob[1:1000])
}
boxplot(diffs)
sum(diffs > 0.005)
sum(diffs2 > 0)
# Hier tritt das Phänomen gar nicht auf


# Graphics for the paper
load(paste(dir,"/results/clas.RData", sep = ""))
pdf(file = paste0("/home/probst/Paper/Ntree_RandomForest/Paper/", "initial_example.pdf"), width = 12, height = 6)
par(mfrow = c(1,2))
# Dataset 36
clas[36,]
mean_oob = apply(res[[36]], 2, mean)
plot(mean_oob, type = "l", main = "Dataset with OpenML ID 938", xlab = "number of trees", ylab = "mean oob error rate")

mean_oob = apply(res[[111]], 2, mean)
plot(mean_oob, type = "l", main = "Dataset with OpenML ID 949", xlab = "number of trees", ylab = "mean oob error rate")
dev.off()


pdf(file = paste0("/home/probst/Paper/Ntree_RandomForest/Paper/", "histogram.pdf"), width = 10, height = 4)
par(mfrow = c(1,3))
oml.dset = getOMLDataSet(clas[36,]$data.id)
task = convertOMLDataSetToMlr(oml.dset)
type = getTaskType(task)
target = task$task.desc$target
data = droplevels(oml.dset$data) # drop not existing levels
if(is.factor(data[, target]) == FALSE)
  data[, target] = as.factor(data[, target])
mod = randomForest(as.formula(paste0(target, "~.")), data = data, ntree = 100000, nodesize = 1)
preds = predict(mod, type = "prob")
cols = data[,target] == "P"
probs = c(preds[which(cols == TRUE), 1], preds[which(cols == FALSE), 2])
hist(probs, main = "Dataset with OpenML ID 938", xlab = "Probabilities for correct classification")

oml.dset = getOMLDataSet(clas[111,]$data.id)
task = convertOMLDataSetToMlr(oml.dset)
type = getTaskType(task)
target = task$task.desc$target
data = droplevels(oml.dset$data) # drop not existing levels
if(is.factor(data[, target]) == FALSE)
  data[, target] = as.factor(data[, target])
mod = randomForest(as.formula(paste0(target, "~.")), data = data, ntree = 10000, nodesize = 1)
preds = predict(mod, type = "prob")
cols = data[,target] == "P"
probs = c(preds[which(cols == TRUE), 1], preds[which(cols == FALSE), 2])
hist(probs, main = "Dataset with OpenML ID 949", xlab = "Probabilities for correct classification")
hist(probs[probs > 0.1 & probs < 0.9], main =  "Dataset with OpenML ID 949 scaled on [0.1,0.9]", xlab = "Probabilities for correct classification")
# wie erwartet
dev.off()





# Annex, old

# How do the p_i look like in growing cases (examples)
load(paste(dir,"/results/clas.RData", sep = ""))
par(mfrow = c(2, 2))

# Dataset 24
mean_oob = apply(res[[24]], 2, mean)
plot(mean_oob, type = "l", main = "Mean of OOB-MMCE of 1000 Random Forests on Dataset 24", xlab = "ntree", ylab = "oob mmce")

oml.dset = getOMLDataSet(clas[24,]$data.id)
task = convertOMLDataSetToMlr(oml.dset)
type = getTaskType(task)
target = task$task.desc$target
data = droplevels(oml.dset$data) # drop not existing levels
if(is.factor(data[, target]) == FALSE)
  data[, target] = as.factor(data[, target])
mod = randomForest(as.formula(paste0(target, "~.")), data = data, ntree = 100000, nodesize = 1)
preds = predict(mod, type = "prob")
cols = data[,target] == 0
probs = c(preds[which(cols == TRUE), 1], preds[which(cols == FALSE), 2])
hist(probs, main = "Histogramm of Probabilities", xlab = "Expected Probabilities for correct classification")

# Dataset 36
mean_oob = apply(res[[36]], 2, mean)
plot(mean_oob, type = "l", main = "Mean of OOB-MMCE of 1000 Random Forests on Dataset 36", xlab = "ntree", ylab = "oob mmce")

oml.dset = getOMLDataSet(clas[36,]$data.id)
task = convertOMLDataSetToMlr(oml.dset)
type = getTaskType(task)
target = task$task.desc$target
data = droplevels(oml.dset$data) # drop not existing levels
if(is.factor(data[, target]) == FALSE)
  data[, target] = as.factor(data[, target])
mod = randomForest(as.formula(paste0(target, "~.")), data = data, ntree = 100000, nodesize = 1)
preds = predict(mod, type = "prob")
cols = data[,target] == "P"
probs = c(preds[which(cols == TRUE), 1], preds[which(cols == FALSE), 2])
hist(probs, main = "Histogramm of Probabilities", xlab = "Expected Probabilities for correct classification")

par(mfrow = c(1,3))
# Dataset 111
mean_oob = apply(res[[111]], 2, mean)
plot(mean_oob, type = "l", main = "Mean of OOB-MMCE of 1000 Random Forests on Dataset 111", xlab = "ntree", ylab = "oob mmce")

oml.dset = getOMLDataSet(clas[111,]$data.id)
task = convertOMLDataSetToMlr(oml.dset)
type = getTaskType(task)
target = task$task.desc$target
data = droplevels(oml.dset$data) # drop not existing levels
if(is.factor(data[, target]) == FALSE)
  data[, target] = as.factor(data[, target])
mod = randomForest(as.formula(paste0(target, "~.")), data = data, ntree = 10000, nodesize = 1)
preds = predict(mod, type = "prob")
cols = data[,target] == "P"
probs = c(preds[which(cols == TRUE), 1], preds[which(cols == FALSE), 2])
hist(probs, main = "Histogramm of Probabilities", xlab = "Expected Probabilities for correct classification")
hist(probs[probs > 0.1 & probs < 0.9], main = "Histogramm of Probabilities for probabilites scaled on [0.1,0.9]", xlab = "Expected Probabilities for correct classification")
# wie erwartet
