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
  diffs2[i] = min(mean_oob[201:2000]) - min(mean_oob[1:200])
}
boxplot(diffs)
sum(diffs > 0.005)

bigger = function(x) {
  sum(diffs > x)
}
plot(seq(0, 0.05, 0.001), sapply(seq(0, 0.05, 0.001), bigger), type = "l", xlab = "threshold", ylab = "number of differences bigger than threshold")

boxplot(diffs2)
sum(diffs2 > 0)
sum(diffs2 < 0)
sum(diffs2[1:100] > 0)
sum(diffs2[1:100] < 0)
sum(diffs2[101:193] > 0)
sum(diffs2[101:193] < 0)
# Bei kleinen Datensätzen etwas häufiger
# in ca. 25 % der Fälle

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
# Hier tritt das Phaenomaen gar nicht auf
