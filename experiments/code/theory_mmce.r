# For the paper

# a function to compute P(X>0.5*T) + 0.5*P(X=0.5*T)
oobi = function(T, pi) {
  if (T %% 2 == 1) { # is T an odd number?
    return(pbinom(floor(0.5*T), size = T , prob = pi, lower.tail = FALSE))
  } else {
    return(pbinom(floor(0.5*T), size = T , prob = pi, lower.tail = FALSE) + 0.5 *dbinom(floor(0.5*T), size = T, prob = pi))
  }
}

# a function to plot E(OOB(t)) for different number of trees t (included in vector Tvec) for different pi's (included in vector pivec)
plotoob = function(Tvec, pivec, adj = 1, colores) {
  mat = matrix(NA, length(pivec), length(Tvec))
  plot(c(0, max(Tvec)), c(0, 1), type = "n", xlab = "number of trees", ylab = expression('E(e'[i]*'(T))'))
  for (i in 1:length(pivec)) {
    y = numeric(length(Tvec))
    for (j in 1:length(Tvec)) {
      mat[i, j] = oobi(round(Tvec[j]*adj) , pivec[i])
    }
    legend(450, 0.95, legend = c(expression(epsilon[i]), pivec), col = c("white", colores), lty = 1, bg = "white")
    lines(Tvec, mat[i, ], col = colores[i])
  }
  return(mat)
}

pdf(file = paste0("/home/probst/Paper/Ntree_RandomForest/Paper/", "error_curves.pdf"), width = 12, height = 7)
par(mfrow = c(1,1))
pivec = rev(seq(0.05,0.95,by=0.05))
library(RColorBrewer)
colores = c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 11, name = "Paired"))
mat_rf = plotoob(Tvec = seq(1, 500, 1), pivec=pivec, adj = 1, colores = colores)
dev.off()

# Wie sieht es bei n = 10000 aus?
pivec = rev(seq(0.495,0.505,by=0.001))
library(RColorBrewer)
colores = c(brewer.pal(n = 8, name = "Dark2"), brewer.pal(n = 3, name = "Paired"))
mat_rf = plotoob(Tvec = seq(1, 10000, 5), pivec=pivec, adj = 1, colores = colores)


# Anhang

# a function to compute P(X>0.5*T) + 0.5*P(X=0.5*T)
oobi <- function(T, pi) {
  if (T %% 2 == 1) { # is T an odd number?
    return(pbinom(floor(0.5*T), size = T , prob = pi, lower.tail = FALSE))
  } else {
    return(pbinom(floor(0.5*T), size = T , prob = pi, lower.tail = FALSE) + 0.5 *dbinom(floor(0.5*T), size = T, prob = pi))
  }
}

# a function to compute P(X>0.5*T)
# oobi <- function(T, pi) {
#   return(pbinom(round(0.5*T), size = T , prob = pi, lower.tail = FALSE))
# }


# a function to plot E(OOB(t)) for different number of trees t (included in vector Tvec) for different pi's (included in vector pivec)
plotoob <- function(Tvec, pivec, adj = 1) {
  mat <- matrix(NA, length(pivec), length(Tvec))
  plot(c(0, max(Tvec)), c(0, 1), type = "n")
  for (i in 1:length(pivec)) {
    y <- numeric(length(Tvec))
    for (j in 1:length(Tvec)) {
      mat[i, j] <- oobi(round(Tvec[j]*adj) , pivec[i])
    }
    points(Tvec, mat[i, ], type = "l", col = i)
  }
  return(mat)
}

mat <- plotoob(Tvec = c(20,30,40,50,60,70,80,90,100,125,150,175,200,250,300,350,400,450,500),pivec=seq(0.1,0.9,by=0.05))

# look at E(OOB(t)) for different mixtures of pi's, e.g. 10% with pi=0.6 and 90% with pi=0.4
# (in reality each observation will have its different pi)
plot(c(20,30,40,50,60,70,80,90,100,125,150,175,200,250,300,350,400,450,500),
     0.9*mat[7,]+0.1*mat[11,],type="l",ylim=c(0,1))




# Philipp
library(OpenML)
task = getOMLTask(task.id = 3595, verbosity=0)
library(randomForestSRC)
mod = rfsrc(binaryClass ~., data = task$input$data.set$data, ntree = 100000)
preds = mod$predicted.oob

cols = task$input$data.set$data$binaryClass == "P"
probs = c(preds[which(cols == TRUE), 1], preds[which(cols == FALSE), 2])

mat <- plotoob(Tvec = seq(1, 2000, 1), pivec = probs, adj = exp(-1)) # Adjustiert um die "durchschnittliche Anzahl" an Trainingsbäumen für jede Beob.
means = 1 - apply(mat, 2, mean)

plot(seq(1,2000, 1), means,type="l",ylim=c(0.13, 0.15), main = "randomForestSRC")

run = matrix(NA, 1000, 2000)  
set.seed(102)
for(i in 1:1000){
  print(paste(i))
  run[i,] = rfsrc(binaryClass ~., data = task$input$data.set$data, ntree = 2000, importance="none", mtry=2, nodesize = 1, tree.err = T)$err.rate[,1]
}
runs = apply(run, 2, mean)

lines(1:2000, runs, col = "red")

# Unterschied liegt daran, dass nicht gleich viele Bäume trainiert werden für jede Beobachtung.

# other task
task = getOMLTask(task.id = 3791, verbosity=0) 
library(randomForest)
mod = randomForest(binaryClass ~., data = task$input$data.set$data, ntree = 300000)
preds = predict(mod, type = "prob")

cols = task$input$data.set$data$binaryClass == "P"
probs = c(preds[which(cols == TRUE), 1], preds[which(cols == FALSE), 2])

mat <- plotoob(Tvec = seq(1, 2000, 1), pivec = probs, adj =  exp(-1))
means = 1 - apply(mat, 2, mean)

plot(seq(1,2000, 1), means,type="l",ylim=c(0.068, 0.085), main = "randomForest")

run2 = matrix(NA, 1000, 2000)  
set.seed(101)
for(i in 1:1000){
  print(paste(i))
  run2[i,] = randomForest(binaryClass ~., data = task$input$data.set$data, ntree = 2000, mtry=2, nodesize = 1)$err.rate[,1]
}
runs2 = apply(run2, 2, mean)
lines(1:2000, runs2, col = "red")


# Zacken
pi = 0.4
for (T in 1:10){ 
  print("________________________________")
  print(round(0.5*T)/T)
  print(pbinom(round(0.5*T), size = T , prob = pi, lower.tail = FALSE))
}

# Bestätigung des Restes mit randomForestSRC
library(OpenML)
task = getOMLTask(task.id = 3595, verbosity=0)
samples = sample(1:43, 35)
set.seed(123)
train = task$input$data.set$data[samples,]
test = task$input$data.set$data[!(1:43 %in% samples),]
library(randomForestSRC)
mod_100000 = rfsrc(binaryClass ~., data = train, ntree = 100000)
probs = predict(mod_100000, newdata = test)$predicted
cols = test$binaryClass == "P"
probs = c(probs[which(cols == TRUE), 1], probs[which(cols == FALSE), 2])

set.seed(123)
sequenz = seq(30, 100, 25)
preds = numeric(length(sequenz))
for(i in 1:length(sequenz)){
  for(k in 1:5000) {
    print(paste(i, k))
    mod = rfsrc(binaryClass ~., data = train, ntree = sequenz[i])
    preds[i] = preds[i] + mean(as.numeric(predict(mod, newdata = test, type = "response")$class == test$binaryClass))
  }
}
preds = preds/5000

mat <- plotoob(Tvec = seq(1, 200, 1), pivec=probs, adj = 1)
means = 1 - apply(mat, 2, mean)
plot(means, type = "l")
points(sequenz, 1 - preds, col = "red")
points(sequenz, means[sequenz], col = "green")
# Differenz relativ groß!!




# Bestätigung des Restes mit randomForest
library(OpenML)
task = getOMLTask(task.id = 3595, verbosity=0)
test_samples = c(10, 16, 22, 27, 32, 35, 39, 40)
train_samples = which(!(1:43 %in% test_samples))
set.seed(123)
train = task$input$data.set$data[train_samples, ]
test = task$input$data.set$data[test_samples, ]

library(randomForest)
mod_1000000_rf = randomForest(binaryClass ~., data = train, ntree = 1000000)
probs_rf = predict(mod_1000000_rf, newdata = test[, -4], type = "prob")
cols = test$binaryClass == "P"
probs_rf = c(probs_rf[which(cols == TRUE), 1], probs_rf[which(cols == FALSE), 2])

set.seed(128)
sequenz_rf = seq(31, 100, 7)
preds_rf = numeric(length(sequenz_rf))
for(i in 1:length(sequenz_rf)){
  for(k in 1:10000) {
    print(paste(i, k))
    mod = randomForest(binaryClass ~., data = train, ntree = sequenz_rf[i])
    preds_rf[i] = preds_rf[i] + mean(as.numeric(predict(mod, newdata = test[-4], type = "response") == test$binaryClass))
  }
}
preds_rf = preds_rf/10000


# a function to compute P(X>0.5*T)
oobi <- function(T, pi) {
  if (T %% 2 == 1) { # is T an odd number?
    return(pbinom(floor(0.5*T), size = T , prob = pi, lower.tail = FALSE))
  } else {
    return(pbinom(floor(0.5*T), size = T , prob = pi, lower.tail = FALSE) + 0.5 *dbinom(floor(0.5*T), size = T, prob = pi))
  }
}

# a function to plot E(OOB(t)) for different number of trees t (included in vector Tvec) for different pi's (included in vector pivec)
plotoob <- function(Tvec, pivec, adj = 1) {
  mat <- matrix(NA, length(pivec), length(Tvec))
  plot(c(0, max(Tvec)), c(0, 1), type = "n")
  for (i in 1:length(pivec)) {
    y <- numeric(length(Tvec))
    for (j in 1:length(Tvec)) {
      mat[i, j] <- oobi(round(Tvec[j]*adj) , pivec[i])
    }
    points(Tvec, mat[i, ], type = "l", col = i)
  }
  return(mat)
}

mat_rf <- plotoob(Tvec = seq(1, 200, 1), pivec=probs_rf, adj = 1)

means_rf = 1 - apply(mat_rf, 2, mean)
plot(means_rf, type = "l")
points(sequenz_rf, 1 - preds_rf, col = "red")
points(sequenz_rf, means_rf[sequenz_rf], col = "green")
















# alt
library(OpenML)
task = getOMLTask(task.id = 3595, verbosity=0) 
library(randomForestSRC)
mod = rfsrc(binaryClass ~., data = task$input$data.set$data, ntree = 100000)
preds = mod$predicted.oob

probs = preds[27,2]

data = task$input$data.set$data

preds = matrix(0, nrow(data), 2)
set.seed(123)
i = 27
for(k in 1:1000) {
  print(k)
  #for(i in 1:nrow(data)) {
  #print(paste(k, i))
  mod = rfsrc(binaryClass ~., data = data[-i,], ntree = 101)
  preds[i,] = preds[i,] + as.numeric(predict(mod, newdata = data[i,], type = "response")$class == levels(task$input$data.set$data$binaryClass))
  #print(as.character(predict(mod, newdata = data[i,], type = "response")$class) )
  #  
  print(preds[i,])
  #}
}
preds = preds / 1000


plotoob(Tvec = seq(1, 200, 1),pivec=probs, adj = 1)
points(101, 1- preds_alt[27,1])

