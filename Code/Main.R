args = commandArgs(trailingOnly=TRUE)

n.train <-  500
n.test <- 200
p <- 10
q <- 3
# pi is the ratio between treatment groups
pi <- 0.5

# source("Simulation.R")        # Simulate training and testing datasets
# source("buildForest.R")
# source("predict.R")


library(MOTTE.RF)
#Simulate data
sim.dat <- sim_MOTTE_data(n.train=n.train, n.test=n.test, p=p, q=q, pi=pi)

# str(tmp)

# Training models

exhaust <- build_MOTTE_forest(
  x.b =sim.dat$train$x.b , x.e = sim.dat$train$x.e,
  treat = sim.dat$train$treat,
  y.b = sim.dat$train$y.b, y.e = sim.dat$train$y.e)


# TODO: CONTINUE HERE
print("Finish exhaustive tree")

forest <- buildForest(
  x.b =X.train.base , x.e = X.train.end,
  treat = Treat.train,
  y.b = Y.train.base, y.e = Y.train.end,
  nsplits=1,ntree=200, nodesize=2)

print("Finish forest tree")

# Training Qian and Susam Method
source("Code/susan_method.R")

# Training Loh's method
source("Loh_method.R")

print("Finish fitting model")

### Classification error
#

# each column is a weight vector
weight <- rep(0.3,6)

true.recom <- ifelse(Y.test.case.end%*%weight>Y.test.control.end%*%weight,1,0)
exhaust.recom <- recommendResult(exhaust, X.test.base, weight)
forest.recom <- recommendResult(forest, X.test.base, weight)
susan.recom <- ifelse(susan.treat.pred[,,1]%*%weight > susan.untreat.pred[,,1]%*%weight,1,0)
loh.recom <- ifelse(as.matrix(test.loh.treat.node.res[,5:10])%*%weight>as.matrix(test.loh.untreat.node.res[,5:10])%*%weight,1,0)

# Classification error
exhaust.err.tab <- table(exhaust.recom,true.recom)
exhaust.err <- sum(exhaust.recom!=true.recom)/n.test
forest.err.tab <- table(forest.recom,true.recom)
forest.err <- sum(forest.recom!=true.recom)/n.test
susan.err.tab <- table(susan.recom,true.recom)
susan.err <- sum(susan.recom!=true.recom)/n.test
loh.err.tab <- table(loh.recom, true.recom)
loh.err <- sum(loh.recom!=true.recom)/n.test

eweight.err <- #t(
  c(exhaust.err, forest.err, susan.err, loh.err)
#)

weight <- matrix(runif(n.test*q,-1,1),nrow=n.test)


# Each column represents the classification error of each methods corresponding to each weight

  true.recom <- ifelse(diag(tcrossprod(Y.test.case.end,weight))>diag(tcrossprod(Y.test.control.end,weight)),1,0)
  exhaust.recom <- recommendResult(exhaust, X.test.base, weight)
  forest.recom <- recommendResult(forest, X.test.base, weight)
  susan.recom <- ifelse(diag(tcrossprod(susan.treat.pred[,,1],weight)) > diag(tcrossprod(susan.untreat.pred[,,1],weight)),1,0)
  loh.recom <- ifelse(diag(tcrossprod(as.matrix(test.loh.treat.node.res[,5:10]),weight))>diag(tcrossprod(as.matrix(test.loh.untreat.node.res[,5:10]),weight)),1,0)
  
  # Classification error
  exhaust.err.tab <- table(exhaust.recom,true.recom)
  exhaust.err <- sum(exhaust.recom!=true.recom)/n.test
  forest.err.tab <- table(forest.recom,true.recom)
  forest.err <- sum(forest.recom!=true.recom)/n.test
  susan.err.tab <- table(susan.recom,true.recom)
  susan.err <- sum(susan.recom!=true.recom)/n.test
  loh.err.tab <- table(loh.recom, true.recom)
  loh.err <- sum(loh.recom!=true.recom)/n.test
  
  
  rweight.err <- #t(
    c(exhaust.err, forest.err, susan.err, loh.err)
    #)
  
#  return(write.out)
# })

err.output <-c(eweight.err, rweight.err)

write.table(t(err.output), file="Result/classError.csv",append=TRUE,col.names=F)
