args = commandArgs(trailingOnly=TRUE)

# source("Simulation.R")        # Simulate training and testing datasets
# source("buildForest.R")
# source("predict.R")


library(MOTTE.RF)
# Training models

exhaust <- buildForest(
  x.b =X.train.base , x.e = X.train.end,
  treat = Treat.train,
  y.b = Y.train.base, y.e = Y.train.end)

print("Finish exhaustive tree")

# Training Qian and Susam Method
source("susan_method.R")

# Training Loh's method
source("Loh_method.R")

print("Finish fitting model")

### Classification error
#

# each column is a weight vector
weight <- rep(0.3,3)
rweights <- matrix(runif(30,0,1),nrow=3,ncol=10)
weights <- cbind(weight,rweights)

# Each column represents the classification error of each methods corresponding to each weight
err.tab <- apply(weights, 2, FUN=function(x){
  true.recom <- ifelse(Y.test.case.end%*%x>Y.test.control.end%*%x,1,0)
  exhaust.recom <- recommendResult(exhaust, X.test.base, x)
  susan.recom <- ifelse(susan.treat.pred[,,1]%*%x > susan.untreat.pred[,,1]%*%x,1,0)
  loh.recom <- ifelse(as.matrix(test.loh.treat.node.res[,5:7])%*%x>as.matrix(test.loh.untreat.node.res[,5:7])%*%x,1,0)
  
  # Classification error
  exhaust.err.tab <- table(exhaust.recom,true.recom)
  exhaust.err <- sum(exhaust.recom!=true.recom)/n.test
  susan.err.tab <- table(susan.recom,true.recom)
  susan.err <- sum(susan.recom!=true.recom)/n.test
  loh.err.tab <- table(loh.recom, true.recom)
  loh.err <- sum(loh.recom!=true.recom)/n.test
  
  
  write.out <- #t(
    c(exhaust.err, susan.err,loh.err)
    #)
  
  return(write.out)
})

equal.weight.err <- err.tab[,1]
ave.rweight.err <- rowMeans(err.tab[,-1])

err.output <-c(equal.weight.err, ave.rweight.err)

write.table(t(err.output), file="Result/classError.csv",append=TRUE,col.names=F)