#args = commandArgs(trailingOnly=TRUE)

n.train <-  500
n.test <- 200
p <- 10
q <- 3
# pi is the ratio between treatment groups
pi <- 0.5



library(MOTTE.RF)
library(glmnet)

#Simulate data
# TODO: change the set up in the future
set.seed(1)
B <- create.B(10)
Z <- create.Z(10, 3)
sim.dat <- sim_MOTTE_data(n.train=n.train, n.test=n.test, p=p, q=q, ratio=pi,
                          B = B, Z = Z)
# Organize data by standardize
train.dat <- sim.dat$train

x.b <- scale(train.dat$x.b, center = FALSE, scale = TRUE)
x.e <- scale(train.dat$x.e, center = FALSE, scale = TRUE)
y.b <- scale(train.dat$y.b, center = FALSE, scale = TRUE)
y.e <- scale(train.dat$y.e, center = FALSE, scale = TRUE)
treat <- train.dat$trt


# scaling test.dat
test.dat <- sim.dat$test
test.x.b <- scale(test.dat$x.b, center = F, scale = attr(x.b, "scale"))

test.x.b <- scale(test.dat$x.b, center = F, scale = attr(x.b, "scale"))
test.x.b <- scale(test.dat$x.b, center = F, scale = attr(x.b, "scale"))

true.trt.diff <- scale(test.dat$y.e.case, center = F, scale = attr(y.e, "scale")) -
    scale(test.dat$y.e.control, center = F, scale = attr(y.e, "scale"))

# Fit Extreme RF
extm.mdl<- build_MOTTE_forest(x.b, x.e, treat, y.b, y.e,
                              nsplits = 1)
# TODO: write the predict function

# Fit RF based on \mu X.b
RF.mdl <- build_MOTTE_forest(x.b, x.e, treat, y.b, y.e,
                             nsplits = 50)

# Fit Tree based on \mu X.b
tree.mdl <- build_MOTTE_forest(x.b, x.e, treat, y.b, y.e,
                              nsplits = NULL)

tmp <- traverseForest(tree.mdl, test.x.b[1,,drop=F])

# Constructing data with interaction term
dat <- data.frame( x = x.b, Treat = treat)
f <- as.formula(~(.-Treat)*Treat)
x <- model.matrix(f, dat)

cv.res <- cv.glmnet(x, y.e,family="mgaussian", standardize=T, intercept=T)
glm.res <- glmnet(x,y.e,family="mgaussian", lambda = cv.res$lambda.min, intercept=T)

# TODO change the name
test.treat <- data.frame(x=test.x.b, Treat=rep(levels(treat)[1],n.test))
test.untreat <- data.frame(x=test.x.b, Treat=rep(levels(treat)[2],n.test))

levels(test.treat$Treat) <- levels(test.untreat$Treat) <- levels(treat)

x.test.treat <- model.matrix(f, test.treat)
x.test.untreat <- model.matrix(f, test.untreat)
susan.treat.pred <- predict(glm.res, x.test.treat)
susan.untreat.pred <- predict(glm.res, x.test.untreat)

susan.treat.diff <- susan.treat.pred - susan.unstreat.pred








# Generating Rdata file for GUIDE
# TODO: Need to work on this section
# train.lab variable in this file is for the prediction purpose for Loh's method
train.lab <- c(rep(1,n.train),rep(0, 2*n.test))
Xs <- rbind(X.train.base, X.test.base, X.test.base)
Ys <- rbind(Y.train.end, matrix(NA,nrow=2*n.test,ncol=ncol(Y.train.end)))
Treat.all <- c(Treat.train,rep(1,n.test),rep(0,n.test))
all.data <- cbind(train.lab, Treat.all, Xs, Ys)
colnames(all.data) <- c("Train","Treated",paste0("X",1:p),paste0("Y",1:q))
# Save data for Loh's method
write.csv(all.data,paste0(args[1],"input_data.rdata"),row.names=F)

# str(tmp)

# Training models

exhaust <- build_MOTTE_forest(
  x.b =sim.dat$train$x.b , x.e = sim.dat$train$x.e,
  treat = sim.dat$train$treat,
  y.b = sim.dat$train$y.b, y.e = sim.dat$train$y.e)



print("Finish exhaustive tree")
# TODO: ERROR here DeBug
forest <- build_MOTTE_forest(
  x.b =sim.dat$train$x.b , x.e = sim.dat$train$x.e,
  treat = sim.dat$train$treat,
  y.b = sim.dat$train$y.b, y.e = sim.dat$train$y.e,
  nsplits=2,ntree=4, nodesize=2)

print("Finish forest tree")

# Training Qian and Susam Method
source("../MOTTE.RF.Simulation/Code/susan_method.R")

# Training Loh's method
source("../MOTTE.RF.SimulationCode/Loh_method.R")

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
