args=(commandArgs(TRUE))

ind <- diag
AR <- function(x){
  t <- 1:x
  return(0.8^abs(outer(t,t, "-")))
}


if(length(args)==0){
  print("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}


# Required Library
library(MOTE.RF)
library(glmnet)
library(tidyverse)
library(randomForest)

  
pi <- 0.5 # pi is the ratio between treatment groups
it <- Sys.getenv('SLURM_ARRAY_TASK_ID') %>% as.numeric
paste0("This is Run", it, "\n") %>% cat

set.seed(it)
B <- create.B(p, intercept=T)
Z <- create.Z(p, q)
sim.dat <- sim_MOTE_data(n.train=n.train, n.test=n.test, p=p, q=q, ratio=pi,
                         trt.f = c("Linear", "Polynomial", "Box")[trt.f],
                         link.f = c("Linear", "Polynomial")[link.f],
                         cov.mat = sigma(p),
                         B=B, Z = Z)

# Organize data by standardize
train.dat <- sim.dat$train
x.b <- train.dat$x.b
x.e <- train.dat$x.e
y.b <- train.dat$y.b
y.e <- train.dat$y.e
treat <- train.dat$trt


test.dat <- sim.dat$test
test.x.b <- test.dat$x.b
true.trt.diff <- test.dat$y.e.2 -  test.dat$y.e.1

###### MOTEF Model #####
MOTE.fit.time <- system.time({

  RF.mdl <- MOTE(x.b = x.b, x.e = x.e,   # Fit MOTEF
                 treat = treat,
                 y.b = y.b, y.e = y.e,
                 num.trees = 200,
                 num.random.splits = 10,
                 num.threads = 1,
                 oob.error = FALSE,
                 seed = as.numeric(it), 
                 verbose=F)})

MOTE.size <- object.size(RF.mdl)


MOTE.predict.time <- system.time({
  RF.mdl.trt.diff <- predict(RF.mdl, test.x.b)
})


###### l1 penalized model #####
# prepare fitting data
dat <- data.frame(x.b, Treat = treat)
f <- as.formula(~(.-Treat)*Treat)
susan.x <- model.matrix(f, dat)

l1.fit.time <- system.time({
  cv.res <- cv.glmnet(susan.x, y.e, family="mgaussian", standardize=T, intercept=T)
  glm.res <- glmnet(susan.x, y.e, family="mgaussian", lambda = cv.res$lambda.min, intercept=T)
})

l1.size <- object.size(glm.res)

test.treat <- data.frame(test.x.b,
                         Treat=rep(levels(treat)[1],n.test) %>% factor(levels = levels(treat)))
test.untreat <- data.frame(test.x.b,
                           Treat=rep(levels(treat)[2],n.test) %>% factor(levels = levels(treat)))

x.test.treat <- model.matrix(f, test.treat)
x.test.untreat <- model.matrix(f, test.untreat)

l1.predict.time <- system.time({
  susan.treat.pred <- predict(glm.res, x.test.treat)
  susan.untreat.pred <- predict(glm.res, x.test.untreat)
  susan.treat.diff <- (susan.untreat.pred - susan.treat.pred) %>% data.frame
})



###### Marginal RF #####
test.treat <- data.frame(test.x.b,
                         Treat=rep(levels(treat)[1],n.test) %>% factor(levels = levels(treat)))
test.untreat <- data.frame(test.x.b,
                           Treat=rep(levels(treat)[2],n.test) %>% factor(levels = levels(treat)))
x.test.treat <- model.matrix(f, test.treat)
x.test.untreat <- model.matrix(f, test.untreat)

RF.fit.time <- 0
RF.pred.time <- 0
RF.size <- 0

margin.RF <- matrix(NA, nrow = n.test, ncol = q)

for(c in 1:q){
  y1 <- y.e[,c]
  
  tmp.fit.time <- system.time({
    mod <- randomForest(x = susan.x, y = y1, ntree=200)
  })
  
  RF.size <- RF.size + object.size(mod)
  RF.fit.time <- RF.fit.time + tmp.fit.time
  
  tmp.pred.time <- system.time({
    y1.treat1 <- predict(mod, newdata = x.test.treat)
    y1.treat2 <- predict(mod, newdata = x.test.untreat)
    
    ret <- y1.treat2 - y1.treat1
  })
  RF.pred.time <- RF.pred.time + tmp.pred.time
  
  margin.RF[,c] <- ret
}

###### Summarize Simulation Results #####
# Save Prediction Error
sim.res <-  data.frame(
  run = it,
  RF.mdl.MSE = rowSums((RF.mdl.trt.diff$predictions - true.trt.diff)^2) %>% mean,
  RF.mdl.MSE.sd = rowSums((RF.mdl.trt.diff$predictions - true.trt.diff)^2) %>% sd,
  
  susan.MSE = rowSums((susan.treat.diff - true.trt.diff)^2) %>% mean,
  susan.MSE.sd = rowSums((susan.treat.diff - true.trt.diff)^2) %>% sd,
  
  margin.RF.MSE = rowSums((margin.RF - true.trt.diff)^2) %>% mean,
  margin.RF.MSE.sd = rowSums((margin.RF - true.trt.diff)^2) %>% sd
)

job_name <- Sys.getenv('SLURM_JOB_NAME')
saveRDS(sim.res, 
        paste0("/data/user/boyiguo1/MOTE/Res/", job_name,"/it_",it,".rds"))


# Save Running Time & Space
run.time <- data.frame(run = it,
                       MOTE_fit = MOTE.fit.time %>% data.matrix() %>% t,
                       MOTE_pred = MOTE.predict.time %>% data.matrix() %>% t,
                       MOTE_size = MOTE.size  %>% data.matrix() %>% t,
                       l1_fit = l1.predict.time %>% data.matrix() %>% t,
                       l1_pred = l1.predict.time %>% data.matrix() %>% t,
                       l1_size = l1.size %>% data.matrix() %>% t,
                       RF_fit = RF.fit.time %>% data.matrix() %>% t,
                       RF_pred = RF.pred.time %>% data.matrix() %>% t,
                       RF_size = RF.size %>% data.matrix() %>% t,
                       )

saveRDS(run.time, 
        paste0("/data/user/boyiguo1/MOTE/RunTime/", job_name,"/it_",it,".rds"))
