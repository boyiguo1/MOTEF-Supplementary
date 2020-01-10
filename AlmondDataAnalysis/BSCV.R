# Treatment Ratio
r <- sum(Treatment==1)/length(Treatment)

# Bootstrap
BS.result <- matrix(nrow=200,ncol=4)

for(i in 1:200)
{
  # Case control ratio
  r.BS <- sample(c(1,0), size=length(Treatment), replace = T,prob=c(r,1-r))
  BS.indices <- c(
    sample(which(Treatment==1),size = sum(r.BS),replace=T),
    sample(which(Treatment==0),size = length(r.BS)-sum(r.BS), replace=T)
  )
  
  baseline.BS <- baseline.data[BS.indices,]
  end.BS <- end.data[BS.indices,]
  Treatment.BS <- Treatment[BS.indices]
  
  rownames(baseline.BS) <- rownames(end.BS) <- 1:nrow(baseline.BS)
  
  if(sum( (baseline.BS[,"Subject"]!=end.BS[,"Subject"])&
          (baseline.BS[,"Period"]!=end.BS[,"Period"]) ) !=0)
    stop("Unmatched Samples")
  
  nfold <- 10
  
  cv.index <- c(
    rep_len(1:nfold,length.out=sum(Treatment.BS==1)),
    rep_len(1:nfold,length.out=length(Treatment.BS)-sum(Treatment.BS==1))
  )
  
  cv.result <- matrix(nrow=nfold,ncol=4) 
  for(j in 1:nfold)
  {
    baseline.train <- baseline.BS[cv.index!=j, ]
    end.train <- end.BS[cv.index!=j, ]
    Treatment.train <- Treatment.BS[cv.index!=j ]
    
    baseline.test <- baseline.BS[cv.index==j,]
    end.test <- end.BS[cv.index==j, ]
    Treatment.test <- Treatment.BS[cv.index==j]
    
    
    ###############################################################
    #
    #  Training
    #
    ###############################################################
    
    
    ###############################################################
    #
    #   MedTree And MedForest
    #
    ###############################################################
    source("buildForest.R")
    source("predict.R")
    
    MedTree <- buildForest(
      x.b = data.matrix(baseline.train[,order.data.name]),
      x.e = data.matrix(end.train[,order.data.name]),
      treat = as.vector(Treatment.train),
      y.b = data.matrix(baseline.train[,Y.vars]),
      y.e = data.matrix(end.train[,Y.vars]),
      nodesize = 10)
    
    
    MedForest <- buildForest(
      x.b = data.matrix(baseline.train[,order.data.name]),
      x.e = data.matrix(end.train[,order.data.name]),
      treat = as.vector(Treatment.train),
      y.b = data.matrix(baseline.train[,Y.vars]),
      y.e = data.matrix(end.train[,Y.vars]),
      nsplits=10, ntree=200, nodesize=10)
    
    ###############################################################
    #
    #   L1
    #
    ###############################################################   
    
    library(glmnet)
    # Constructing data with interaction term
    dat <- data.frame( x = baseline.train[,order.data.name], Treat = Treatment.train)
    f <- as.formula(~(.-Treat)*Treat)
    x <- model.matrix(f, dat)
    
    cv.res <- cv.glmnet(x,as.matrix(end.train[,Y.vars]),family="mgaussian", standardize=T, intercept=T)
    glm.res <- glmnet(x,as.matrix(end.train[,Y.vars]),family="mgaussian", lambda = cv.res$lambda.min, intercept=T)
    
    ###############################################################
    #
    #   Data Prep for Loh's Method
    #
    ###############################################################
    
    train.lab <- c(rep(1,nrow(baseline.train)),
                   rep(0,2*nrow(baseline.test)))
    
    # tmp <- mvrnorm(nrow(baseline.train),
    #         rep(0,length(order.data.name)),
    #         diag(1,length(order.data.name),length(order.data.name)))
    # colnames(tmp) <- order.data.name
    
    Xs <- rbind(baseline.train[,order.data.name],
                baseline.test[,order.data.name],
                baseline.test[,order.data.name])
    
    tmp <- matrix(NA,nrow=2*nrow(baseline.test),ncol=length(Y.vars))
    colnames(tmp) <- Y.vars
    
    Ys <- rbind(end.train[,Y.vars],
                tmp)
    
    Treat.all <- c(as.numeric(as.character(Treatment.train)),rep(1,nrow(baseline.test)),rep(0,nrow(baseline.test)))
    all.data <- cbind(train.lab, Treat.all, Xs, Ys)
    #colnames(all.data) <- c("Train","Treated",paste0("X",1:p),paste0("Y",1:q))
    # Save data for Loh's method
    write.csv(all.data,"Real_data.csv",row.names=F)
    
    
    ###############################################################
    #
    #   Loh's Method
    #
    ###############################################################
    shell("guide < RD_Setting.in")
    
    
    
    ###############################################################
    #
    #  Predicting and Recommendation
    #
    ###############################################################    
    
    eWeight <- rep(0.1,10)
    
    ###############################################################
    #
    #   MedTree And MedForest
    #
    ###############################################################
    
    exhaust.recom <- recommendResult(MedTree, data.matrix(baseline.test[,order.data.name]), eWeight)
    forest.recom <- recommendResult(MedForest, data.matrix(baseline.test[,order.data.name]), eWeight)
    
    ###############################################################
    #
    #   L1
    #
    ###############################################################     
    test.treat <- data.frame(x=baseline.test[,order.data.name], Treat=rep(1,nrow(baseline.test)))
    test.untreat <- data.frame(x=baseline.test[,order.data.name], Treat=rep(0,nrow(baseline.test)))
    
    x.test.treat <- model.matrix(f, test.treat)
    x.test.untreat <- model.matrix(f, test.untreat)
    susan.treat.pred <- predict(glm.res, x.test.treat)
    susan.untreat.pred <- predict(glm.res, x.test.untreat)    
    
    susan.recom <- ifelse(susan.treat.pred[,,1]%*%eWeight > susan.untreat.pred[,,1]%*%eWeight,1,0)
    
    
    ###############################################################
    #
    #   Loh
    #
    ############################################################### 
    
    # Loh Predicting
    # Model result parsing
    node.reg.res <- read.table("fitted.res",header=T)
    test.res.node <- read.table("node.res",header=T)
    # Node information of prediction if with treatment
    test.loh.treat.node <- test.res.node[(nrow(baseline.train)+1):(nrow(baseline.train)+nrow(baseline.test)),]
    # Node information of prediction if without treatment
    test.loh.untreat.node <- test.res.node[(nrow(baseline.train)+nrow(baseline.test)+1):(nrow(baseline.train)+2*nrow(baseline.test)),]
    
    Treat.all<-rep(1,nrow(baseline.test))
    test.loh.treat.node <- cbind(test.loh.treat.node,Treat.all)
    Treat.all<-rep(0,nrow(baseline.test))
    test.loh.untreat.node <- cbind(test.loh.untreat.node,Treat.all)
    
    # Complete prediction when given treatment
    test.loh.treat.node.res <- merge(test.loh.treat.node,node.reg.res,
                                     by=c("node","Treat.all"),sort=FALSE)
    # Reorder data by case id
    test.loh.treat.node.res <- test.loh.treat.node.res[order(test.loh.treat.node.res$case),]
    
    # Complete prediction when treatment is not given
    test.loh.untreat.node.res <- merge(test.loh.untreat.node,node.reg.res,
                                       by=c("node","Treat.all"),sort=FALSE)
    # Reorder data by case id
    test.loh.untreat.node.res <- test.loh.untreat.node.res[order(test.loh.untreat.node.res$case),]
    
    loh.recom <- ifelse(as.matrix(test.loh.treat.node.res[,5:14])%*%eWeight > 
                          as.matrix(test.loh.untreat.node.res[,5:14])%*%eWeight,
                        1,0)
    cv.result[j,] <- c(
      (colMeans(end.test[exhaust.recom==Treatment.test,Y.vars])-colMeans(end.test[,Y.vars]))%*%eWeight,
      (colMeans(end.test[forest.recom==Treatment.test,Y.vars])-colMeans(end.test[,Y.vars]))%*%eWeight,
      (colMeans(end.test[susan.recom==Treatment.test,Y.vars])-colMeans(end.test[,Y.vars]))%*%eWeight,
      (colMeans(end.test[loh.recom==Treatment.test,Y.vars])-colMeans(end.test[,Y.vars]))%*%eWeight
    )
    BS.result[i,]<- colMeans(cv.result)
  }
}