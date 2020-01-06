current <- paste0(getwd(),"/")
setwd(paste0(current,args[1]))

# Run Loh's model
system("./guide < Loh_Setting.in > log.txt")

# Model result parsing
node.reg.res <- read.table("fitted.res",header=T)
test.res.node <- read.table("node.res",header=T)
# Node information of prediction if with treatment
test.loh.treat.node <- test.res.node[(n.train+1):(n.train+n.test),]
# Node information of prediction if without treatment
test.loh.untreat.node <- test.res.node[(n.train+n.test+1):(n.train+2*n.test),]

Treated<-rep(1,n.test)
test.loh.treat.node <- cbind(test.loh.treat.node,Treated)
Treated<-rep(0,n.test)
test.loh.untreat.node <- cbind(test.loh.untreat.node,Treated)

# Complete prediction when given treatment
test.loh.treat.node.res <- merge(test.loh.treat.node,node.reg.res,
                           by=c("node","Treated"),sort=FALSE)
# Reorder data by case id
test.loh.treat.node.res <- test.loh.treat.node.res[order(test.loh.treat.node.res$case),]

# Complete prediction when treatment is not given
test.loh.untreat.node.res <- merge(test.loh.untreat.node,node.reg.res,
                                 by=c("node","Treated"),sort=FALSE)
# Reorder data by case id
test.loh.untreat.node.res <- test.loh.untreat.node.res[order(test.loh.untreat.node.res$case),]

setwd(current)
