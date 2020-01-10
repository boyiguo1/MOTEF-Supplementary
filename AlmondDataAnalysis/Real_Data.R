###############################################################
#
#   Data Prep
#
###############################################################

# Read In CSV data
data <- read.csv("C:/Users/BOYI/Dropbox/Ruoqing/almond-treatment5.csv",header=TRUE)
# Remove irrelavent variables: SampleID,Reextraction,TrtTime,TrtTime2
data <- subset(data,select=-c(SampleID,Reextraction,TrtTime,TrtTime2))

# Factor variables
var.names <- c("Subject", "Period", "Almond", "WholeAlmond", "RoastedAlmond", "SEX.M", 
               "BMI_BIN", "CRP_BIN", "SAA_BIN", "CHOL_BIN", "HDL_BIN", "LDL_BIN", "TRIG_BIN", "Glu_BIN", 
               "Methanosphaera_BIN", "Bifidobacterium_BIN", "Collinsella_BIN", "Parabacteroides_BIN",
               "Clostridium_BIN", "Lachnospira_BIN", "Roseburia_BIN", "Oscillospira_BIN", "Akkermansia_BIN")
for(name in var.names) data[,name]<-factor(data[,name])

# Invalid Entries
#   Delete the entries that don't have corresponding baselin or end
invalid.indices <- which(data$Subject==4517 & data$Period %in% c(2,4))
data <- data[-invalid.indices,]
clean.data <- data[order(data[,"Subject"],data[,"Period"]),]

# Grep MicroBiome data
bact.indices <- grep("k__",names(clean.data))
z <- clean.data[,bact.indices]
family.index <- 1:41
genus.index <- 42:length(names(z))
family.data <- z[,family.index]
genus.data <- z[,genus.index]

###################### Finding Order Level ############################
#
family.index <- 70:110
tmp <- clean.data[,family.index]
tmp <- tmp[,order(names(tmp))]

tmp.names <- strsplit(names(tmp),split=".f__")
names <- sapply(tmp.names,FUN=function(x) return(x[1]))
uni.name <- unique(names)

order.data <- NULL
order.data.name <- NULL
for(i in uni.name){
  indices <- grep(i,names(tmp))
  order.data.name <- c(order.data.name,i)
  if(length(indices)==1){
    order.data <- cbind(order.data,tmp[,indices])
  } else {
    order.data <- cbind(order.data,rowSums(tmp[,indices]))
  }
}
colnames(order.data) <- order.data.name

#
############################################################################

#Grep Response Variables
# Ys
Y.vars <- c("BMI_kg_m2","IL_6_pg_mL","CRP_ng_mL","SAA_ng_mL","SICAM_1_ng_mL",
            "SVCAM_1_ng_mL","CHOL_mg_dL","dLDL_mg_dL","TRIG_mg_dL","Glu_mg_dL")
y <- clean.data[,Y.vars]

meta.vars <- c("Subject","Period","BaselineEnd","Treatment","WholeAlmond")
meta <- clean.data[,meta.vars]

tmp.data <- cbind(meta,order.data,y)
#tmp.data <- cbind(meta,family.data,y)
#tmp.data <- cbind(meta,genus.data,y)


Control.data <- subset(tmp.data,Treatment=="control")
NWhole.data <- subset(tmp.data,Treatment %in% c("Butter","Chopped"))
Whole.data <- subset(tmp.data,Treatment %in% c("wholeRaw","WholeRoasted"))

baseline.data <- rbind(subset(Whole.data,BaselineEnd=="Baseline"),
                       subset(NWhole.data,BaselineEnd=="Baseline"))
end.data <- rbind(subset(Whole.data,BaselineEnd=="End"),
                  subset(NWhole.data,BaselineEnd=="End"))

# Check Subject Matching
sum( (baseline.data[,"Subject"]!=end.data[,"Subject"])&
       (baseline.data[,"Period"]!=end.data[,"Period"]) )


Treatment <- end.data[,"WholeAlmond"]


###############################################################
#
#   MedTree And MedForest
#
###############################################################

source("buildForest.R")
source("predict.R")

exhaust <- buildForest(
  x.b = data.matrix(baseline.data[,order.data.name]),
  x.e = data.matrix(end.data[,order.data.name]),
  treat = as.vector(Treatment),
  y.b = data.matrix(baseline.data[,Y.vars]),
  y.e = data.matrix(end.data[,Y.vars]),
  nodesize = 10)


forest <- buildForest(
  x.b = data.matrix(baseline.data[,order.data.name]),
  x.e = data.matrix(end.data[,order.data.name]),
  treat = as.vector(Treatment),
  y.b = data.matrix(baseline.data[,Y.vars]),
  y.e = data.matrix(end.data[,Y.vars]),
  nsplits=10, ntree=200, nodesize=10)


###############################################################
#
#   L1
#
###############################################################
library(glmnet)
# Constructing data with interaction term
dat <- data.frame( x = baseline.data[,order.data.name], Treat = Treatment)
f <- as.formula(~(.-Treat)*Treat)
x <- model.matrix(f, dat)

cv.res <- cv.glmnet(x,end.data[,Y.vars],family="mgaussian", standardize=T, intercept=T)
glm.res <- glmnet(x,end.data[,Y.vars],family="mgaussian", lambda = cv.res$lambda.min, intercept=T)

test.treat <- data.frame(x=X.test.base, Treat=rep(1,n.test))
test.untreat <- data.frame(x=X.test.base, Treat=rep(0,n.test))

x.test.treat <- model.matrix(f, test.treat)
x.test.untreat <- model.matrix(f, test.untreat)
susan.treat.pred <- predict(glm.res, x.test.treat)
susan.untreat.pred <- predict(glm.res, x.test.untreat)


###############################################################
#
#   GUIDE
#
###############################################################


# Run Loh's model
system("./guide < RD_Setting.in > Real.Data.Log.txt")

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




eWeight <- rep(0.1,10)

exhaust.recom <- recommendResult(exhaust, data.matrix(baseline.data[,order.data.name]), eWeight)
forest.recom <- recommendResult(forest, data.matrix(baseline.data[,order.data.name]), eWeight)
