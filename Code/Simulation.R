library(MASS)     # R library supporting multivariate normal simulation

####################################
# Simulate Setting
#
####################################

n.train <-  500
n.test <- 200
p <- 10
q <- 3
# pi is the ratio between treatment groups
pi <- 0.5

# Treatment effect function
treat.effect <- function(x.base,treat,i,j)
{
  if(i == 1 ) y <- x.base[,c(i,j)]%*%c(-0.50,-0.265)
  else if(i ==2) y <- x.base[,c(i,j)]%*%c(-0.16,-0.05)
  else y <- x.base[,c(i,j)]%*%c(-0.38,0.8525)
  
  # Create treatment effective population
  # Extremely old version
  #treat1.pop <- x.base[,i]+x.base[,j]>0
  # V3.0 stable version
  treat1.pop <- rep(TRUE,nrow(x.base))
  treat0.pop <- rep(TRUE,nrow(x.base))

  # Create treatment effective magnitude
  no.treat.effect <- 0
  # Extremely old version
  #  treat1.effect <- ifelse(treat1.pop,6,no.treat.effect)
  #  treat0.effect <- ifelse(treat0.pop,3,no.treat.effect)
  # V3.0 stable version
  treat1.effect <- ifelse(treat1.pop,y,no.treat.effect)
  treat0.effect <- ifelse(treat0.pop,-y,no.treat.effect)

  return(ifelse(treat,treat1.effect,treat0.effect))
}

# Beta matrix
Z <- matrix(
  cbind(
    c(rep(1,5),rep(0,5)),
    c(rep(0,2),rep(1,5),rep(0,3)),
    c(rep(0,5),rep(1,5))
  ),nrow=p,ncol=q)


####################################
# Simulate Training data
#
####################################
# Check if the portion size is integer

# Simulate the treatment
Treat.train <- sample(
  c(
    rep(1, floor(n.train*pi)),           # Treatment Group 1
    rep(0, ceiling(n.train*(1-pi)))      # Treatment Group 2
  )
)

# Simulate the autoregressive covariance matrix of X
x.sig <- 0.8^(abs(outer(1:p,1:p,"-")))

# Simulate x.b
#X.train.base <- mvrnorm(n.train,rep(0,p),x.sig)
X.train.base <- MASS::mvrnorm(n.train,rep(0,p),diag(p))


X.train.end <- X.train.base
X.train.end[,5] <- X.train.end[,5] + treat.effect(X.train.base,Treat.train,1,3)
X.train.end[,6] <- X.train.end[,6] + treat.effect(X.train.base,Treat.train,2,4)
X.train.end[,7]<- X.train.end[,7] + treat.effect(X.train.base,Treat.train,3,5)
X.train.end <- X.train.end + mvrnorm(n.train, rep(0,p), 0.1*diag(p))   

Y.train.base <- (X.train.base)%*%Z + mvrnorm(n.train, rep(0,q), 0.1*diag(q))
Y.train.end <- (X.train.end)%*%Z + mvrnorm(n.train, rep(0,q), 0.1*diag(q))


####################################
# Simulate Testing data
#
####################################

#X.test.base  <- mvrnorm(n.test,rep(0,p),x.sig)
X.test.base  <- mvrnorm(n.test,rep(0,p),diag(p))
# With/Without treatment X.end
X.test.case.end <-  X.test.base
X.test.control.end <- X.test.base
X.test.case.end[,5] <- X.test.case.end[,5] + treat.effect(X.test.base,rep(1,n.test),1,3)
X.test.case.end[,6] <- X.test.case.end[,6] + treat.effect(X.test.base,rep(1,n.test),2,4)
X.test.case.end[,7] <- X.test.case.end[,7] + treat.effect(X.test.base,rep(1,n.test),3,5)
X.test.control.end[,5] <- X.test.control.end[,5] + treat.effect(X.test.base,rep(0,n.test),1,3)
X.test.control.end[,6] <- X.test.control.end[,6] + treat.effect(X.test.base,rep(0,n.test),2,4)
X.test.control.end[,7] <- X.test.control.end[,7] + treat.effect(X.test.base,rep(0,n.test),3,5)
X.test.case.end <- X.test.case.end
X.test.control.end <- X.test.control.end
# With/Without treatment Y.base
Y.test.case.end <- (X.test.case.end)%*%Z
Y.test.control.end <- (X.test.control.end)%*%Z

# Generating Rdata file for GUIDE
# train.lab variable in this file is for the prediction purpose for Loh's method
train.lab <- c(rep(1,n.train),rep(0, 2*n.test))
Xs <- rbind(X.train.base, X.test.base, X.test.base)
Ys <- rbind(Y.train.end, matrix(NA,nrow=2*n.test,ncol=ncol(Y.train.end)))
Treat.all <- c(Treat.train,rep(1,n.test),rep(0,n.test))
all.data <- cbind(train.lab, Treat.all, Xs, Ys)
colnames(all.data) <- c("Train","Treated",paste0("X",1:p),paste0("Y",1:q))
# Save data for Loh's method
write.csv(all.data,paste0(args[1],"input_data.rdata"),row.names=F)



