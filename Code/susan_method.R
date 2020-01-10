library(glmnet)

# Constructing data with interaction term
dat <- data.frame( x = sim.dat$train$x.b, Treat = sim.dat$train$treat)
f <- as.formula(~(.-Treat)*Treat)
x <- model.matrix(f, dat)

cv.res <- cv.glmnet(x,sim.dat$train$y.e,family="mgaussian", standardize=T, intercept=T)
glm.res <- glmnet(x,sim.dat$train$y.e,family="mgaussian", lambda = cv.res$lambda.min, intercept=T)

test.treat <- data.frame(x=sim.dat$test$x.b, Treat=rep(1,n.test))
test.untreat <- data.frame(x=sim.dat$test$x.b, Treat=rep(0,n.test))

x.test.treat <- model.matrix(f, test.treat)
x.test.untreat <- model.matrix(f, test.untreat)
susan.treat.pred <- predict(glm.res, x.test.treat)
susan.untreat.pred <- predict(glm.res, x.test.untreat)
#true.treat <- ifelse(Y.test.case.end%*%weight > Y.test.control.end%*%weight,1,0)
#susan.treat <- ifelse(susan.treat.pred [,,1]%*%weight > susan.untreat.pred[,,1]%*%weight,1,0)





