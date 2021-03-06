library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)
BC.bagging <- bagging(Class ~., data=BreastCancer[,-1], mfinal=20, control=rpart.control(maxdepth=3))
BC.bagging.pred <-predict.bagging( BC.bagging, newdata=BreastCancer[-sub,-1])
BC.bagging.pred$confusion

library(ipred)
data(Ozone)
l <- length(Ozone[,1])
sub <- sample(1:l,2*l/3)
OZ.bagging <- bagging(V4 ~., data=Ozone[,-1], mfinal=30, control=rpart.control(maxdepth=5))
OZ.bagging.pred <-predict.bagging( OZ.bagging, newdata=Ozone[-sub,-4])
OZ.bagging.pred$confusion

library(ipred)
library("MASS")
library("survival")


data("BreastCancer", package = "mlbench")


mod <- bagging(Class ~ Cl.thickness + Cell.size
               + Cell.shape + Marg.adhesion   
               + Epith.c.size + Bare.nuclei   
               + Bl.cromatin + Normal.nucleoli
               + Mitoses, data=BreastCancer, coob=TRUE)
print(mod)

data("Ionosphere", package = "mlbench")
Ionosphere$V2 <- NULL 

bagging(Class ~ ., data=Ionosphere, coob=TRUE)


comb.lda <- list(list(model=lda, predict=function(obj, newdata)
  predict(obj, newdata)$x))


mod <- bagging(Class ~ ., data=Ionosphere, comb=comb.lda) 

predict(mod, Ionosphere[1:10,])


data("BostonHousing", package = "mlbench")


mod <- bagging(medv ~ ., data=BostonHousing, coob=TRUE)
print(mod)

library("mlbench")
learn <- as.data.frame(mlbench.friedman1(200))


mod <- bagging(y ~ ., data=learn, coob=TRUE)
print(mod)


data("DLBCL", package = "ipred")
mod <- bagging(Surv(time,cens) ~ MGEc.1 + MGEc.2 + MGEc.3 + MGEc.4 + MGEc.5 +
                 MGEc.6 + MGEc.7 + MGEc.8 + MGEc.9 +
                 MGEc.10 + IPI, data=DLBCL, coob=TRUE)

print(mod)

