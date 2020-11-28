v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores

install.packages("Hmisc")
library(Hmisc)
AthleticsData <- spss.get("AthleticsData.sav")
attach(AthleticsData)
#
names(AthleticsData)

cor(AthleticsData)
prcomp(AthleticsData)

fit.2 <- factanal(AthleticsData,factors=2,rotation="varimax")
print(fit.2)

fit.3 <- factanal(AthleticsData,factors=3,rotation="varimax")
print(fit.3)
print(fit.3, digits = 2, cutoff = .2, sort = TRUE)

install.packages("GPArotation")
library(GPArotation)

fit <- principal(AthleticsData, nfactors=3, rotate=â???varimaxâ???)
fit # print results


# do not go past here unless you can find fa.promax.R



fit.3.promax <- update(fit.3,rotation="promax") 
colnames(fit.3.promax$loadings)<-c("Endurance","Strength","Hand-Eye") 
print(loadings(fit.3.promax), digits = 2, cutoff = .2, sort = TRUE)
AssignFactorNames <- function(fit.object,names)
{
  colnames(fit.object$promax.loadings)<-names
  colnames(fit.object$varimax.loadings)<-names
  rownames(fit.object$corr.factors)<-names
  colnames(fit.object$corr.factors)<-names
}
fit.3.Enzmann <- fa.promax(AthleticsData,factors=3, digits=2, sort=TRUE) AssignFactorNames(fit.3.Enzmann,factor.names)
fit.3.Enzmann


data(epi)
epi.keys <- make.keys(epi,list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,
                                     -29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                               N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,
                                   43, 45, 47, 50, 52, 55, 57),
                               L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                               I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                               S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))
scores <- scoreItems(epi.keys,epi)
N <- epi[abs(epi.keys[,"N"]) >0]
E <- epi[abs(epi.keys[,"E"]) >0]
fa.lookup(epi.keys[,1:3],epi.dictionary) #show the items and keying information


