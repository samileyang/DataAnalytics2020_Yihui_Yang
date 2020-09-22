multivariate <- read.csv("multivariate.csv")
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm

plot(Income,Immigrant,main = "Scatterplot")
plot(Immigrant,Homeowners)


help(lm)
mm<-lm(Homeowners~Immigrant)
mm
plot(Immigrant,Homeowners)
abline(mm)

HP<- Homeowners/Population
PD<-Population/area
mm<-lm(Immigrant~Income+Population+HP+PD)
summary(mm)
