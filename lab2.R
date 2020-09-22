library(tidyr)
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
summary(mm)
abline(mm)
abline(mm,col=2,lwd=3)

HP<- Homeowners/Population
PD<-Population/area
mm<-lm(Immigrant~Income+Population+HP+PD)
summary(mm)

newImmigrantdata <- data.frame(Immigrant = c(0,  20))
mm %>% predict(newImmigrantdata)
abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients



