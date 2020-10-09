EPI_data <- read.csv("EPI_data.csv")
EPI<-EPI_data$EPI
DALY<-EPI_data$DALY

fivenum(EPI)
summary(EPI)
boxplot(EPI)
fivenum(DALY)
summary(DALY)
boxplot(DALY)
EPI2020 <- read.csv("2010EPI_data.csv")
EPI2 <- EPI2020$EPI
DALY2 <- EPI2020$DALY
hist(EPI)
hist(DALY)

qqnorm(EPI_data$EPI)

EPI_data <- read.csv("EPI_data.csv")
attach(EPI_data)
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)

lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)

DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval="prediction")
cENV<- predict(lmENVH,NEW,interval="confidence")
pENV

lmAIR_E<-lm(AIR_E~DALY+AIR_H+WATER_H)
lmAIR_E
summary(lmAIR_E)
cENVH<-coef(lmAIR_E)

lmCLIMATE<-lm(CLIMATE~DALY+AIR_H+WATER_H)
lmCLIMATE
summary(lmCLIMATE)
cENVH<-coef(lmCLIMATE)


pENV
