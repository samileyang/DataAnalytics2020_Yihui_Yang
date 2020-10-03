library(ISLR)
library(ggplot2)
library(class)
library(dplyr)
# Assignment2 p1

EPI_data <- read.csv("EPI_data.csv")
EPI<-EPI_data$EPI
DALY<-EPI_data$DALY

dim(EPI_data)
names(EPI_data)

attach(EPI_data)
#central tendency
summary(EPI)
fivenum(EPI)
mean(EPI, na.rm = TRUE)
median(EPI, na.rm = TRUE)

summary(DALY)
fivenum(DALY)
mean(DALY, na.rm = TRUE)
median(DALY, na.rm = TRUE)

#Histogram
hist(EPI)
hist(DALY)

#box plot
boxplot(ENVHEALTH, ECOSYSTEM)
#Q-Q plot
qqplot(ENVHEALTH, ECOSYSTEM)

# 2b
# Linear and least squares
boxplot(ENVHEALTH, DALY, AIR_H, WATER_H)

lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH

summary(lmENVH)
cENVH <- coef(lmENVH)
cENVH 

# To predict
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))

NEW <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)

pENV <- predict(lmENVH, NEW, interval = "prediction")
cENV <- predict(lmENVH, NEW, interval = "confidence")

# Repeat for AIR_E
lmAIR_E <- lm(AIR_E~DALY+AIR_H+WATER_H)
lmAIR_E

summary(lmAIR_E)
cAIR_E <- coef(lmAIR_E)
cAIR_E

# To predict
pAIR_E<- predict(lmAIR_E, NEW, interval = "prediction")
cAIR_E <- predict(lmAIR_E, NEW, interval = "confidence")

# Repeat for CLIMATE
lmCLIMATE <- lm(CLIMATE~DALY+AIR_H+WATER_H)
lmCLIMATE

summary(lmCLIMATE)
cCLIMATE <- coef(lmCLIMATE)
cCLIMATE

pCLIMATE <- predict(lmCLIMATE, NEW, interval = "prediction")
cCLIMATE <- predict(lmCLIMATE, NEW, interval = "confidence")

# Euro
EPI_Euro <- EPI_data[EPI_data$EPI_regions=="Europe",]
data_lr <- Filter(is.numeric, EPI_Euro)
head(data_lr)

EPI_lr <- lm(EPI~AIR_H+DALY+Population07+PopulationDensity+WATER_H, data = data_lr)
EPI_lr

summary(EPI_lr)

# Assignment2 p2

# Exercise 1: Regression
regression <- read.csv("dataset_multipleRegression.csv")
head(regression)

# UNEM&HGRAD
m1 <- lm(ROLL~UNEM+HGRAD, data = regression)

summary(m1)

UNEM <- 7.0
HGRAD <- 90000

new1 <- data.frame(UNEM, HGRAD)

pred_ROLL <- predict(m1, new1, interval = "prediction")
pred_ROLL

#INC
m2 <- lm(ROLL~UNEM+HGRAD+INC, data = regression)

summary(m2)

UNEM <- 7.0
HGRAD <- 90000
INC <- 25000

new2 <- data.frame(UNEM, HGRAD, INC)

pred_ROLL_2 <- predict(m2, new2, interval = "prediction")
pred_ROLL_2

# Exercise 2: Classification
abalone <- read.csv("abalone.csv")
head(abalone)
summary(abalone$Rings)

abalone$Rings <- as.numeric(abalone$Rings)
abalone$Rings <- cut(abalone$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$Age <- as.factor(abalone$Rings)
summary(abalone$Age)

aba <- abalone
aba$Sex <- NULL

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dim(aba)
names(aba)

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$Shucked.weight)

ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]

sqrt(dim(KNNtrain)[1])

KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$Age, k = 55)
table(KNNpred)

# Exercise 3: Clustering
head(iris)
summary(iris)

iris_new <- iris[,1:4]
iris_new 

set.seed(1)
k.max <- 12

# optimal k
wss<- sapply(1:k.max,function(k){kmeans(iris[,1:4],k,nstart = 20,iter.max = 1000)$tot.withinss})

plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

icluster <- kmeans(iris[,1:4],3,nstart = 20,iter.max = 1000)

table(iris[,5], icluster$cluster)

# Exercise 4: Dplyr
EPI_data <- read.csv("EPI_data.csv")

EPI<-data.frame(EPI_data$EPI)
DALY<-data.frame(EPI_data$DALY)

head(EPI_data)

sample_n(EPI, 5)
sample_n(DALY, 5)

sample_frac(EPI, 0.1)
sample_frac(DALY, 0.1)

new_decs_EPI <- arrange(EPI, desc(EPI_data$EPI))
new_decs_DALY <- arrange(DALY, desc(EPI_data$DALY))

EPI_data %>% mutate(
  double_EPI = EPI *2,
  double_DALY = DALY *2
)

summarise(EPI_data, avg_EPI = mean(EPI, na.rm = TRUE)) 
summarise(EPI_data, avg_DALY = mean(DALY, na.rm = TRUE)) 












