setwd("C:/Users/16437/Desktop/2020 Fall/assignment 7")
library(ggplot2)
library(rlang)
library(glue)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(factoextra)
library(usethis)
df_sobar <- read.csv("sobar-72.csv")

df_red <- read.csv("winequality-red.csv",sep = ";")
df_white <- read.csv("winequality-white.csv",sep = ";")
df_red$color <- 'red'
df_white$color <- 'white'

df <- rbind(df_red,df_white)
df
head(df)
summary(df)

qplot(quality, data = df,fill = color,binwidth = 1)

qplot(x = color, y = density, data = df, geom = "boxplot")
qplot(x = color, y = pH, data = df, geom = "boxplot")
qplot(x = color, y = alcohol, data = df, geom = "boxplot")


shapiro.test(sample(df$quality,5000,replace = FALSE,prob = NULL))
boxplot(df$quality)
library(tidyverse)
library(corrplot)
corrplot(df)

str(df)
df1 <- subset(df,select = -c(color))
str(df1)
heatmap(df1)
df1$quality <- as.numeric(df1$quality)
cor(df1)
heatmap(cor(df1))
corrplot(cor(df1))
library(randomForest)
df1$quality <- as.factor(df1$quality)
training <- df1[1:5198,]
validation <- df1[5199:6497,]
wine.rf <- randomForest(quality ~ .,  
                        data = training) 
wine.rf

prediction_for_table <- predict(wine.rf,validation[,1:11])
prediction_for_table
install.packages('confusionMatrix')

install.packages('class')
library(class)

train_wine <- df1[1:5198,]
test_wine <- df1[5199:6497,]

train_wine_target <- df1[1:5198,12]
test_wine_target <- df1[5199:6497,12]
library(class)
prc_test_pred <- knn(train = train_wine, test = test_wine,cl = train_wine_target, k=10)
install.packages("gmodels")
library(gmodels)
CrossTable(x = test_wine_target, y = prc_test_pred)

fviz_nbclust(subset(df1,select = -c(quality)), kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")
set.seed(123)
km.res <- kmeans(subset(df1,select = -c(quality)), 3, nstart = 25)
print(km.res$cluster)
df1$cluster <- print(km.res$cluster)
CrossTable(y = df1$quality, x = df1$cluster)
library(aod)
df_sobar
sobar_lr <-glm(ca_cervix~.,data =df_sobar)
summary(sobar_lr)

df_sobar$ca_cervix <- as.factor(df_sobar$ca_cervix)
sobar_rf <- randomForest(ca_cervix~.,data =df_sobar)
sobar_rf


df_sobar1 <- subset(df_sobar,select = -c(ca_cervix))
corrplot(cor(df_sobar1))

train_sobar <- df_sobar[1:5198,]
test_wine <- df_sobar[5199:6497,]

train_wine_target <- df1[1:5198,12]
test_wine_target <- df1[5199:6497,12]

set.seed(123) 
split = sample.split(df_sobar$ca_cervix, SplitRatio = 0.75) 

install.packages('caTools') 
library(caTools) 
training_set = subset(df_sobar, split == TRUE) 
test_set = subset(df_sobar, split == FALSE)
install.packages('e1071') 
library(e1071) 
classifier <- svm(formula = ca_cervix ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear')
summary(classifier)
y_pred <- predict(classifier, newdata = subset(test_set,select = -c(ca_cervix)))
cm = table(test_set$ca_cervix, y_pred)
cm
head(df_sobar)
boxplot(df_sobar1)
summary(df_sobar$ca_cervix)

boxplot(df_red)
boxplot(df_white)
