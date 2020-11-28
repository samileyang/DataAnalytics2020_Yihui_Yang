setwd("C:/Users/16437/Desktop/2020 Fall/project")
library(readxl)
library(rlang)
library(glue)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(factoextra)
library(usethis)

df <- read_excel("data.xlsx")

ggplot(df, aes(x=Datetime, y=AQI)) +
  geom_line()

str(df)
head(df)
boxplot(df$AQI)
boxplot(df$PM2.5)
summary(df)

df[df$AQI == 0,]$AQI <-mean(df$AQI)
df[df$PM2.5 == 0,]$PM2.5 <-mean(df$PM2.5)
df[df$PM10 == 0,]$PM10 <-mean(df$PM10)
df[df$SO2 == 0,]$SO2 <-mean(df$SO2)
df[df$CO == 0,]$CO <-mean(df$CO)
df[df$NO2 == 0,]$NO2 <-mean(df$NO2)
df[df$O3_8h == 0,]$O3_8h <-mean(df$O3_8h)
summary(df)

ggplot(df, aes(x=Datetime, y=AQI)) +
  geom_line()
ggplot(df, aes(x=Datetime, y=PM2.5)) +
  geom_line()

df_bymonth <- df %>% group_by(month=floor_date(Datetime, "month")) %>%
  summarize(AQI=mean(AQI))
df_bymonth
ggplot(df_bymonth, aes(x=month, y=AQI)) +
  geom_line()


df_2016 <- df %>% filter(Datetime >= '2016-01-01' & Datetime <= '2016-12-31')
df_2017 <- df %>% filter(Datetime >= '2017-01-01' & Datetime <= '2017-12-31')
df_2019 <- df %>% filter(Datetime >= '2019-01-01' & Datetime <= '2019-12-31')
df_2018 <- df %>% filter(Datetime >= '2018-01-01' & Datetime <= '2018-12-31')
df_2020_during_cov <- df %>% filter(Datetime >= '2020-01-01' & Datetime <= '2020-04-30')
df_2020_after_cov <- df %>% filter(Datetime >= '2020-05-01' & Datetime <= '2020-09-30')
df_2019_during_cov_period <- df %>% filter(Datetime >= '2019-01-01' & Datetime <= '2019-04-30')


ggplot(df_2019, aes(x=Datetime, y=AQI)) +
  geom_line()
ggplot(df_2018, aes(x=Datetime, y=AQI)) +
  geom_line()
ggplot(df_2020_during_cov, aes(x=Datetime, y=AQI)) +
  geom_line()
ggplot(df_2020_after_cov, aes(x=Datetime, y=AQI)) +
  geom_line()
ggplot(df_2018, aes(x=Datetime, y=AQI)) +
  geom_line(color = "darkred")

df_heatmap <- subset(df,select = -c(Datetime))
data.matrix(df_heatmap)
cor(df_heatmap)
heatmap(cor(df_heatmap))

df_scale <- scale(df_heatmap)
head(df_scale)


fviz_nbclust(df_heatmap, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
set.seed(123)
km.res <- kmeans(df_scale, 4, nstart = 25)
print(km.res)
km.res.agg <- aggregate(df_heatmap,by=list(km.res$cluster),mean)
km.res.agg

df$kmean <- as.numeric(km.res$cluster)
length(df$kmean)
kmeans_df <- subset(df,select = -c(Datetime))
kmeans_df_agg <- aggregate(kmeans_df,by=list(kmeans_df$kmean),mean)
kmeans_df_agg
kmeans_df_count <- aggregate(kmeans_df,by=list(kmeans_df$kmean),count)
kmeans_df_count
df$kmean <- as.factor(km.res$cluster)
b <- ggplot(df, aes(x = Datetime, y = AQI))
b + geom_point(aes(color = kmean))


mean(df_2020_during_cov$AQI)
mean(df_2019_during_cov_period$AQI)

mean(df_2018$AQI)
mean(df_2019$AQI)
mean(df_2017$AQI)
mean(df_2016$AQI)


res_16_17 <- t.test(df_2016$AQI,df_2017$AQI,var.equal = TRUE,alternative = "greater")
res_16_17

res_17_18 <- t.test(df_2017$AQI,df_2018$AQI,var.equal = TRUE,alternative = "greater")
res_17_18

res_18_19 <- t.test(df_2018$AQI,df_2019$AQI,var.equal = TRUE,alternative = "greater")
res_18_19

res_cov <- t.test(df_2019_during_cov_period$AQI,df_2020_during_cov$AQI,var.equal = TRUE,alternative = "greater")
res_cov

ts_df <- df[,c("Datetime","AQI")]
library(forecast)
aqi.ts <- ts(ts_df$AQI,start = c(2016,1,1),end = c(2019,12,31),freq=30)
aqi.ts

# plot the series
# the bty option allows to custom the box around the plot
plot(aqi.ts,xlab="Time",ylab="AQI")
nValid <- 5
nTrain <- length(aqi.ts)-nValid
train.ts <- window(aqi.ts, start = c(2016,1), end = c(2016,nTrain))
valid.ts <- window(aqi.ts,start = c(2016,nTrain+1),end=c(2016,nTrain+nValid))

train.lm <- tslm(train.ts~trend)
train.lm.pred <- forecast(train.lm, h=nValid, level=0)

plot(train.lm.pred,ylab="AQI",xlab="Time",
     xaxt="n")

lines(train.lm.pred$fitted,lwd=2,col="blue")
lines(valid.ts)

summary(train.lm)
train.lm.season <- tslm(train.ts~season)
summary(train.lm.season)
train.lm.trend.season <- tslm(train.ts~trend+I(trend^2)+season)
summary(train.lm.trend.season)
train.lm.pred <- forecast(train.lm.trend.season, h=nValid, level=0)
plot(train.lm.pred,ylab="AQI",xlab="Time",
     xaxt="n",lty = 1)

lines(train.lm.pred$fitted,lwd=3,col="red",lty = 5)
lines(valid.ts)
accuracy(train.lm.pred,valid.ts)
