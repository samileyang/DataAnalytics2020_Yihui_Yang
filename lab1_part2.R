help("qqnorm")
EPI_data <- read.csv("EPI_data.csv")
EPI<-EPI_data$EPI

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)



plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type = "l")
points(pressure$temperature,pressure$pressure)
lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")

qplot(pressure$temperature,pressure$pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()


barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data = mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 10)
qplot(mpg,data=mtcars,binwidth = 4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 5)

plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data = ToothGrowth)
boxplot(len~supp+dose,data = ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom = "boxplot")
qplot(supp,len,data= ToothGrowth,geom ="boxplot")
ggplot(ToothGrowth,aes(x= supp,y=len))+geom_boxplot()
