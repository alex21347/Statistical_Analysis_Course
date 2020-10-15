#Can download R from here: https://cran.r-project.org/

#Data taken from Moore/McCabe Example 2.12

#Input data
x<-c(-94,-57,-29,135,143,151,245,355,392,473,486,535,571,580,620,690)
y<-c(4.2,3,3.7,2.7,3.2,3.6,2.4,1.3,3.8,1.7,1.6,2.2,1,0.4,2.3,1.1)

#First visualisation
plot(y~x)

#Simple linear regression
model<-lm(y~x)
model

#Plotting regression line
par(mfrow=c(1,1))
plot(y~x)
abline(model$coefficients)

#Adding additional point
a<-model$coefficients[1]+model$coefficients[2]*3000
a

x1<-c(3000,x)
y1<-c(a,y)

x2<-c(3000,x)
y2<-c(5,y)

x3<-c(mean(x),x)
y3<-c(2000,y)

model
model1<-lm(y1~x1)
model1

model2<-lm(y2~x2)
model2

model3<-lm(y3~x3)
model3

model

plot(y1~x1)
abline(model$coefficients)
abline(model1$coefficients)

plot(y2~x2)
abline(model$coefficients)
abline(model2$coefficients)

plot(y3~x3,ylim=c(0,120))
abline(model$coefficients)
abline(model3$coefficients)

hatvalues(model1)
sum(hatvalues(model1))
plot(hatvalues(model1))
hatvalues(model2)
hatvalues(model3)
plot(hatvalues(model3))

plot(hatvalues(model)~x)
plot(hatvalues(model1)~x1)

influence(model1)$coefficients

cooks.distance(model1)
plot(cooks.distance(model1))
cooks.distance(model2)
plot(cooks.distance(model2))
cooks.distance(model3)
plot(cooks.distance(model3))

plot(model1)
plot(model2)
plot(model3)

conf1<-predict(model,interval="confidence", level=.99)
conf2<-predict(model,interval="confidence", level=.95)
matplot(x,cbind(conf1,conf2,y),pch=c(2,1,1,1,1,1),col=c(1,2,2,3,4,4))

pred1<-predict(model,list(x),interval="prediction")
matplot(x,cbind(conf1,pred1,y),pch=c(2,1,1,1,1,1),col=c(1,2,2,3,4,4))




