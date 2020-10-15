#Assignment 3 Script


###### 1

data1 <- read.csv("a3d1.csv")
BPchange <- unlist(data1["BPchange"])
BPinit <- unlist(data1["BPinit"])
Dose <- unlist(data1["Dose"])

model0 <- lm(BPchange ~ BPinit)
model1 <- lm(BPchange ~ Dose)

plot(BPchange ~ BPinit)
abline(model0$coefficients)

plot(BPchange ~ Dose)
abline(model1$coefficients)

plot(model0)
plot(model1)

model2 <- lm(BPchange ~ BPinit + Dose)
model2

nullmodel <- lm(BPchange ~ 1)
anova(nullmodel,model2)

summary(model2)

####### 2

data2 <- read.csv("a3d2.csv")
y <- unlist(data2["y"])
x <- unlist(data2["x"])

plot(x,y,  xlim=c(0, max(x)), ylim=c(0, max(y)))

model0<-lm(y~1)
model1<-lm(y~x)
x2<-x*x
model2<-lm(y~x+x2)
x3<-x*x*x
model3<-lm(y~x+x2+x3)
x4<-x*x*x*x
model4<-lm(y~x+x2+x3+x4)
anova(model0,model1,model2,model3,model4)


plot(y~x, xlim=c(min(x), max(x)), ylim=c(min(y), max(y)))
abline(model0,col="red")
abline(model1$coefficients,col="blue")
xseq<-seq(0,max(x),0.01)
lines(xseq,model2$coefficients[1]+model2$coefficients[2]*xseq+model2$coefficients[3]*xseq*xseq,col="black")
lines(xseq,model3$coefficients[1]+model3$coefficients[2]*xseq+model3$coefficients[3]*xseq*xseq+model3$coefficients[4]*xseq*xseq*xseq,col="purple")
legend("topright", c("Null","Linear","Quadratic","Cubic", "Data"),col=c("red","Blue","black","Purple","black"),pch=c("-","-","-","-","o"), ncol=1)

pred <- predict(model3,newdata = list(x), interval = "prediction")
lwr<-pred[,2]
upr<-pred[,3]

plot(y~x, xlim=c(min(x), max(x)), ylim=c(min(lwr), max(upr)))
xseq<-seq(min(x),max(x),0.01)
lines(xseq,model3$coefficients[1]+model3$coefficients[2]*xseq+model3$coefficients[3]*xseq*xseq+model3$coefficients[4]*xseq*xseq*xseq,col="black")
legend("topright", c("Fitted Cubic", "Data","Prediction Interval"),col=c("black","black","red"),pch=c("-","o","*"), ncol=1)
points(x,lwr,col="red", pch="*")
points(x,upr,col="red", pch="*")

newdata <- data.frame(x = 0,x2 = 0,x3 = 0,x4 = 0)

pred1 <- predict(model1,newdata = newdata, interval = "prediction")
pred2 <- predict(model2,newdata = newdata, interval = "prediction")
pred3 <- predict(model3,newdata = newdata, interval = "prediction")
pred4 <- predict(model4,newdata = newdata, interval = "prediction")

width1 = pred1[3] - pred1[2]
width2 = pred2[3] - pred2[2]
width3 = pred3[3] - pred3[2]
width4 = pred4[3] - pred4[2]

plot(model3)
plot(model2)
plot(model1)
plot(model4)

data2 <- read.csv("a3d2.csv")
data2new <- data2[c(-1),]
xnew <- unlist(data2new["x"])
ynew <- unlist(data2new["y"])

modelnew0<-lm(ynew~1)
modelnew1<-lm(ynew~xnew)
xnew2<-xnew*xnew
modelnew2<-lm(ynew~xnew+xnew2)
xnew3<-xnew*xnew*xnew
modelnew3<-lm(ynew~xnew+xnew2+xnew3)
xnew4<-xnew*xnew*xnew*xnew
modelnew4<-lm(ynew~xnew+xnew2+xnew3+xnew4)
anova(modelnew0,modelnew1,modelnew2,modelnew3,modelnew4)

newdata <- data.frame(xnew = 0,xnew2 = 0)
prednew2 <- predict(modelnew2, newdata = newdata, interval = "prediction")
prednew2

plot(ynew~xnew, xlim=c(min(x), max(x)), ylim=c(min(y), max(y)))
xseq<-seq(0,max(x),0.01)
lines(xseq,model2$coefficients[1]+model2$coefficients[2]*xseq+model2$coefficients[3]*xseq*xseq,col="black")
lines(xseq,modelnew2$coefficients[1]+modelnew2$coefficients[2]*xseq+modelnew2$coefficients[3]*xseq*xseq,col="red")

####### 3

require("yarrr")
data3 <- yarrr :: diamonds

weight <- unlist(data3["weight"])
clarity <- unlist(data3["clarity"])
color <- unlist(data3["color"])
value <- unlist(data3["value"])

modelw <- lm(value ~ weight)
plot(modelw)

modelcl <- lm(value ~ clarity)
plot(modelcl)

modelco <- lm(value ~ color)
plot(modelco)
summary(modelco)

fullmodel <- lm(value~ weight + clarity + color)
plot(fullmodel)

plot(value~color)
plot(value~clarity)
plot(value~weight)

require("leaps")
models <- regsubsets(value~., data = data3, nvmax = 3)
summary(models)

modelwcl <- lm(value ~ weight + clarity)
nullmodel <- lm(value~1)

anova(nullmodel,modelw,modelwcl,fullmodel)

plot(modelwcl)

newdata <- data.frame(weight = 10,clarity = 1,color = 5)

predwcl <- predict(modelwcl,newdata = newdata, interval = "prediction")
confwcl <- predict(modelwcl,newdata = newdata, interval = "confidence")

predwcl
confwcl






