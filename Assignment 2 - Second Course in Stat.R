
#Input Data
cw<-c(5,5,7,8,8,10,11,12,12,13,14,16)
ex<-c(38,45,50,56,62,69,65,68,71,66,75,96)

#First visualisation
plot(ex~cw)

#Simple linear regression
model<-lm(ex~cw)
model
plot(model)

#Plotting regression line
par(mfrow=c(1,1))
plot(ex~cw)
abline(model$coefficients)

conf1<-predict(model,interval="confidence", level=.95)
matplot(cw,cbind(conf1),pch=c(2,1,1),col=c(1,2,2))

pred1<-predict(model,list(cw),interval="prediction")
matplot(cw,cbind(pred1,ex),pch=c(2,1,1,1,1,1),col=c(1,2,2,3,4,4))

new.data0<- data.frame(cw=c(10,20))
predict(model, newdata=new.data0)

#################################

x<-rnorm(100,10,4)
y<-seq(from = 1, to = 100, by = 1)


#Plotting regression line
par(mfrow=c(1,1))
plot(y~x)
abline(model2$coefficients)


#Simple linear regression
model2<-lm(y~x)
model2
plot(model2)

x1<-rnorm(20,10,4)
y1<-seq(from = 1, to = 20, by = 1)

#Plotting regression line
par(mfrow=c(1,1))
plot(y1~x1)
abline(model3$coefficients)


#Simple linear regression
model3<-lm(y1~x1)
model3
plot(model3)

#########################

data = read.csv("C:\\Users\\alex\\Desktop\\data.csv", header=T, sep=" ")
x9<-data.matrix(data[1], rownames.force = NA)
y9<-data.matrix(data[2], rownames.force = NA)
plot(y9~x9)

#Simple linear regression
model9<-lm(y9~x9)
model9
plot(model9)

#Plotting regression line
par(mfrow=c(1,1))
plot(y9~x9)
abline(model9$coefficients)

x10<-log(x9)
plot(y9~x10)
model10<-lm(y9~x10)
plot(model10)
res<-resid(model10)
hist(res,probability=TRUE)           
lines(density(res, adjust=2), lty="dotted")

x12<-rnorm(100,10,4)
y12<-2*x12+10+rnorm(100,10,x12/2)
plot(y12~x12)
model12<-lm(y12~x12)
plot(model12)

