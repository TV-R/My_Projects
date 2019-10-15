# question 1

rm(list=ls())
hybrid=read.table(file=file.choose(),header=TRUE)
summary(hybrid)
attach(hybrid)
par(mfrow=c(1,2))
plot(hybrid$accelrate,hybrid$msrp,xlab='accelerate',ylab='msrp')
abline(lm(msrp~accelrate,data=hybrid),col='blue')
plot(hybrid$mpg,hybrid$msrp,xlab='mpg',ylab='msrp')
abline(lm(msrp~mpg,data=hybrid),col='blue')
model1.hybrid.onlyintercept=lm(msrp~1)
model1.hybrid=lm(msrp~accelrate+mpg,data=hybrid)
summary(model1.hybrid)
anova(model1.hybrid)
model1.hybrid.mpg=lm(msrp~mpg+accelrate)
anova(model1.hybrid.onlyintercept,model1.hybrid)
model1.hybrid.SST=sum((msrp-mean(msrp))^2)
model1.hybrid.SSR <- sum((model1.hybrid$fitted.values - mean(msrp))^2)
model1.hybrid.SSE=sum((model1.hybrid$residuals)^2)
n=153
p=3
MSR=model1.hybrid.SSR/(p-1)
MSE=model1.hybrid.SSE/(n-p)
R.2=model1.hybrid.SSR/model1.hybrid.SST

model1.hybrid.ANOVA <- data.frame("SS"  = c(model1.hybrid.SSR, model1.hybrid.SSE,model1.hybrid.SST),
                         "Df"  = c(p - 1, n - p, n - 1),
                         "MSS" = c(MSR, MSE, NA))
row.names(model1.hybrid.ANOVA) <- c("Regression", "Error", "Total")

boxplot(msrp~class,data=hybrid,ylab='msrp')
model2.hybrid=lm(msrp~class+accelrate+mpg,data=hybrid)
anova(model1.hybrid,model2.hybrid)
par(mfrow = c(2,2))
library(MASS)
model2.hybrid.stdres=stdres(model2.hybrid)
qqnorm(model2.hybrid.stdres, main="")
qqline(model2.hybrid.stdres)
plot(model2.hybrid$residuals, xlab = "Index", ylab = "Residual")
plot(model2.hybrid$fitted.values,model2.hybrid$residuals, xlab = "Fitted value", ylab = "Residual")
lines(lowess(model2.hybrid$residuals ~ model2.hybrid$fitted.values), col = "red")
plot(model2.hybrid.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
library(dplyr)
par(mfrow=c(1,2))
plot(model2.hybrid$residuals~mpg,ylab='residuals')
plot(model2.hybrid$residuals~accelrate,ylab='residuals')
second.order.model.noncentered=lm(msrp~accelrate+I(accelrate^2)+class+mpg+I(mpg^2))
anova(model2.hybrid,second.order.model.noncentered)
mpg.mean=mpg%>%mean()
accelrate.mean=accelrate%>%mean()
mpg.centered=mpg-mpg.mean
accelrate.centered=accelrate-accelrate.mean
second.ordermodel<-lm(msrp~mpg.centered+I(mpg.centered^2)+accelrate.centered+I(accelrate.centered^2)+class)
anova(model2.hybrid,second.ordermodel)
noncentered.coef=unname(coefficients(second.order.model.noncentered))
centered.coef=unname(coefficients(second.ordermodel))
centered.coef[2]-2*centered.coef[3]*mpg.mean
centered.coef[4]-2*centered.coef[5]*accelrate.mean
prediction=predict(second.order.model.noncentered,data.frame('mpg'=45.50,'accelrate'=15.26,'class'='L'))
mpg1=mpg*(mpg<25)
mpg2=(mpg-25)*(mpg>25)
lm(msrp~mpg+mpg2+accelrate+class)%>%summary()
lm(msrp~mpg+mpg2+accelrate+class+I(mpg^2)+I(mpg2^2))%>%summary()
model.kink=lm(msrp~mpg+mpg2+accelrate+class+I(mpg^2)+I(mpg2^2)+I(accelrate^2))
predict.mpg=(45.50-25)*(45.50>25)
prediction2=predict(model.kink,data.frame('mpg'=45.50,'mpg2'=predict.mpg,'accelrate'=15.26,'class'='L'))
qqnorm(stdres(model.kink))
qqline(stdres(model.kink))
plot(model.kink$fitted.values,model.kink$residuals, xlab = "Fitted value", ylab = "Residual")
lines(lowess(model.kink$residuals ~ model.kink$fitted.values), col = "red")
detach(hybrid)

rm(list=ls())
#question 2
cafe = read.table(file=file.choose(),header=TRUE)
summary(cafe)
cafe=cafe[,-1]
cafe=na.omit(cafe)
attach(cafe)

#cafe[,1]=as.factor(as.integer(cafe[,1]))
model1.cafe=lm(Sales~.,data=cafe)

library(leaps)model1.cafe.sum=lm(Sales~.,data=cafe,contrasts = list(Day.of.Week=contr.sum))
cafe2<-cafe[c(1:40),]
model2.cafe=lm(Sales~.,data=cafe2)
model.search=regsubsets(Sales~.,data=cafe2,method='exhaustive',nvmax=25,nbest=1)

#detach(cafe)
#attach(cafe)
#cafe <- one_hot(as.data.table(cafe))
#cafe2<-cafe[c(1:40),]
#X=cafe2[,2:16]
#y=cafe2[,17]
#cp=leaps(X,y,method='Cp')
#adjr2=leaps(X,y,method='adjr2')
#library(data.table)
#library(mltools)
mse=c()
for(i in c(1:19)){mse[i]=summary(model.search)$rss[i]/(40-(i+1))}
all.models=summary(model.search)$which
all.models.cp=summary(model.search)$cp
all.models.rsq=summary(model.search)$rsq
allmodels.adjr2=summary(model.search)$adjr2
mse.x=which.min(mse)
cp.x=which.min(all.models.cp)
rsq.x=which.max(all.models.rsq)
adjr2.x=which.max(allmodels.adjr2)
mse.x.best=all.models[14,]
mse.x.best=ifelse(mse.x.best==TRUE,1,0)
mse.x.best=c(1,0,1,1,1,1,1,1,0,0,1,1,0,1,1,0)
cafe.mse=cafe2[,c(1,3,4,5,6,7,8,11,12,14,15,17)]
bic.x=summary(model.search)$bic
n <- length(cafe2$Sales)
p <- apply(all.models, 1, sum)
aic <- bic.x - log(n) * p + 2 * p
aic.x=which.min(aic)
aic.x.best=which.min(aic)
aic.x.best=all.models[12,]
aic.x.best=ifelse(aic.x.best==TRUE,1,0)
aic.x.best=c(1,0,0,0,1,1,1,1,0,0,1,1,0,1,1,0)
cafe.aic=cafe2[,c(1,5,6,7,8,11,12,14,15,17)]
mse.best=lm(Sales~.,data=cafe.mse)
rsq.best=lm(Sales~.,data=cafe2)
aic.best=lm(Sales~.,data=cafe.aic)
predict.cafe=cafe[c(41:47),]
predict.mse.cafe=cafe[c(41:47),c(1,3,4,5,6,7,8,11,12,14,15,17)]
MSE.mse <- summary(mse.best)$sigma^2
aic.mse <- summary(aic.best)$sigma^2
rsq.mse <- summary(rsq.best)$sigma^2

MSE.msep <- mean((predict(mse.best, newdata = predict.cafe) - predict.cafe$Sales)^2)
aic.msep <- mean((predict(aic.best, newdata = predict.cafe) - predict.cafe$Sales)^2)
rsq.msep <- mean((predict(rsq.best, newdata = predict.cafe) - predict.cafe$Sales)^2)
library(MASS)
model.null=lm(Sales~1,data=cafe2)
model.aic.forward=stepAIC(model.null, scope = list(upper =model2.cafe, lower = ~ 1), direction = "forward")
model.aic.backward=stepAIC(model2.cafe, scope = list(upper =model2.cafe, lower = ~ 1), direction = "backward")
model.aic.full.both=stepAIC(model2.cafe, scope = list(upper =model2.cafe, lower = ~ 1), direction = "both")
model.aic.null.both=stepAIC(model.null, scope = list(upper =model2.cafe, lower = ~ 1), direction = "both")
stepaic.number=12
stepaic.mse<-summary(model.aic.null.both)$sigma^2
stepaic.msep <- mean((predict(model.aic.null.both, newdata = predict.cafe) - predict.cafe$Sales)^2)
MSE.press <- sum((residuals(mse.best) / (1 - lm.influence(mse.best)$hat))^2)
aic.press <- sum((residuals(aic.best) / (1 - lm.influence(aic.best)$hat))^2)
rsq.press <- sum((residuals(rsq.best) / (1 - lm.influence(rsq.best)$hat))^2)
stepaic.press <- sum((residuals(model.aic.null.both) / (1 - lm.influence(model.aic.null.both)$hat))^2)
number.of.variables=c(stepaic.number,rsq.x,adjr2.x,mse.x,aic.x,stepaic.number)
MSE=c(aic.mse,rsq.mse,MSE.mse,MSE.mse,aic.mse,stepaic.mse)
MSEP=c(aic.msep,rsq.msep,MSE.msep,MSE.msep,aic.msep,stepaic.msep)
PRESS.n=c(aic.press/40,rsq.press/40,MSE.press/40,MSE.press/40,aic.press/40,stepaic.press/40)
combined.frame=data.frame(rbind(number.of.variables,MSE,MSEP,PRESS.n),row.names = c('number.of.variables','MSE','MSEP','PRESS.n'))
names(combined.frame)=c('cp','rsq','adjr2','mse','aic','stepaic')
optimal.model=model.aic.null.both
qqnorm(stdres(optimal.model))
qqline(stdres(optimal.model))
plot(optimal.model$residuals, xlab = "Index", ylab = "Residual")
plot(optimal.model$fitted.values,optimal.model$residuals, xlab = "Fitted value", ylab = "Residual")
lines(lowess(optimal.model$residuals ~ optimal.model$fitted.values), col = "red")
plot(stdres(optimal.model), xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
cafepredict=read.table(file=file.choose(),header=TRUE)
predict(rsq.best, newdata = cafepredict)
prediction=predict(rsq.best, newdata = cafepredict,interval='prediction',level=0.99)
detach(cafe)
rm(list=ls())
#question 3
golf=read.table(file=file.choose(),header=TRUE)
attach(golf)
pairs(golf)
golf.linear.all=lm(X202624~.,data=golf)
corr=cor(golf)
summary(golf.linear.all)
correlation=matrix(nrow=12,ncol=12)

for(i in c(1:12)){for( j in c(1:12)){if(abs(corr[i,j])>0.6) correlation[i,j]=corr[i,j]}}
qqnorm(X202624)
qqline(X202624)
qqnorm(log(X202624))
qqline(log(X202624))
golf.linear.all.log=lm(log(X202624)~.,data=golf)

alpha <- 0.05
confinterval=confint(golf.linear.all.log, parm = c(5,8), level = 1 - alpha / 3)



library(ellipse)
alpha=0.05
plot(ellipse(golf.linear.all.log, which = c("X29.35", "X1.79"), level = 1 - alpha), type = "l")
points(golf.linear.all.log$coefficients["X29.35"], golf.linear.all.log$coefficients["X1.79"])
library(MASS)
qqnorm(stdres(golf.linear.all.log))
qqline(stdres(golf.linear.all.log))
plot(golf.linear.all.log$residuals, xlab = "Index", ylab = "Residual")
plot(golf.linear.all.log$fitted.values,golf.linear.all.log$residuals, xlab = "Fitted value", ylab = "Residual")
lines(lowess(golf.linear.all.log$residuals ~ golf.linear.all.log$fitted.values), col = "red")
plot(stdres(golf.linear.all.log), xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
library(lmtest)
bptest(golf.linear.all.log)
residuals=golf.linear.all.log$residuals
Weighted_fit <- rlm(log(X202624)~., data = golf, weights = 1/abs(residuals))
new.frame=data.frame(cbind(log(X202624),Weighted_fit$fitted.values,golf.linear.all.log$fitted.values))


plot(Weighted_fit$fitted.values,Weighted_fit$residuals, xlab = "Fitted value", ylab = "Residual")
lines(lowess(Weighted_fit$residuals ~ Weighted_fit$fitted.values), col = "red")
vif=solve(cor(golf[, 2:12]))
library(car)
vif2=vif(golf.linear.all.log)
corr.eigen=cor(golf[,-1])
v=eigen(corr.eigen)
rm(list=ls())
#question 4
dataset = read.table(file=file.choose(), header = T)

attach(dataset)

mean(dataset$x)
var(dataset$x)

model = lowess(dataset$x, dataset$y)

#1b
par(mfrow = c(1,1))
plot(dataset$x, dataset$y ,main="Lowess vs. Scatter", xlab = "x", ylab = "y")
for (s in 1:length(seq)){
  lines(lowess(dataset$x, dataset$y, f = seq[s]), col = colors[s])
}
legend(480, 0.3, col = colors, legend = seq, lty=1:2, cex=0.8)


#residuals
par(mfrow=c(2,2))

for (s in 1:length(seq)){
  model = lowess(dataset$x, dataset$y, f= seq[s])
  lfun <- approxfun(model)
  fitted <- lfun(dataset$x)
  resid <- dataset$y-fitted
  plot(fitted,resid, col = colors[s])
}


#C
y = dataset$y
x = dataset$x
mu = mean(x)
sd = sd(x)
A = 1/sqrt(2*pi)

m<-nls(y~(a/b)*exp(-(((x-c)^2)/2*(b^2))), start = list(a=A,b=,c=mu))