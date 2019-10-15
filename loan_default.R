#1.1

#getwd()

rm(list=ls())
fulldata.A = read.table(file=file.choose(), header = TRUE)
set.seed(488725)
rownumbers = sample(1:6436,size=1000)
mydata.A = fulldata.A[rownumbers,]

attach(mydata.A)
#attach(dataset)
#names(dataset)
par(mfrow=c(1,2))
plot(Customer, Loan,xlab='customer',ylab='loan')
plot(Gender, Loan,xlab='gender',ylab='loan')
boxplot(Loan~Customer*Gender)
#check for heteroskedasticy... try transforming the response into log
plot(Income, log(Loan))
plot(Age, log(Loan))

#ANOVA ... not needed but just for curiosity
plot(Customer, Loan)
plot(Gender, Loan)


#approximately normal errors, meaning we can use a regression model for continuous, 
#positive responses... 
library(MASS)


full_model = lm(log(Loan)~Income+I(Income^2)+I(Income^3)+Gender+Customer+Age + Income*Gender + Income*Customer
                + Income*Age + Gender*Customer + Gender*Age)

selection = stepAIC(object = full_model, scope = list(upper = ~ Income+I(Income^2)+I(Income^3)+Gender+Customer+Age + Income*Gender + Income*Customer
                                                      + Income*Age + Gender*Customer + Gender*Age
                                                      , lower = ~Income+Gender+Customer+Age), direction = "both")


round(selection$coefficients,4)
selected_model = lm(log(Loan)~Income+I(Income^2)
                    +I(Income^3)+Gender+Customer+Age + Income*Age)



summary(selected_model)
par(mfrow=c(2,2))
plot(selected_model)

#one of the considered models:
considered_model = lm(log(Loan)~Income+Gender+Customer+Age)
par(mfrow(2,2))
plot(considered_model)


#1.2

#split into 8 knots
N = 8
#fit the spline
spline_fit=spline(Age, y = Loan, n = N, method = "fmm",
                  xmin = min(Age), xmax = max(Age), ties = mean)
#x here will give the location of the knots
x = spline_fit$x
#y will give function values at those knots
y = spline_fit$y
#new window to plot in
dev.new(width=12, height=12)
plot(Age, Loan, col="gray", xlab="Age",ylab="Amount of Loan", main = paste("spline[fun](.) through", n=N, "points"))
lines(spline(Age, Loan, n = N), col = 2)
abline(v=x,lty=2,col="darkgreen")

#obtain the 4 coefficients of each of the cubic basis splines
coefficients = splines::interpSpline(spline_fit$x,spline_fit$y)$coef

#coefficients of basis functions for overall model
fit<-lm(Loan ~ bs(Age,knots = x),data = dataset)
summary(fit)

#derivative matrix... drop the first column (that's just intercepts of each cubic spline)
derivative_coefficients = coefficients [, 2:4]


#analytical derivative
install.packages("polynom")
library(polynom)


#obtaining coefficients for the splines, 1 coefficient for each spline
coefs_for_function = fit$coefficients
#scatter plot
plot(Age, Loan, col="gray", xlab="Age",ylab="Amount of Loan", main = paste("Derivative Plot"), ylim = c(0,15))
curve(coefs_for_function[2]*(derivative_coefficients[1, 1] + derivative_coefficients[1, 2]*x + derivative_coefficients[1, 3]*x^2), xlim = c(x[1],x[2]), add = T, col=2) 

curve(coefs_for_function[3]*(derivative_coefficients[2, 1] + derivative_coefficients[2, 2]*x + derivative_coefficients[2, 3]*x^2), xlim = c(x[2],x[3]), add = T, col=2)

curve(coefs_for_function[4]*(derivative_coefficients[3, 1] + derivative_coefficients[3, 2]*x + derivative_coefficients[3, 3]*x^2), xlim = c(x[3],x[4]), add = T, col=2)

curve(coefs_for_function[5]*(derivative_coefficients[4, 1] + derivative_coefficients[4, 2]*x + derivative_coefficients[4, 3]*x^2), xlim = c(x[4],x[5]), add = T, col=2)

curve(coefs_for_function[6]*(derivative_coefficients[5, 1] + derivative_coefficients[5, 2]*x + derivative_coefficients[5, 3]*x^2), xlim = c(x[5],x[6]), add = T, col=2)

curve(coefs_for_function[7]*(derivative_coefficients[6, 1] + derivative_coefficients[6, 2]*x + derivative_coefficients[6, 3]*x^2), xlim = c(x[6],x[7]), add = T, col=2)

curve(coefs_for_function[8]*(derivative_coefficients[7, 1] + derivative_coefficients[7, 2]*x + derivative_coefficients[7, 3]*x^2), xlim = c(x[7],x[8]), add = T, col=2)



#1.3
fit1binomial=glm(Default~Income+Loan+Age+Gender+Education+Children+Employment+Phone+Customer+Term+Limit,family=binomial(link='logit'))
null.model=glm(Default~1,family=binomial)
model.aic=stepAIC(null.model,scope=list(upper=fit1binomial,lower=~1),direction='forward')
model.aic$terms
round(model.aic$coefficients,4)
AIC(model.aic)
summary(model.aic)

#second order model:
secondorder.model=glm(Default~(Income+Loan+Age+Gender+Education+Children+Employment+Phone+Customer+Term+Limit)^2,family=binomial)
model.aic.secondorder=stepAIC(null.model,scope=list(upper=secondorder.model,lower=~1),direction='forward',family=binomial)
model.aic.secondorder$terms
round(model.aic.secondorder$coefficients,4)
AIC(model.aic.secondorder)
summary(model.aic.secondorder)

library(mgcv)

null.model = gam(Default ~ 1, family = binomial)
model1.gam =gam(Default~s(Income)+s(Loan)+Age+Gender+Education+Children+Employment+Phone+Customer+Term+Limit,family=binomial)
AIC(model1.gam)
summary(model1.gam)
round(model1.gam$coefficients,4)

#question 2a

fulldata.B=read.table(file=file.choose(),header=T)
set.seed(488725)
rownumbers.B = sample(unique(fulldata.B$Patient),80,replace=F)
mydata.B = fulldata.B[fulldata.B$Patient%in%rownumbers.B,]
library(hglm)
mydata.B.2=mydata.B[,-1]

#aic trial and error

fitB1 = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Heartfail+Heartrate+Age+Gender+Hypertn+Smoke+Diabetes+Highchol, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1$likelihood$cAIC
summary(fitB1)
fitB1.dropHypertn = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Heartfail+Heartrate+Age+Gender+Smoke+Diabetes+Highchol, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertn$likelihood$cAIC
summary(fitB1.dropHypertn)
fitB1.dropHypertnhrate = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Heartfail+Age+Gender+Smoke+Diabetes+Highchol, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertnhrate$likelihood$cAIC
summary(fitB1.dropHypertnhrate)
fitB1.dropHypertnhratesmoke = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Heartfail+Age+Gender+Diabetes+Highchol, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertnhratesmoke$likelihood$cAIC
summary(fitB1.dropHypertnhratesmoke)
fitB1.dropHypertnhratesmokehighchol = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Heartfail+Age+Gender+Diabetes, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertnhratesmokehighchol$likelihood$cAIC
summary(fitB1.dropHypertnhratesmokehighchol)
fitB1.dropHypertnhratesmokehighcholDiab = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Heartfail+Age+Gender, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertnhratesmokehighcholDiab$likelihood$cAIC
summary(fitB1.dropHypertnhratesmokehighcholDiab)
fitB1.dropHypertnhratesmokehighcholdiabAge = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Heartfail+Gender, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertnhratesmokehighcholdiabAge$likelihood$cAIC
summary(fitB1.dropHypertnhratesmokehighcholdiabAge)
fitB1.dropHypertnhratesmokehighcholdiabAgeheartfail = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress+Gender, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertnhratesmokehighcholdiabAgeheartfail$likelihood$cAIC
summary(fitB1.dropHypertnhratesmokehighcholdiabAgeheartfail)
fitB1.dropHypertnhratesmokehighcholdiabAgeheartfailGender = hglm(fixed = Enzyme~Hunthess+Heartperf+Bloodpress, random = ~1|Day, data=mydata.B.2,calc.lik=T, family = binomial(link='logit'), maxit=500)
fitB1.dropHypertnhratesmokehighcholdiabAgeheartfailGender$likelihood$cAIC
summary(fitB1.dropHypertnhratesmokehighcholdiabAgeheartfailGender)


#question 2b.



fulldata.B=read.table(file=file.choose(),header=T)
set.seed(488725)
rownumbers.B = sample(unique(fulldata.B$Patient),80,replace=F)
mydata.B = fulldata.B[fulldata.B$Patient%in%rownumbers.B,]
attach(mydata.B)
names(mydata.B)

install.packages("lmmlasso")
library(lmmlasso)


#fit a model and see what's significant:
model = lm(log(Heartperf) ~  Enzyme + Hunthess + Bloodpress+ Heartfail + Heartrate
           + Age + Gender + Hypertn + Smoke + Diabetes + Highchol)
summary(model)

#significant regressors are Hearfail  and Heartrate and Gender


x.matrix=as.matrix(cbind(1,mydata.B[,c("Heartfail","Heartrate", "Gender")]))
z = matrix(rep(1,nrow(mydata.B)),ncol=1)
y = Heartperf
colnames(z)="Intercept"
grp = mydata.B$Patient
mylambda=1
fitB2= lmmlasso(y=y,x=x.matrix,z=z,grp=grp,lambda=mylambda,pdMat="pdIdent")
fitB2$aic
summary(fitB2)


#now we define a sequence of potential lambdas to try
lambdas = seq(0, 10, 0.5)
vector_of_aics = c()
#obtain AIC for each model according to each lambda
for (i in 1:length(lambdas)){
  fit = lmmlasso(y=y,x=x.matrix,z=z,grp=grp,lambda=lambdas[i],pdMat="pdIdent")
  vector_of_aics[i] = fit$aic
}
plot(lambdas, vector_of_aics, xlab ="Lambda penalty", ylab = "AIC", main = "AIC as a function of Lambda")

#find index of minimum AIC:
index_min_aic = which.min(vector_of_aics)
index_min_aic


best_lambda = lambdas[index_min_aic]
best_lambda
#now summarize the model with the best lambda
fit_best_lambda = lmmlasso(y=y,x=x.matrix,z=z,grp=grp,lambda=best_lambda,pdMat="pdIdent")
summary(fit_best_lambda)
par(mfrow=c(1,1))
plot(fit_best_lambda$fitted.values, fit_best_lambda$residuals)
qqnorm(fit_best_lambda$residuals)
abli
summary(lm(y~x.matrix))



