title: "Bayesian estimation of Weibull Shape Parameter"
author: "Matthew De Bie u18115943"
date: "20 June 2021"
output: html_document

#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
library(ggplot2)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

#SIM1 setup
n = 10000
beta = c(0.1,0.4)
alpha = 2
x = rnorm(n)
x = scale(x)
y = rweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha)
yy=as.data.frame(y)
data = list(y=y, x=x,n=n)
formula =y~x

#inla.doc("weibull")

#names(inla.models()$prior)
#names(inla.models()$likelihood)
#inla.doc('The Weibull Likelihood')

#gamma prior
params= c(0.1,0.5)
#where params are our covariate Betas.
for (j in 1:length(params)){
  count_gamma=0
  param1=params[j]
  prior.function = function(alpha,a) {
    theta=log(alpha)/0.1;
    logdens = log(a^a) - lgamma(a) + (a-1)*log(alpha) - a*alpha;
    log_jacobian = log(0.1)+0.1*theta;
    return(logdens + log_jacobian)
  }
}

alpha1 = seq(0.01, 5, len=1000)
prior.table = paste(c("table:", cbind(alpha1, prior.function(alpha1,param1))),
                    sep = "", collapse = " ")

result2<-inla(formula,family="weibull",
              control.family =list(hyper = list(theta=list(prior=prior.table)),
                                   variant = 1),data=data)
summary(result2)
est_alpha2= result2$summary.hyperpar$mean
fitsum=result2$summary.fitted.value

beta[1] 
beta[2]
#y=dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha2)

curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha2), from=0, to=50, col='blue', add=TRUE)
legend(25, 0.000025, legend=c("Simulated Curve", "Gamma Estimated Curve"),
       col=c("red", "blue"),lty=1, cex=.9)


#PC Prior
data = list(y=y, x=x,n=n)
formula =y~x
result1 = inla(formula, family = "weibull", data = data)
summary(result1) 

fit1=result1$summary.fitted.values
fit1mean=exp(result1$summary.fitted.values$mean)
fit1mean=as.data.frame(fit1mean)
LP=result1$summary.linear.predictor$mean
LP=exp(LP)
LP=as.data.frame(LP)
shape1=result1$summary.hyperpar$mean
shape1=as.data.frame(shape1)
shape1=shape1[1,]
shp1=data.frame(matrix(shape1, nrow = n, ncol = 1))
EX=LP*(gamma(1+1/shp1))

est_alpha1=result1$summary.hyperpar$mean
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha1), from=0, to=50, col='black', add=TRUE)
legend(25, 0.000025, legend=c("Simulated Curve", "PC Prior Estimated Curve"),
       col=c("red", "black"),lty=1, cex=.9)




#Sim 2
n = 10000
beta = c(0.4,0.75)
alpha = 0.95
x = rnorm(n)
x = scale(x)
y = rweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha)
data = list(y=y, x=x,n=n)
formula =y~x

#gamma prior
params= beta
#where params are our covariate Betas.
for (j in 1:length(params)){
  count_gamma=0
  param1=params[j]
  prior.function = function(alpha,a) {
    theta=log(alpha)/0.1;
    logdens = log(a^a) - lgamma(a) + (a-1)*log(alpha) - a*alpha;
    log_jacobian = log(0.1)+0.1*theta;
    return(logdens + log_jacobian)
  }
}

alpha1 = seq(0.01, 5, len=1000)
prior.table = paste(c("table:", cbind(alpha1, prior.function(alpha1,param1))),
                    sep = "", collapse = " ")

result2b<-inla(formula,family="weibull",
               control.family =list(hyper = list(theta=list(prior=prior.table)),
                                    variant = 1),data=data)
summary(result2b)
est_alpha2b= result2b$summary.hyperpar$mean

curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha2b), from=0, to=50, col='blue', add=TRUE)
legend(25, 0.000025, legend=c("Simulated Curve", "Gamma Estimated Curve"),
       col=c("red", "blue"),lty=1, cex=.9)

#PC Prior
data = list(y=y, x=x,n=n)
formula =y~x
result2a = inla(formula, family = "weibull", data = data)
summary(result2a) 

est_alpha2a=result2a$summary.hyperpar$mean
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,col='red',
      main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha2a), from=0, to=50, col='black', add=TRUE)
legend(25, 0.000025, legend=c("Simulated Curve", "PC Prior Estimated Curve"),
       col=c("red", "black"),lty=1, cex=.9)


#Sim 3
n = 10000
beta = c(0.8,0.8,0.66,0.55)
alpha = 0.98
x = rnorm(n)
x = scale(x)
y = rweibull(n, scale = exp(beta[1] + beta[2]*x +beta[3]*x + beta[4]*x), shape = alpha)
data = list(y=y, x=x,n=n)
formula =y~x

#gamma prior
params= beta
#where params are our covariate Betas.
for (j in 1:length(params)){
  count_gamma=0
  param1=params[j]
  prior.function = function(alpha,a) {
    theta=log(alpha)/0.1;
    logdens = log(a^a) - lgamma(a) + (a-1)*log(alpha) - a*alpha;
    log_jacobian = log(0.1)+0.1*theta;
    return(logdens + log_jacobian)
  }
}

alpha1 = seq(0.01, 5, len=1000)
prior.table = paste(c("table:", cbind(alpha1, prior.function(alpha1,param1))),
                    sep = "", collapse = " ")

result3b<-inla(formula,family="weibull",
               control.family =list(hyper = list(theta=list(prior=prior.table)),
                                    variant = 1),data=data)
summary(result3b)
est_alpha3b= result3b$summary.hyperpar$mean

curve(dweibull(n, scale = exp(beta[1] + beta[2]*x + beta[3]*x + beta[4]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x + beta[3]*x + beta[4]*x), shape = est_alpha3b), from=0, to=50, col='blue', add=TRUE)
legend(20, 0.000025, legend=c("Simulated Curve", "Gamma Estimated Curve"),
       col=c("red", "blue"),lty=1, cex=.9)

data = list(y=y, x=x,n=n)
formula =y~x
result3a = inla(formula, family = "weibull", data = data)
summary(result3a) 

est_alpha3a=result3a$summary.hyperpar$mean
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x  + beta[3]*x + beta[4]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x  + beta[3]*x + beta[4]*x), shape = est_alpha3a), from=0, to=50, col='black', add=TRUE)
legend(25, 0.000025, legend=c("Simulated Curve", "PC Prior Estimated Curve"),
       col=c("red", "black"),lty=1, cex=.9)

#MLE 
grd = 25
k=grd+1
matrix_index_alpha_1= seq(0,1, by=(1/grd))
a=matrix_index_alpha_1
matrix_index_B0_1 = matrix(0.1,nrow=grd,ncol=1) #keeping this parameter known

B0=matrix_index_B0_1

matrix_index_B1_1 =seq(0,1, by=(1/grd))

B1=matrix_index_B1_1

matrix_index_B0_1 = seq(0,1, by=(1/grd)) 
B0=matrix_index_B0_1

input_1 = cbind(rep(a, each = length(B1)), rep(B1, length(a)))
input_2_a =cbind(rep(a, each=length(B1)**2), rep(B1, each=length(a)))
input_2_b= rep(B0, length(B1)**2)
input2 = cbind(input_2_a,input_2_b)
q=nrow(input2)
head(input2)

loglike_3p = matrix(0,nrow=q,ncol=1)

for (i in 1:q){
  alpha = input2[i,1]
  lamba = input2[i,3] + (input2[i,2])
  loglike_3p[i,1] = sum(dweibull(y, scale = exp(input2[i,3] + input2[i,2]*x), shape = input2[i,1], log = TRUE))
}

loglike3p_remove = loglike_3p 
loglike3p_remove[is.nan(loglike3p_remove)] <- -Inf  # Remove NaN from vector
loglike_3p=loglike3p_remove

out=cbind(input2,loglike_3p)
locat1_a= max(loglike_3p)
locat1_a
locat2_a=which(out ==locat1_a, arr.ind = T)
locat2_a
PAR_3=out[locat2_a,]
PAR_3=PAR_3[1,]
PAR_3 


#SIM4 setup
n = 1000
beta = c(0.4,0.4)
alpha = 0.85
x = rnorm(n)
x = scale(x)
y = rweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha)
data = list(y=y, x=x,n=n)
formula =y~x


#gamma prior
params= beta
#where params are our covariate Betas.
for (j in 1:length(params)){
  count_gamma=0
  param1=params[j]
  prior.function = function(alpha,a) {
    theta=log(alpha)/0.1;
    logdens = log(a^a) - lgamma(a) + (a-1)*log(alpha) - a*alpha;
    log_jacobian = log(0.1)+0.1*theta;
    return(logdens + log_jacobian)
  }
}

alpha1 = seq(0.01, 5, len=1000)
prior.table = paste(c("table:", cbind(alpha1, prior.function(alpha1,param1))),
                    sep = "", collapse = " ")

result2d<-inla(formula,family="weibull",
              control.family =list(hyper = list(theta=list(prior=prior.table)),
                                   variant = 1),data=data)
summary(result2d)
est_alpha2d= result2d$summary.hyperpar$mean

curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,ylim=c(0,0.0004),col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha2d), from=0, to=50, col='blue', add=TRUE)

legend(25, 0.00025, legend=c("Simulated Curve", "Gamma Estimated Curve"),
       col=c("red", "blue"),lty=1, cex=.9)


#PC Prior
data = list(y=y, x=x,n=n)
formula =y~x
result1d = inla(formula, family = "weibull", data = data)
summary(result1d) 

est_alpha1d=result1$summary.hyperpar$mean
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha1d), from=0, to=50, col='black', add=TRUE)

legend(25, 0.00025, legend=c("Simulated Curve", "PC Prior Estimated Curve"),
       col=c("red", "black"),lty=1, cex=.9)