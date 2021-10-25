library(INLA)
library(ggplot2)

#SIM setup
n = 10000
beta = c(0.1,0.5)
x = rnorm(n)
x = scale(x)
alpha = 0.8
y = rweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha)
hist(y)

#MLE
loglik = function(xx){
  beta = xx[1:2]
  alpha = xx[3]
  f = -sum(dweibull(y, scale = exp(beta[1] + beta[2]*x), shape = alpha, log = TRUE))
  return(f)
}

MLEs = c(0,0,1)
loglik(MLEs)

#Graph
loglik_a = function(a){
  return(loglik(c(beta,a)))
}

plot(seq(0.01,1.5,by = 0.01), apply(matrix(seq(0.01,1.5,by = 0.01)), 1, loglik_a), type = "l",
     main = "-LogLik (conditional) of alpha",
     xlab = expression(alpha),
     ylab = expression(paste("-log(f(",alpha,"|y)")))

MLEs = optim(MLEs, loglik)
MLEs$par 



index = seq(0, 1, by=.005)
index

#For Shape Parameter
R = length(index)
loglike = double(R)
for(r in 1:R){
  loglike[r] = sum(dweibull(y, scale = exp(beta[1] + beta[2]*x), shape = index[r], log = TRUE))
}

plot(index, loglike, type="l")
abline(v=index[which.max(loglike)], col=2, lty=2)

index[which.max(loglike)]

#For Scale Parameter [1]
R = length(index)
loglike2 = double(R)
for(r in 1:R){
  loglike2[r] = sum(dweibull(y, scale = exp(index[r] +beta[2]*x), shape = alpha, log = TRUE))
}

index[which.max(loglike2)]

#For Scale Parameter [2]
R = length(index)
loglike3 = double(R)
for(r in 1:R){
  loglike3[r] = sum(dweibull(y, scale = exp(beta[1] +index[r]*x), shape = alpha, log = TRUE))
}

index[which.max(loglike3)]



#Attempt 2 of Grid Search: Finding all combinations
n = 25
k=n+1
matrix_index_alpha_1= seq(0,1, by=(1/n))
a=matrix_index_alpha_1
matrix_index_B0_1 = matrix(0.1,nrow=n,ncol=1) #keeping this parameter known

B0=matrix_index_B0_1

matrix_index_B1_1 =seq(0,1, by=(1/n))

B1=matrix_index_B1_1

matrix_index_1= cbind(a,B0,B1)
matrix_index_1
input=as.data.frame(matrix_index_1)
input


input_1 = cbind(rep(a, each = length(B1)), rep(B1, length(a)))
input1 = cbind(input_1, rep(beta[1], nrow(input_1)))
q=nrow(input1)
head(input1)

loglike_twoUP = matrix(0, nrow=q, ncol=1)
loglike_int = matrix(0,nrow=q,ncol=1)
loglike_4 = matrix(0,nrow=q,ncol=1)

for (i in 1:q){
  alpha = input1[i,1]
  lamba = input1[i,3] + (input1[i,2])
  loglike_4[i,1] = sum(dweibull(y, scale = exp(input1[i,3] + input1[i,2]*x), shape = input1[i,1], log = TRUE))
}

loglike4_remove = loglike_4 
loglike4_remove[is.nan(loglike4_remove)] <- -Inf  # Remove NaN from vector
loglike_4=loglike4_remove
out=cbind(input1,loglike_4)


locat1= max(loglike_4)
locat1
locat2=which(out ==locat1, arr.ind = T)
locat2
PAR=out[locat2,]
PAR=PAR[1,]
PAR 
#Above Par is for Two unknown parameters


matrix_index_B0_1 = seq(0,1, by=(1/n)) 
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
#For 3 unknown parameters.

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

curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha3b), from=0, to=50, col='blue', add=TRUE)
legend(20, 0.000025, legend=c("Simulated Curve", "Gamma Estimated Curve"),
       col=c("red", "blue"),lty=1, cex=.9)

data = list(y=y, x=x,n=n)
formula =y~x
result3a = inla(formula, family = "weibull", data = data)
summary(result3a) 

est_alpha3a=result3a$summary.hyperpar$mean
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = alpha), from=0, to=50,col='red',main = 'Weibull Distribution', ylab = 'Density')
curve(dweibull(n, scale = exp(beta[1] + beta[2]*x), shape = est_alpha3a), from=0, to=50, col='black', add=TRUE)
legend(25, 0.000025, legend=c("Simulated Curve", "PC Prior Estimated Curve"),
       col=c("red", "black"),lty=1, cex=.9)



