library(INLA)
library(ggplot2)
library(readr)
library(weibulltools)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()


master_dataframe <- read_csv("1 UNI STUFF/Honours Research/Data and Cleaning/All Wind Speeds At All Altitudes.csv")
#View(All_Wind_Speeds_At_All_Altitudes)
nn=nrow(master_dataframe)
q=50000
sample_Q=master_dataframe[sample(nn, q), ]

#Simple model
#eta_{i} = Beta_{0} + Beta_{1}*ALT_{q} + v_{i}
#data set up
#we have to define the design matrix (the X matrix)
#we have that lambda = exp(B0+B1(Alt)...)
#we are modelling wind speed as a function of the unknown shape parameter
#and as a function of lambda (which is specified in the paper)

q=nrow(sample_Q)
n=q
print(n)
mon=12
#V is just a random month effect
Samp_month=sample_Q$Month

Month_num=data.frame(matrix(1:mon, nrow = mon, ncol = 1))
Month_nam=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
Month=cbind(Month_num,Month_nam)
Month=as.matrix(Month)

Month_num=as.matrix(Month_num)
#Month_rand=as.matrix(Month_rand)
Samp_month=as.matrix(Samp_month)

month_number=matrix(1, nrow = n, ncol = 1)

actual=sample_Q$Wind_Speed
actual=as.data.frame(actual)
nn=nrow(actual)
obs=data.frame(matrix(1:nn, nrow = nn, ncol = 1))
actual=cbind(obs,actual)
actual_names=c("obs","actual_wind_speeds")
names(actual)=actual_names
#plot(actual, main = "Actual Wind Speeds From Sample", type = "s",col="black")







f_month <- sample_Q$Month
f_month[f_month == "JAN"] = 1
f_month[f_month == "FEB"] = 2
f_month[f_month == "MAR"] = 3
f_month[f_month == "APR"] = 4
f_month[f_month == "MAY"] = 5
f_month[f_month == "JUN"] = 6
f_month[f_month == "JUL"] = 7
f_month[f_month == "AUG"] = 8
f_month[f_month == "SEP"] = 9
f_month[f_month == "OCT"] = 10
f_month[f_month == "NOV"] = 11
f_month[f_month == "DEC"] = 12

sample_Q$f_month = f_month



#making months numerical for use in later models
sample_Q=cbind(sample_Q,month_number)
n=nrow(sample_Q)






#Model1:Basic Model;
formula_1= Wind_Speed ~ 1 + Altitude
result1 = inla(formula = formula_1, family = "Weibull",
               control.family= list(variant="1"), data = sample_Q,
               inla.mode = "experimental",
               control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, waic = TRUE))
summary(result1)



#Summary of Fitted Values from Input Data (Summary of Linear Predictor Component)
fit1=result1$summary.fitted.values
fit1mean=exp(result1$summary.fitted.values$mean)
fit1mean=as.data.frame(fit1mean)

#Estimated Shape Parameter
shape1=result1$summary.hyperpar$mean
shape1=as.data.frame(shape1)
shape1=shape1[1,]


#Marginal Plots of Fixed and Hyper parameters
plot(result1$marginals.fixed$Altitude, 
     main = "Posterior Density of Altitude Model 1", type = "l",col="black")

plot(result1$marginals.hyperpar$`alpha parameter for weibull`, 
     main= "Density of Hyper Parameter Model 1",type="l",col="blue")

#Model Selection Criteria
eval1dic=result1$dic$dic
eval1waic=result1$waic$waic
eval1marg=result1$mlik
eval1marg=eval1marg[1,]
eval1marg=as.numeric(eval1marg)

#Model 2: Spline Model for Altitude
#Altitude Jittering Effect:
sample_Q$Altitude <- sample_Q$Altitude + rnorm(q, mean = 0, sd = 4)
n=nrow(sample_Q)

formula_2 = Wind_Speed ~ 1 + f(inla.group(Altitude, n = 30), model = "rw2") 
result2 = inla(formula = formula_2, family = "Weibull",
               control.family= list(variant="1"), data = sample_Q,
               inla.mode = "experimental",
               control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, waic = TRUE))
summary(result2)



#Summary of Fitted Values from Input Data (Summary of Linear Predictor Component)
fit2=result2$summary.fitted.values
fit2mean=exp(result2$summary.fitted.values$mean)
fit2mean=as.data.frame(fit2mean)





#Estimated Shape Parameter
shape2=result2$summary.hyperpar$mean
shape2=as.data.frame(shape2)
shape2=shape2[1,]

result2$summary.random$`inla.group(Altitude, n = 30)`
Alt_ID=result2$summary.random$`inla.group(Altitude, n = 30)`[,1]
Alt_mean=result2$summary.random$`inla.group(Altitude, n = 30)`[,2]
Alt_0.025=result2$summary.random$`inla.group(Altitude, n = 30)`[,4]
Alt_0.975=result2$summary.random$`inla.group(Altitude, n = 30)`[,6]

plot(result2$summary.random$`inla.group(Altitude, n = 30)`[,1],
     result2$summary.random$`inla.group(Altitude, n = 30)`[,2], type = "l",
     main= "Posterior Density of Altitude With Spline Model 2")
lines(Alt_ID,Alt_0.025, col="Red")
lines(Alt_ID,Alt_0.975, col="Red")
legend(50,0.13, legend=c("0.975 Credible Interval", "Mean","0.025 Credible Interval"),
       col=c("red", "black","red"), lty=1:2, cex=0.8)



plot(result2$marginals.hyperpar$`alpha parameter for weibull`, type = "l",col="blue",
     main= "Posterior Density of Shape Parameter Estimated by way of PC Prior Model 2 ")

#Model Selection Criteria
eval2dic=result2$dic$dic
eval2waic=result2$waic$waic
eval2marg=result2$mlik
eval2marg=eval2marg[1,]
eval2marg=as.numeric(eval2marg)




#Model3a:Month Unstructured Effect and Spline Model for Altitude
#We assume that not every Altitude recording was 100% exact at that altitude
#Altitude Jittering Effect
sample_Q$Altitude <- sample_Q$Altitude + rnorm(q, mean = 0, sd = 4)


formula_3a = Wind_Speed ~ 1 + f(inla.group(Altitude, n = 30), model = "rw2") + f(Month, model = "iid")
result3a = inla(formula = formula_3a, family = "Weibull",
               control.family= list(variant="1"), data = sample_Q,
               inla.mode = "experimental",
               control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, waic = TRUE))
summary(result3a)



#Summary of Fitted Values from Input Data (Summary of Linear Predictor Component)
fit3a=result3a$summary.fitted.values
fit3amean=exp(result3a$summary.fitted.values$mean)
fit3amean=as.data.frame(fit3amean)

#Estimated Shape Parameter
shape3a=result3a$summary.hyperpar$mean
shape3a=as.data.frame(shape3a)
shape3a=shape3a[1,]

result3a$summary.random$`inla.group(Altitude, n = 30)`
Alt_ID=result3a$summary.random$`inla.group(Altitude, n = 30)`[,1]
Alt_mean=result3a$summary.random$`inla.group(Altitude, n = 30)`[,2]
Alt_0.025=result3a$summary.random$`inla.group(Altitude, n = 30)`[,4]
Alt_0.975=result3a$summary.random$`inla.group(Altitude, n = 30)`[,6]

plot(result3a$summary.random$`inla.group(Altitude, n = 30)`[,1],
     result3a$summary.random$`inla.group(Altitude, n = 30)`[,2], type = "l",
     main= "Posterior Density of Altitude With Spline Model 2")
lines(Alt_ID,Alt_0.025, col="Red")
lines(Alt_ID,Alt_0.975, col="Red")
legend(50,0.13, legend=c("0.975 Credible Interval", "Mean","0.025 Credible Interval"),
       col=c("red", "black","red"), lty=1:2, cex=0.8)


result3a$summary.random$Month
#Note, Months are not recorded in the proper order, must adjust in order to AR1
plot(result3a$summary.random$Month[,2],
     main= "Random Month Effect Model 2 ")

plot(result3a$marginals.hyperpar$`alpha parameter for weibull`, type = "l",col="blue",
     main= "Posterior Density of Shape Parameter Estimated by way of PC Prior Model 2 ")

#Model Selection Criteria
eval3adic=result3a$dic$dic
eval3awaic=result3a$waic$waic
eval3amarg=result3a$mlik
eval3amarg=eval3amarg[1,]
eval3amarg=as.numeric(eval3amarg)






#Model 3b:AR1 Model for Month and Spline for Altitude
#Note, the use of Month_number now ensures that the Months are correctly ordered from 1-12
#where 1=Jan, 2=Feb...
formula_3b = Wind_Speed ~ 1 + f(inla.group(Altitude, n = 30), model = "rw2") + f(month_number, model = "ar1")
result3b = inla(formula = formula_3b, family = "Weibull",
               control.family= list(variant="1"), data = sample_Q,
               inla.mode = "experimental",
               control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, waic = TRUE))
summary(result3b)
result3b$summary.random$month_number



#Summary of Fitted Values from Input Data (Summary of Linear Predictor Component)
fit3b=result3b$summary.fitted.values
fit3bmean=exp(result3b$summary.fitted.values$mean)
fit3bmean=as.data.frame(fit3bmean)

#Estimated Shape Parameter
shape3b=result3b$summary.hyperpar$mean
shape3b=as.data.frame(shape3b)
shape3b=shape3b[1,]


result3b$summary.random$`inla.group(Altitude, n = 30)`
Alt_ID=result3b$summary.random$`inla.group(Altitude, n = 30)`[,1]
Alt_mean=result3b$summary.random$`inla.group(Altitude, n = 30)`[,2]
Alt_0.025=result3b$summary.random$`inla.group(Altitude, n = 30)`[,4]
Alt_0.975=result3b$summary.random$`inla.group(Altitude, n = 30)`[,6]

plot(result3b$summary.random$`inla.group(Altitude, n = 30)`[,1],
     result3b$summary.random$`inla.group(Altitude, n = 30)`[,2], type = "l",
     main= "Posterior Density of Altitude With Spline Model 3")
lines(Alt_ID,Alt_0.025, col="Red")
lines(Alt_ID,Alt_0.975, col="Red")
legend(50,0.13, legend=c("0.975 Credible Interval", "Mean","0.025 Credible Interval"),
       col=c("red", "black","red"), lty=1:2, cex=0.8)


result3b$summary.random$month_number
rho_month=result3b$marginals.hyperpar$`Rho for month_number`

plot(rho_month, type = "l",col="orange",
     main= "Posterior Density of Month AR1")

#Note, Months are now recorded in the proper order
plot(result3b$summary.random$month_number[,2], type= "l",
     main= "AR(1) Month Effect Model 3b ", ylab="Month Effect",
     xlab="Month",col="purple",
     cex.axis=1)

plot(result3b$marginals.hyperpar$`alpha parameter for weibull`, type = "l",col="blue",
     main= "Posterior Density of Shape Parameter Estimated by way of PC Prior Model 3b")

#Model Selection Criteria
eval3bdic=result3b$dic$dic
eval3bwaic=result3b$waic$waic
eval3bmarg=result3b$mlik
eval3bmarg=eval3bmarg[1,]
eval3bmarg=as.numeric(eval3bmarg)









#obtaining predictions:
#E(X)=bΓ(1+1/a)
#LP=result2$summary.linear.predictor$mean
#LP=exp(LP)
#LP=as.data.frame(LP)
#shape2=result2$summary.hyperpar$mean
#shape2=as.data.frame(shape2)
#shape2=shape2[1,]
#shp2=data.frame(matrix(shape2, nrow = n, ncol = 1))
#EX=LP*(gamma(1+1/shp2))







