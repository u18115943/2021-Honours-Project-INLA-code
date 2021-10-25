rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

library(janitor)
library(readxl)
library(readr)
library(dplyr)
library('rio')
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape)
library(reshape2)
library(tibble)

setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/1 JAN")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\1 JAN",pattern="*.csv")
myfiles=lapply(files,read.delim)
JAN= do.call(rbind, lapply
               (files, read.csv, as.is=T, skip = 18, header = FALSE))
JAN=tibble(JAN)
q=row_to_names(JAN,1)
JAN=row_to_names(JAN,1)
JAN=subset(JAN, date_time!="date_time")

JAN[, 3:47] <- sapply(JAN[, 3:47], as.numeric)
head(JAN)
class(JAN$` WS_62_mean`)



JANS1=subset(JAN,` Station_ID`==" WM01")
JANS2=subset(JAN,` Station_ID`==" WM02")
JANS3=subset(JAN,` Station_ID`==" WM03")
JANS4=subset(JAN,` Station_ID`==" WM04")
JANS5=subset(JAN,` Station_ID`==" WM05")
JANS6=subset(JAN,` Station_ID`==" WM06")
JANS7=subset(JAN,` Station_ID`==" WM07")
JANS8=subset(JAN,` Station_ID`==" WM08")
JANS9=subset(JAN,` Station_ID`==" WM09")
JANS10=subset(JAN,` Station_ID`!=" WM01"  & ` Station_ID`!=" WM02" & 
                ` Station_ID`!=" WM03" & ` Station_ID`!=" WM04" &
                ` Station_ID`!=" WM05" & ` Station_ID`!=" WM06" &
                ` Station_ID`!=" WM07" & ` Station_ID`!=" WM08" &
                ` Station_ID`!=" WM09")
#Site 10 only began recording on the 17th of Jan, all prior =NULL




n=nrow(JANS6)
c=ncol(JANS6)
vec=matrix(nrow=1,ncol=c, "NA")

df=SEPS9=data.frame(matrix(NA_integer_, nrow = 1, ncol = ncol(SEPS1)))
names(df)=names(q)
JANS6=rbind(df,JANS6)


JAN_WS_62_mean=cbind.data.frame(JANS1$date_time,JANS1$` WS_62_mean`,JANS2$` WS_62_mean`, JANS3$` WS_62_mean`,
                       JANS4$` WS_62_mean`,JANS5$` WS_62_mean`,JANS6$` WS_62_mean`,
                       JANS7$` WS_62_mean`,JANS8$` WS_62_mean`,JANS9$` WS_62_mean`,
                       JANS10$` WS_62_mean`
  
)
names(JAN_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                    "Site6","Site7","Site8","Site9","Site10")
class(WS_62_mean$Site6)
JAN_WS_62_mean[, 2:11] <- sapply(JAN_WS_62_mean[, 2:11], as.numeric)

JAN_WS_62_mean1_5= JAN_WS_62_mean[,2:6]

JAN_WS_62_mean6= JAN_WS_62_mean[,7]
JAN_WS_62_mean6= subset(JAN_WS_62_mean6, JAN_WS_62_mean6 > 0 ) 

JAN_WS_62_mean7_9= JAN_WS_62_mean[,8:10]

JAN_WS_62_mean10 = JAN_WS_62_mean[,11]
JAN_WS_62_mean10= subset(JAN_WS_62_mean10, JAN_WS_62_mean10 > 0 ) 

#Obtain mean of each column at 62 meters
WS_62_JAN1_5=colMeans(JAN_WS_62_mean1_5[sapply(JAN_WS_62_mean1_5, is.numeric)])
WS_62_JAN6=mean(JAN_WS_62_mean6)
WS_62_JAN7_9=colMeans(JAN_WS_62_mean7_9[sapply(JAN_WS_62_mean7_9, is.numeric)])
WS_62_JAN10=mean(JAN_WS_62_mean10)


WS_62_Site_mean_val=c(WS_62_JAN1_5,WS_62_JAN6,WS_62_JAN7_9,WS_62_JAN10)
JAN_62_means=WS_62_Site_mean_val
WS_62_Site_mean_name=c("Site1","Site2","Site3","Site4","Site5",
                    "Site6","Site7","Site8","Site9","Site10")
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)
WS_62_Site_mean_bind= rbind(WS_62_Site_mean_name,WS_62_Site_num,WS_62_Site_mean_val)
WS_62_Site_meandf=data.frame(WS_62_Site_mean_bind)
JAN_WS_62_Site_meandf=data.frame(WS_62_Site_num,WS_62_Site_mean_val)


JAN_plt=ggplot(JAN_WS_62_Site_meandf, aes(WS_62_Site_num, JAN_62_means))+
  geom_point(col = "red") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(JAN_plt + ggtitle("January 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))





##FEBRUARY
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/2 FEB")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\2 FEB",pattern="*.csv")
myfiles=lapply(files,read.delim)
FEB= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
FEB=tibble(FEB)
FEB=row_to_names(FEB,1)
FEB=subset(FEB, date_time!="date_time")

FEB[, 3:47] <- sapply(FEB[, 3:47], as.numeric)
head(FEB)
class(FEB$` WS_62_mean`)

FEBS1=subset(FEB,` Station_ID`==" WM01")
FEBS2=subset(FEB,` Station_ID`==" WM02")
FEBS3=subset(FEB,` Station_ID`==" WM03")
FEBS4=subset(FEB,` Station_ID`==" WM04")
FEBS5=subset(FEB,` Station_ID`==" WM05")
FEBS6=subset(FEB,` Station_ID`==" WM06")
FEBS7=subset(FEB,` Station_ID`==" WM07")
FEBS8=subset(FEB,` Station_ID`==" WM08")
FEBS9=subset(FEB,` Station_ID`==" WM09")
FEBS10=subset(FEB, ` Station_ID`==" WM10") 

FEB_WS_62_mean=cbind.data.frame(FEBS1$date_time,FEBS1$` WS_62_mean`,FEBS2$` WS_62_mean`, FEBS3$` WS_62_mean`,
                            FEBS4$` WS_62_mean`,FEBS5$` WS_62_mean`,FEBS6$` WS_62_mean`,
                            FEBS7$` WS_62_mean`,FEBS8$` WS_62_mean`,FEBS9$` WS_62_mean`,
                            FEBS10$` WS_62_mean`
                            
)
names(FEB_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                    "Site6","Site7","Site8","Site9","Site10")

FEB_62_means=colMeans(FEB_WS_62_mean[sapply(FEB_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

FEB_WS_62_Site_meandf=data.frame(WS_62_Site_num,FEB_62_means)


FEB_plt=ggplot(FEB_WS_62_Site_meandf, aes(WS_62_Site_num, FEB_62_means))+
  geom_point(col = "blue") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(FEB_plt + ggtitle("February 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))




##MARCH
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/3 MAR")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\3 MAR",pattern="*.csv")
myfiles=lapply(files,read.delim)
MAR= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
MAR=tibble(MAR)
MAR=row_to_names(MAR,1)
MAR=subset(MAR, date_time!="date_time")

MAR[, 3:47] <- sapply(MAR[, 3:47], as.numeric)
head(MAR)
class(MAR$` WS_62_mean`)

MARS1=subset(MAR,` Station_ID`==" WM01")
MARS2=subset(MAR,` Station_ID`==" WM02")
MARS3=subset(MAR,` Station_ID`==" WM03")
MARS4=subset(MAR,` Station_ID`==" WM04")
MARS5=subset(MAR,` Station_ID`==" WM05")
MARS6=subset(MAR,` Station_ID`==" WM06")
MARS7=subset(MAR,` Station_ID`==" WM07")
MARS8=subset(MAR,` Station_ID`==" WM08")
MARS9=subset(MAR,` Station_ID`==" WM09")
MARS10=subset(MAR, ` Station_ID`==" WM10") 

MAR_WS_62_mean=cbind.data.frame(MARS1$date_time,MARS1$` WS_62_mean`,MARS2$` WS_62_mean`, MARS3$` WS_62_mean`,
                                MARS4$` WS_62_mean`,MARS5$` WS_62_mean`,MARS6$` WS_62_mean`,
                                MARS7$` WS_62_mean`,MARS8$` WS_62_mean`,MARS9$` WS_62_mean`,
                                MARS10$` WS_62_mean`
                                
)
names(MAR_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

MAR_62_means=colMeans(MAR_WS_62_mean[sapply(MAR_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)
#NOTE: SECOND HALF OF MONTH WAS NOT RECORDED BY SITE 10
MAR_WS_62_Site_meandf=data.frame(WS_62_Site_num,MAR_62_means)


MAR_plt=ggplot(MAR_WS_62_Site_meandf, aes(WS_62_Site_num, MAR_62_means))+
  geom_point(col = "green") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(MAR_plt + ggtitle("March 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))




##APRIL
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/4 APR")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\4 APR",pattern="*.csv")
myfiles=lapply(files,read.delim)
APR= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
APR=tibble(APR)
APR=row_to_names(APR,1)
APR=subset(APR, date_time!="date_time")

APR[, 3:47] <- sapply(APR[, 3:47], as.numeric)
head(APR)
class(APR$` WS_62_mean`)

APRS1=subset(APR,` Station_ID`==" WM01")
APRS2=subset(APR,` Station_ID`==" WM02")
APRS3=subset(APR,` Station_ID`==" WM03")
APRS4=subset(APR,` Station_ID`==" WM04")
APRS5=subset(APR,` Station_ID`==" WM05")
APRS6=subset(APR,` Station_ID`==" WM06")
APRS7=subset(APR,` Station_ID`==" WM07")
APRS8=subset(APR,` Station_ID`==" WM08")
APRS9=subset(APR,` Station_ID`==" WM09")
APRS10=subset(APR, ` Station_ID`==" WM10") 

APR_WS_62_mean=cbind.data.frame(APRS1$date_time,APRS1$` WS_62_mean`,APRS2$` WS_62_mean`, APRS3$` WS_62_mean`,
                                APRS4$` WS_62_mean`,APRS5$` WS_62_mean`,APRS6$` WS_62_mean`,
                                APRS7$` WS_62_mean`,APRS8$` WS_62_mean`,APRS9$` WS_62_mean`,
                                APRS10$` WS_62_mean`
                                
)
names(APR_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

APR_62_means=colMeans(APR_WS_62_mean[sapply(APR_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

APR_WS_62_Site_meandf=data.frame(WS_62_Site_num,APR_62_means)


APR_plt=ggplot(APR_WS_62_Site_meandf, aes(WS_62_Site_num, APR_62_means))+
  geom_point(col = "purple") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(APR_plt + ggtitle("April 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))




##MAY
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/5 MAY")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\5 MAY",pattern="*.csv")
myfiles=lapply(files,read.delim)
MAY= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
MAY=tibble(MAY)
MAY=row_to_names(MAY,1)
MAY=subset(MAY, date_time!="date_time")

MAY[, 3:47] <- sapply(MAY[, 3:47], as.numeric)
head(MAY)
class(MAY$` WS_62_mean`)

MAYS1=subset(MAY,` Station_ID`==" WM01")
MAYS2=subset(MAY,` Station_ID`==" WM02")
MAYS3=subset(MAY,` Station_ID`==" WM03")
MAYS4=subset(MAY,` Station_ID`==" WM04")
MAYS5=subset(MAY,` Station_ID`==" WM05")
MAYS6=subset(MAY,` Station_ID`==" WM06")
MAYS7=subset(MAY,` Station_ID`==" WM07")
MAYS8=subset(MAY,` Station_ID`==" WM08")
MAYS9=subset(MAY,` Station_ID`==" WM09")
MAYS10=subset(MAY, ` Station_ID`==" WM10") 

MAY_WS_62_mean=cbind.data.frame(MAYS1$date_time,MAYS1$` WS_62_mean`,MAYS2$` WS_62_mean`, MAYS3$` WS_62_mean`,
                                MAYS4$` WS_62_mean`,MAYS5$` WS_62_mean`,MAYS6$` WS_62_mean`,
                                MAYS7$` WS_62_mean`,MAYS8$` WS_62_mean`,MAYS9$` WS_62_mean`,
                                MAYS10$` WS_62_mean`
                                
)
names(MAY_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

MAY_62_means=colMeans(MAY_WS_62_mean[sapply(MAY_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

MAY_WS_62_Site_meandf=data.frame(WS_62_Site_num,MAY_62_means)


MAY_plt=ggplot(MAY_WS_62_Site_meandf, aes(WS_62_Site_num, MAY_62_means))+
  geom_point(col = "orange") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(MAY_plt + ggtitle("May 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))



##JUNE
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/6 JUN")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\6 JUN",pattern="*.csv")
myfiles=lapply(files,read.delim)
JUN= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
JUN=tibble(JUN)
JUN=row_to_names(JUN,1)
JUN=subset(JUN, date_time!="date_time")

JUN[, 3:47] <- sapply(JUN[, 3:47], as.numeric)
head(JUN)
class(JUN$` WS_62_mean`)

JUNS1=subset(JUN,` Station_ID`==" WM01")
JUNS2=subset(JUN,` Station_ID`==" WM02")
JUNS3=subset(JUN,` Station_ID`==" WM03")
JUNS4=subset(JUN,` Station_ID`==" WM04")
JUNS5=subset(JUN,` Station_ID`==" WM05")
JUNS6=subset(JUN,` Station_ID`==" WM06")
JUNS7=subset(JUN,` Station_ID`==" WM07")
JUNS8=subset(JUN,` Station_ID`==" WM08")
JUNS9=subset(JUN,` Station_ID`==" WM09")
JUNS10=subset(JUN, ` Station_ID`==" WM10") 

JUN_WS_62_mean=cbind.data.frame(JUNS1$date_time,JUNS1$` WS_62_mean`,JUNS2$` WS_62_mean`, JUNS3$` WS_62_mean`,
                                JUNS4$` WS_62_mean`,JUNS5$` WS_62_mean`,JUNS6$` WS_62_mean`,
                                JUNS7$` WS_62_mean`,JUNS8$` WS_62_mean`,JUNS9$` WS_62_mean`,
                                JUNS10$` WS_62_mean`
                                
)
names(JUN_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

JUN_62_means=colMeans(JUN_WS_62_mean[sapply(JUN_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

JUN_WS_62_Site_meandf=data.frame(WS_62_Site_num,JUN_62_means)

#NOTE: SITE 2, SITE 5 and SITE 10 had NA in recordings.
JUN_plt=ggplot(JUN_WS_62_Site_meandf, aes(WS_62_Site_num, JUN_62_means))+
  geom_point(col = "black") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(JUN_plt + ggtitle("June 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))




##JULY
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/7 JUL")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\7 JUL",pattern="*.csv")
myfiles=lapply(files,read.delim)
JUL= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
JUL=tibble(JUL)
JUL=row_to_names(JUL,1)
JUL=subset(JUL, date_time!="date_time")

JUL[, 3:47] <- sapply(JUL[, 3:47], as.numeric)
head(JUL)
class(JUL$` WS_62_mean`)

JULS1=subset(JUL,` Station_ID`==" WM01")
JULS2=subset(JUL,` Station_ID`==" WM02")
JULS3=subset(JUL,` Station_ID`==" WM03")
JULS4=subset(JUL,` Station_ID`==" WM04")
JULS5=subset(JUL,` Station_ID`==" WM05")
JULS6=subset(JUL,` Station_ID`==" WM06")
JULS7=subset(JUL,` Station_ID`==" WM07")
JULS8=subset(JUL,` Station_ID`==" WM08")
JULS9=subset(JUL,` Station_ID`==" WM09")
JULS10=subset(JUL, ` Station_ID`==" WM10") 

JUL_WS_62_mean=cbind.data.frame(JULS1$date_time,JULS1$` WS_62_mean`,JULS2$` WS_62_mean`, JULS3$` WS_62_mean`,
                                JULS4$` WS_62_mean`,JULS5$` WS_62_mean`,JULS6$` WS_62_mean`,
                                JULS7$` WS_62_mean`,JULS8$` WS_62_mean`,JULS9$` WS_62_mean`,
                                JULS10$` WS_62_mean`
                                
)
names(JUL_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

JUL_62_means=colMeans(JUL_WS_62_mean[sapply(JUL_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

JUL_WS_62_Site_meandf=data.frame(WS_62_Site_num,JUL_62_means)

#Note: SITE 9 had recording failures towards month end.
JUL_plt=ggplot(JUL_WS_62_Site_meandf, aes(WS_62_Site_num, JUL_62_means))+
  geom_point(col = "brown") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(JUL_plt + ggtitle("July 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))



##AUGUST
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/8 AUG")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\8 AUG",pattern="*.csv")
myfiles=lapply(files,read.delim)
AUG= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
AUG=tibble(AUG)
AUG=row_to_names(AUG,1)
AUG=subset(AUG, date_time!="date_time")

AUG[, 3:47] <- sapply(AUG[, 3:47], as.numeric)
head(AUG)
class(AUG$` WS_62_mean`)

AUGS1=subset(AUG,` Station_ID`==" WM01")
AUGS2=subset(AUG,` Station_ID`==" WM02")
AUGS3=subset(AUG,` Station_ID`==" WM03")
AUGS4=subset(AUG,` Station_ID`==" WM04")
AUGS5=subset(AUG,` Station_ID`==" WM05")
AUGS6=subset(AUG,` Station_ID`==" WM06")
AUGS7=subset(AUG,` Station_ID`==" WM07")
AUGS8=subset(AUG,` Station_ID`==" WM08")
AUGS9=subset(AUG,` Station_ID`==" WM09")
AUGS9_ACC=subset(AUGS9,` WS_62_mean`>0)
AUG9_mean=mean(AUGS9_ACC$` WS_62_mean`)
AUGS10=subset(AUG, ` Station_ID`==" WM10") 
AUGS10_ACC=subset(AUGS10,` WS_62_mean`>0)
AUG10_mean=mean(AUGS10_ACC$` WS_62_mean`)


AUG_WS_62_mean=cbind.data.frame(AUGS1$date_time,AUGS1$` WS_62_mean`,AUGS2$` WS_62_mean`, AUGS3$` WS_62_mean`,
                                AUGS4$` WS_62_mean`,AUGS5$` WS_62_mean`,AUGS6$` WS_62_mean`,
                                AUGS7$` WS_62_mean`,AUGS8$` WS_62_mean`,AUGS9$` WS_62_mean`,
                                AUGS10$` WS_62_mean`
                                
)
names(AUG_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

AUG_62_means=colMeans(AUG_WS_62_mean[sapply(AUG_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

AUG_WS_62_Site_meandf=data.frame(WS_62_Site_num,AUG_62_means)
AUG_WS_62_Site_meandf[10,2]=AUG10_mean

#Note:Site 9 failed to record for most of the month
AUG_plt=ggplot(AUG_WS_62_Site_meandf, aes(WS_62_Site_num, AUG_62_means))+
  geom_point(col = "gold") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(AUG_plt + ggtitle("August 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))


##SEPTEMBER
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/9 SEP")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\9 SEP",pattern="*.csv")
myfiles=lapply(files,read.delim)
SEP= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
SEP=tibble(SEP)
SEP=row_to_names(SEP,1)
SEP=subset(SEP, date_time!="date_time")

SEP[, 3:47] <- sapply(SEP[, 3:47], as.numeric)
head(SEP)
class(SEP$` WS_62_mean`)

#NOTE no S9 data for September.
SEPS1=subset(SEP,` Station_ID`==" WM01")
SEPS2=subset(SEP,` Station_ID`==" WM02")
SEPS3=subset(SEP,` Station_ID`==" WM03")
SEPS4=subset(SEP,` Station_ID`==" WM04")
SEPS5=subset(SEP,` Station_ID`==" WM05")
SEPS6=subset(SEP,` Station_ID`==" WM06")
SEPS7=subset(SEP,` Station_ID`==" WM07")
SEPS8=subset(SEP,` Station_ID`==" WM08")
SEPS9=subset(SEP,` Station_ID`==" WM09")
SEPS9=data.frame(matrix(NA_integer_, nrow = nrow(SEPS1), ncol = ncol(SEPS1)))
names(SEPS9)=names(q)
SEPS10=subset(SEP, ` Station_ID`==" WM10") 

SEP_WS_62_mean=cbind.data.frame(SEPS1$date_time,SEPS1$` WS_62_mean`,SEPS2$` WS_62_mean`, SEPS3$` WS_62_mean`,
                                SEPS4$` WS_62_mean`,SEPS5$` WS_62_mean`,SEPS6$` WS_62_mean`,
                                SEPS7$` WS_62_mean`,SEPS8$` WS_62_mean`,SEPS9$` WS_62_mean`,
                                SEPS10$` WS_62_mean`
                                
)
names(SEP_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

SEP_62_means=colMeans(SEP_WS_62_mean[sapply(SEP_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

SEP_WS_62_Site_meandf=data.frame(WS_62_Site_num,SEP_62_means)


SEP_plt=ggplot(SEP_WS_62_Site_meandf, aes(WS_62_Site_num, SEP_62_means))+
  geom_point(col = "navy") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(SEP_plt + ggtitle("September 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))


##OCTOBER.
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/10 OCT")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\10 OCT",pattern="*.csv")
myfiles=lapply(files,read.delim)
OCT= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
OCT=tibble(OCT)
OCT=row_to_names(OCT,1)
OCT=subset(OCT, date_time!="date_time")

OCT[, 3:47] <- sapply(OCT[, 3:47], as.numeric)
head(OCT)
class(OCT$` WS_62_mean`)

OCTS1=subset(OCT,` Station_ID`==" WM01")
OCTS2=subset(OCT,` Station_ID`==" WM02")
OCTS3=subset(OCT,` Station_ID`==" WM03")
OCTS4=subset(OCT,` Station_ID`==" WM04")
OCTS5=subset(OCT,` Station_ID`==" WM05")
OCTS6=subset(OCT,` Station_ID`==" WM06")
OCTS7=subset(OCT,` Station_ID`==" WM07")
OCTS7_ACC=subset(OCTS7,` WS_62_mean`>0)
OCT7_mean=mean(OCTS7_ACC$` WS_62_mean`)
OCTS8=subset(OCT,` Station_ID`==" WM08")
OCTS9=subset(OCT,` Station_ID`==" WM09")
OCTS9=data.frame(matrix(NA_integer_, nrow = nrow(OCTS1), ncol = ncol(OCTS1)))
names(OCTS9)=names(q)
OCTS10=subset(OCT, ` Station_ID`==" WM10") 

OCT_WS_62_mean=cbind.data.frame(OCTS1$date_time,OCTS1$` WS_62_mean`,OCTS2$` WS_62_mean`, OCTS3$` WS_62_mean`,
                                OCTS4$` WS_62_mean`,OCTS5$` WS_62_mean`,OCTS6$` WS_62_mean`,
                                OCTS7$` WS_62_mean`,OCTS8$` WS_62_mean`,OCTS9$` WS_62_mean`,
                                OCTS10$` WS_62_mean`
                                
)
names(OCT_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

OCT_62_means=colMeans(OCT_WS_62_mean[sapply(OCT_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

OCT_WS_62_Site_meandf=data.frame(WS_62_Site_num,OCT_62_means)
OCT_WS_62_Site_meandf[7,2]=OCT7_mean


OCT_plt=ggplot(OCT_WS_62_Site_meandf, aes(WS_62_Site_num, OCT_62_means))+
  geom_point(col = "maroon") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(OCT_plt + ggtitle("October 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))



##NOVEMBER
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/11 NOV")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\11 NOV",pattern="*.csv")
myfiles=lapply(files,read.delim)
NOV= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
NOV=tibble(NOV)
NOV=row_to_names(NOV,1)
NOV=subset(NOV, date_time!="date_time")

NOV[, 3:47] <- sapply(NOV[, 3:47], as.numeric)
head(NOV)
class(NOV$` WS_62_mean`)

NOVS1=subset(NOV,` Station_ID`==" WM01")
NOVS2=subset(NOV,` Station_ID`==" WM02")
NOVS3=subset(NOV,` Station_ID`==" WM03")
NOVS4=subset(NOV,` Station_ID`==" WM04")
NOVS5=subset(NOV,` Station_ID`==" WM05")
NOVS6=subset(NOV,` Station_ID`==" WM06")
NOVS7=subset(NOV,` Station_ID`==" WM07")
NOVS8=subset(NOV,` Station_ID`==" WM08")
NOVS9=subset(NOV,` Station_ID`==" WM09")
NOVS9=data.frame(matrix(NA_integer_, nrow = nrow(NOVS1), ncol = ncol(NOVS1)))
names(NOVS9)=names(q)
NOVS10=subset(NOV, ` Station_ID`==" WM10") 

NOV_WS_62_mean=cbind.data.frame(NOVS1$date_time,NOVS1$` WS_62_mean`,NOVS2$` WS_62_mean`, NOVS3$` WS_62_mean`,
                                NOVS4$` WS_62_mean`,NOVS5$` WS_62_mean`,NOVS6$` WS_62_mean`,
                                NOVS7$` WS_62_mean`,NOVS8$` WS_62_mean`,NOVS9$` WS_62_mean`,
                                NOVS10$` WS_62_mean`
                                
)
names(NOV_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

NOV_62_means=colMeans(NOV_WS_62_mean[sapply(NOV_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

NOV_WS_62_Site_meandf=data.frame(WS_62_Site_num,NOV_62_means)


NOV_plt=ggplot(NOV_WS_62_Site_meandf, aes(WS_62_Site_num, NOV_62_means))+
  geom_point(col = "orange") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(NOV_plt + ggtitle("November 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))



##DECEMBER
setwd("~/1 UNI STUFF/Honours Research/Data and Cleaning/SITE DATA/12 DEC")
files=list.files(path="C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\SITE DATA\\12 DEC",pattern="*.csv")
myfiles=lapply(files,read.delim)
DEC= do.call(rbind, lapply
             (files, read.csv, as.is=T, skip = 16, header = FALSE))
DEC=tibble(DEC)
DEC=row_to_names(DEC,1)
DEC=subset(DEC, date_time!="date_time")

DEC[, 3:47] <- sapply(DEC[, 3:47], as.numeric)
head(DEC)
class(DEC$` WS_62_mean`)

DECS1=subset(DEC,` Station_ID`==" WM01")
DECS2=subset(DEC,` Station_ID`==" WM02")
DECS2_ACC=subset(DECS2,` WS_62_mean`>0)
DEC2_mean=mean(DECS2_ACC$` WS_62_mean`)
DECS3=subset(DEC,` Station_ID`==" WM03")
DECS4=subset(DEC,` Station_ID`==" WM04")
DECS5=subset(DEC,` Station_ID`==" WM05")
DECS5_ACC=subset(DECS5,` WS_62_mean`>0)
DEC5_mean=mean(DECS5_ACC$` WS_62_mean`)
DECS6=subset(DEC,` Station_ID`==" WM06")
DECS7=subset(DEC,` Station_ID`==" WM07")
DECS7_ACC=subset(DECS7,` WS_62_mean`>0)
DEC7_mean=mean(DECS7_ACC$` WS_62_mean`)
DECS8=subset(DEC,` Station_ID`==" WM08")
DECS9=subset(DEC,` Station_ID`==" WM09")
DECS9_ACC=subset(DECS9,` WS_62_mean`>0)
DEC9_mean=mean(DECS9_ACC$` WS_62_mean`)
DECS10=subset(DEC, ` Station_ID`==" WM10") 

DEC_WS_62_mean=cbind.data.frame(DECS1$date_time,DECS1$` WS_62_mean`,DECS2$` WS_62_mean`, DECS3$` WS_62_mean`,
                                DECS4$` WS_62_mean`,DECS5$` WS_62_mean`,DECS6$` WS_62_mean`,
                                DECS7$` WS_62_mean`,DECS8$` WS_62_mean`,DECS9$` WS_62_mean`,
                                DECS10$` WS_62_mean`
                                
)
names(DEC_WS_62_mean)=c("date_time","Site1","Site2","Site3","Site4","Site5",
                        "Site6","Site7","Site8","Site9","Site10")

DEC_62_means=colMeans(DEC_WS_62_mean[sapply(DEC_WS_62_mean, is.numeric)])
WS_62_Site_num=c(1,2,3,4,5,6,7,8,9,10)

DEC_WS_62_Site_meandf=data.frame(WS_62_Site_num,DEC_62_means)
DEC_WS_62_Site_meandf[2,2]=DEC2_mean
DEC_WS_62_Site_meandf[5,2]=DEC5_mean
DEC_WS_62_Site_meandf[7,2]=DEC7_mean
DEC_WS_62_Site_meandf[9,2]=DEC9_mean

DEC_plt=ggplot(DEC_WS_62_Site_meandf, aes(WS_62_Site_num, DEC_62_means))+
  geom_point(col = "black") + 
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
print(DEC_plt + ggtitle("December 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))

ALL_WS_62_Site_meandf=data.frame(WS_62_Site_num,JAN_62_means,FEB_62_means
                                 ,MAR_62_means,APR_62_means,MAY_62_means,
                                 JUN_62_means,JUL_62_means,AUG_62_means,
                                 OCT_62_means,NOV_62_means,DEC_62_means)

ALL_WS_62_Site_meandf_melt = melt(ALL_WS_62_Site_meandf, id.vars="WS_62_Site_num")

ALL_plt=ggplot(ALL_WS_62_Site_meandf_melt, aes(WS_62_Site_num,value, col=variable)) + 
  geom_point() +
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
  #stat_smooth() 
print(ALL_plt + ggtitle("All Months 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))

ALL_plt_smooth=ggplot(ALL_WS_62_Site_meandf_melt, aes(WS_62_Site_num,value, col=variable)) + 
  geom_line() +
  labs(x = "Site Name", y = "Mean Wind Speed per Site")
  stat_smooth() 
print(ALL_plt_smooth + ggtitle("All Months 2011")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9, 10)))


#Creating a new Dataframe
#JAN
JAN_MON=data.frame(matrix("JAN", nrow = nrow(JAN), ncol = 1))
MON=c("MONTH")
names(JAN_MON)=MON
JAN=add_column(JAN, JAN_MON, .after = 2)
#FEB
FEB_MON=data.frame(matrix("FEB", nrow = nrow(FEB), ncol = 1))
add_column(FEB, FEB_MON, .after = 2)
names(FEB_MON)=MON
FEB=add_column(FEB, FEB_MON, .after = 2)
#MAR
MAR_MON=data.frame(matrix("MAR", nrow = nrow(MAR), ncol = 1))
add_column(MAR, MAR_MON, .after = 2)
names(MAR_MON)=MON
MAR=add_column(MAR, MAR_MON, .after = 2)
#APR
APR_MON=data.frame(matrix("APR", nrow = nrow(APR), ncol = 1))
add_column(APR, APR_MON, .after = 2)
names(APR_MON)=MON
APR=add_column(APR, APR_MON, .after = 2)
#MAY
MAY_MON=data.frame(matrix("MAY", nrow = nrow(MAY), ncol = 1))
add_column(MAY, MAY_MON, .after = 2)
names(MAY_MON)=MON
MAY=add_column(MAY, MAY_MON, .after = 2)
#JUN
JUN_MON=data.frame(matrix("JUN", nrow = nrow(JUN), ncol = 1))
add_column(JUN, JUN_MON, .after = 2)
names(JUN_MON)=MON
JUN=add_column(JUN, JUN_MON, .after = 2)
#JUL
JUL_MON=data.frame(matrix("JUL", nrow = nrow(JUL), ncol = 1))
add_column(JUL, JUL_MON, .after = 2)
names(JUL_MON)=MON
JUL=add_column(JUL, JUL_MON, .after = 2)
#AUG
AUG_MON=data.frame(matrix("AUG", nrow = nrow(AUG), ncol = 1))
add_column(AUG, AUG_MON, .after = 2)
names(AUG_MON)=MON
AUG=add_column(AUG, AUG_MON, .after = 2)
#SEP
SEP_MON=data.frame(matrix("SEP", nrow = nrow(SEP), ncol = 1))
add_column(SEP, SEP_MON, .after = 2)
names(SEP_MON)=MON
SEP=add_column(SEP, SEP_MON, .after = 2)
#OCT
OCT_MON=data.frame(matrix("OCT", nrow = nrow(OCT), ncol = 1))
add_column(OCT, OCT_MON, .after = 2)
names(OCT_MON)=MON
OCT=add_column(OCT, OCT_MON, .after = 2)
#NOV
NOV_MON=data.frame(matrix("NOV", nrow = nrow(NOV), ncol = 1))
add_column(NOV, NOV_MON, .after = 2)
names(NOV_MON)=MON
NOV=add_column(NOV, NOV_MON, .after = 2)
#DEC
DEC_MON=data.frame(matrix("DEC", nrow = nrow(DEC), ncol = 1))
add_column(DEC, DEC_MON, .after = 2)
names(DEC_MON)=MON
DEC=add_column(DEC, DEC_MON, .after = 2)



names=c("Obs", "Site" ,"Month", "Altitude", "Wind_Speed")
master_df=rbind(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)


#ALTITUDE AT 10 meters.
ALT_mean_10=data.frame(matrix(10, nrow = nrow(master_df), ncol = 1))
ALT=c("Altitude")
names(ALT_mean_10)=ALT
ALT_10=data.frame(master_df$date_time,master_df$` Station_ID`
                  ,master_df$MONTH,ALT_mean_10,master_df$` WS_10_mean`)
ALT_names=c("date_time", "Site" ,"Month", "Altitude", "Wind_Speed")
names(ALT_10)=ALT_names

#ALTITUDE AT 20 meters.
ALT_mean_20=data.frame(matrix(20, nrow = nrow(master_df), ncol = 1))
ALT=c("Altitude")
names(ALT_mean_20)=ALT
ALT_20=data.frame(master_df$date_time,master_df$` Station_ID`
                  ,master_df$MONTH,ALT_mean_20,master_df$` WS_20_mean`)
ALT_names=c("date_time", "Site" ,"Month", "Altitude", "Wind_Speed")
names(ALT_20)=ALT_names


#ALTITUDE AT 40 meters
ALT_mean_40=data.frame(matrix(40, nrow = nrow(master_df), ncol = 1))
ALT=c("Altitude")
names(ALT_mean_40)=ALT
ALT_40=data.frame(master_df$date_time,master_df$` Station_ID`
                  ,master_df$MONTH,ALT_mean_40,master_df$` WS_40_mean`)
ALT_names=c("date_time", "Site" ,"Month", "Altitude", "Wind_Speed")
names(ALT_40)=ALT_names
ALL_WS_62_Site_meandf_melt = melt(ALL_WS_62_Site_meandf, id.vars="WS_62_Site_num")



#ALTITUDE AT 60 meters.
ALT_mean_60=data.frame(matrix(60, nrow = nrow(master_df), ncol = 1))
ALT=c("Altitude")
names(ALT_mean_60)=ALT
ALT_60=data.frame(master_df$date_time,master_df$` Station_ID`
                  ,master_df$MONTH,ALT_mean_60,master_df$` WS_60_mean`)
ALT_names=c("date_time", "Site" ,"Month", "Altitude", "Wind_Speed")
names(ALT_60)=ALT_names



#ALTITUDE AT 62 meters.
ALT_mean_62=data.frame(matrix(62, nrow = nrow(master_df), ncol = 1))
ALT=c("Altitude")
names(ALT_mean_62)=ALT
ALT_62=data.frame(master_df$date_time,master_df$` Station_ID`
                  ,master_df$MONTH,ALT_mean_62,master_df$` WS_62_mean`)
ALT_names=c("date_time", "Site" ,"Month", "Altitude", "Wind_Speed")
names(ALT_62)=ALT_names


#BINDING ALL ALTITUDES TOGETHER.
ALL_WS_ALT_means=rbind(ALT_10,ALT_20,ALT_40,ALT_60,ALT_62)
#THIS IS THE PRIME DATA_FRAME


n=nrow(ALL_WS_ALT_means)
obs=data.frame(matrix(1:n, nrow = n, ncol = 1))
obs_names=c("obs")
names(obs)=obs_names
ALL_WS_ALT_means=cbind(obs,ALL_WS_ALT_means)
Better_ALT_names=c("Obs","date_time", "Site" ,"Month", "Altitude", "Wind_Speed")
names(ALL_WS_ALT_means)=Better_ALT_names

#write.csv(ALL_WS_ALT_means,"C:\\Users\\Matthew\\Documents\\1 UNI STUFF\\Honours Research\\Data and Cleaning\\All Wind Speeds At All Altitudes.csv", row.names = TRUE)

tempdir()
dir.create(tempdir())