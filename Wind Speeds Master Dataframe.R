library(INLA)
library(ggplot2)
library(readr)

master_dataframe <- read_csv("1 UNI STUFF/Honours Research/Data and Cleaning/All Wind Speeds At All Altitudes.csv")
#View(All_Wind_Speeds_At_All_Altitudes)
n=nrow(master_dataframe)


q=1000
sample_Q=master_dataframe[sample(n, q), ]




