# 2021 Honours Project INLA code
 This code was compiled by Matthew De Bie (u18115943- University of Pretoria) with assistance from Dr Janet Van Niekerk under the researhc topic "Using Principled Bayesian inference to asses the viabillity of wind power in South Africa".
 
 The files "Bayes and INLA estimation" and "Weibull_MLE and Grid Search" contain example code whereby the MLE and INLA approaches to estimations are compared using simulated data.
 
 The file "Wind Speed site by month import and EDA" is an extensive programme detailing how data retrieved from Wind Atlas South Africa (WASA) was first imported and then transformed from over 100 separated excel sheet each with roughly 5,000 observations and 50 columns into a single 'master' dataframe of just 6 columns where each of 2,562475 observations has a unique identifier.
 
Finally, the file "Application of INLA to data" details how several models of increasing complexity where fitted to the 'master' dataframe by means of the INLA method. This programme also contains the necessary code to draw a sample from the dataframe and add additional identifiers to ensure smooth model fitting.
