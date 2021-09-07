library(tidyverse)
library(tswge)


#Read in file
df <-read.csv(file="C:/users/bodie/Documents/CAPSTONE_DATA.csv")
#Change Date format
df$DAY <- as.Date(df$DAY,format="%m/%d/%Y")

#Subset dataframe
Load_df <- df %>% select('DAY','TOTAL_LOAD')

#Quick check for na's and types
summary(Load_df)
str(Load_df)

###Boxplot of Total Load
Load_df %>% ggplot(aes(TOTAL_LOAD))+geom_boxplot()

##Histogram
Load_df %>% ggplot(aes(TOTAL_LOAD))+geom_histogram()


##plots
plotts.sample.wge(Load_df$TOTAL_LOAD)

###Doesn't look to meet three conditions of stationary but will proceed for base line
nrow(Load_df)



##Define short term Forecasting as 30 days
##Long term forecasting as 180 days. 
##Can redefine later but lets use as base for now

##Update test size 
test_size = nrow(Load_df)-1000
#Checking length of test_size values
test_size
Train_Load_DF <- Load_df[1:test_size,]
Test_Load_DF <- Load_df[(test_size+1):nrow(Load_df),]

##Check the lengths tie out

((nrow(Train_Load_DF)+nrow(Test_Load_DF))==nrow(Load_df))

##Creating List for compare later on

Model_name <- vector(mode = "list")
Short_Term_ASE_Score <- vector(mode = "list")
Long_Term_ASE_Score <- vector(mode = "list")


##ARMA

aic5.wge(Train_Load_DF$TOTAL_LOAD,p=0:5,q=0:5,type="bic")
aic5.wge(Train_Load_DF$TOTAL_LOAD,p=0:5,q=0:5,type="aic")


#Bic picks 1,4 and in top for AIC.
arma_model = est.arma.wge(Train_Load_DF$TOTAL_LOAD,p=1,q=4)


##Short TERM forecasting for ARMA model 30 days 

f = fore.aruma.wge(Train_Load_DF$TOTAL_LOAD,phi = arma_model$phi, theta = arma_model$theta, n.ahead = 30,limits = T, lastn = T)

Short_Term_ARMA_ASE = mean((Test_Load_DF$TOTAL_LOAD[1:30]- f$f)^2)

Short_Term_ARMA_ASE

#Long Term ARMA

f = fore.aruma.wge(Train_Load_DF$TOTAL_LOAD,phi = arma_model$phi, theta = arma_model$theta, n.ahead = 180,limits = T, lastn = T)
Long_Term_ARMA_ASE = mean((Test_Load_DF$TOTAL_LOAD[1:180]- f$f)^2)


Long_Term_ARMA_ASE


##Append List
Model_name <- c(Model_name,"ARMA")
Short_Term_ASE_Score <- c(Short_Term_ASE_Score,Short_Term_ARMA_ASE)
Long_Term_ASE_Score <- c(Long_Term_ASE_Score,Long_Term_ARMA_ASE)


#####ARIMA

##Here we are differencing the dataset then doing train/test split.

trans.p = artrans.wge(Load_df$TOTAL_LOAD,phi.tr = 1)
plotts.sample.wge(trans.p)

#Train test split
length(trans.p)
#4168

#Dataframe so we can subset
trans.p <- as.data.frame(trans.p)

test_size_difference = nrow(trans.p)-1000
#Checking length of test_size values
test_size_difference

Train_Load_DF_Diff <- trans.p[1:test_size_difference,]
Test_Load_DF_Diff <- trans.p[(test_size_difference+1):nrow(trans.p),]

#Check the lengths tie out. Since the train/test sets aren't DF we have to use lengths instead of nrow.

(length(Train_Load_DF_Diff)+length(Test_Load_DF_Diff)) == nrow(trans.p)

#Find p and Q
aic5.wge(Train_Load_DF_Diff,p=0:5,q=0:5,type='aic')
aic5.wge(Train_Load_DF_Diff,p=0:5,q=0:5,type='bic')

#Both BIC & AIC pick 4,3.
arima_model = est.arma.wge(Train_Load_DF_Diff,p=4,q=3)


#short term
f = fore.aruma.wge(Train_Load_DF_Diff,phi = arima_model$phi, theta = arima_model$theta, n.ahead = 30,limits = T, lastn = T)

Short_Term_ARIMA_ASE = mean((Test_Load_DF_Diff[1:30] - f$f)^2)

Short_Term_ARIMA_ASE

##Long term
f = fore.aruma.wge(Train_Load_DF_Diff,phi = arima_model$phi, theta = arima_model$theta, n.ahead = 180,limits = T, lastn = T)
Long_Term_ARIMA_ASE = mean((Test_Load_DF_Diff[1:180] - f$f)^2)

Long_Term_ARIMA_ASE


##Append List
Model_name <- c(Model_name,"ARIMA")
Short_Term_ASE_Score <- c(Short_Term_ASE_Score,Short_Term_ARIMA_ASE)
Long_Term_ASE_Score <- c(Long_Term_ASE_Score,Long_Term_ARIMA_ASE)

###Why are we getting neg values when we difference the data? This is why the ASE is terrible.


###Log  this works

#Let's do log then take the difference
Load_df$LOG_LOAD <- log(Load_df$TOTAL_LOAD)

LOG_DF <-Load_df %>% select('DAY','LOG_LOAD')

plotts.sample.wge(LOG_DF$LOG_LOAD)

###Difference
trans.log = artrans.wge(LOG_DF$LOG_LOAD,phi.tr = 1)

##Train test split

trans.log <-as.data.frame(trans.log)

test_size_log_difference = nrow(trans.log)-1000
#Checking length of test_size values
test_size_log_difference

Train_Load_DF_log_Diff <- trans.log[1:test_size_log_difference,]
Test_Load_DF_log_Diff <- trans.log[(test_size_log_difference+1):nrow(trans.log),]

((length(Train_Load_DF_log_Diff)+length(Test_Load_DF_log_Diff))==nrow(trans.log))

aic5.wge(Train_Load_DF_log_Diff,p=0:5,q=0:5,type='aic')
aic5.wge(Train_Load_DF_log_Diff,p=0:5,q=0:5,type='bic')

#Both BIC & AIC pick 4,3.
log_difference = est.arma.wge(Train_Load_DF_log_Diff,p=4,q=3)


#short term
f = fore.aruma.wge(Train_Load_DF_log_Diff,phi = log_difference$phi, theta = log_difference$theta, n.ahead = 30,limits = T, lastn = T)

Short_Term_log_difference_ASE = mean((Test_Load_DF_log_Diff[1:30] - f$f)^2)

Short_Term_log_difference_ASE

###Long
f = fore.aruma.wge(Train_Load_DF_log_Diff,phi = log_difference$phi, theta = log_difference$theta, n.ahead = 180,limits = T, lastn = T)

Long_Term_log_difference_ASE = mean((Test_Load_DF_log_Diff[1:180] - f$f)^2)

Long_Term_log_difference_ASE

##Append
Model_name <- c(Model_name,"Log Difference")
Short_Term_ASE_Score <- c(Short_Term_ASE_Score,Short_Term_log_difference_ASE)
Long_Term_ASE_Score <- c(Long_Term_ASE_Score,Long_Term_log_difference_ASE)


###Seasonality compoent

Seasonality_removed_df <- artrans.wge(LOG_DF$LOG_LOAD,c(rep(0,364),1))
plotts.sample.wge(Seasonality_removed_df)

length(Seasonality_removed_df)

Seasonality_removed_df <- as.data.frame(Seasonality_removed_df)




test_size_Seasonality = nrow(Seasonality_removed_df)-1000
#Checking length of test_size values
test_size_Seasonality

Train_Load_DF_seasonality_removed <- Seasonality_removed_df[1:test_size_Seasonality,]
Test_Load_DF_seasonality_removed <- Seasonality_removed_df[(test_size_Seasonality+1):nrow(Seasonality_removed_df),]

#check the splits add up
((length(Train_Load_DF_seasonality_removed)+length(Test_Load_DF_seasonality_removed))==nrow(Seasonality_removed_df))


aic5.wge(Train_Load_DF_seasonality_removed,p=0:5,q=0:5,type='aic')
aic5.wge(Train_Load_DF_seasonality_removed,p=0:5,q=0:5,type='bic')

#Both BIC & AIC pick 4,3.
seasonality_removed_model = est.arma.wge(Train_Load_DF_seasonality_removed,p=5,q=5)


##short term
f = fore.aruma.wge(Train_Load_DF_seasonality_removed,phi = seasonality_removed_model$phi, theta = seasonality_removed_model$theta, n.ahead = 30,limits = T, lastn = T)

Short_Term_seasonality_ASE = mean((Test_Load_DF_seasonality_removed[1:30] - f$f)^2)

Short_Term_seasonality_ASE

##long term
f = fore.aruma.wge(Train_Load_DF_seasonality_removed,phi = seasonality_removed_model$phi, theta = seasonality_removed_model$theta, n.ahead = 180,limits = T, lastn = T)

long_Term_seasonality_ASE = mean((Test_Load_DF_seasonality_removed[1:180] - f$f)^2)

long_Term_seasonality_ASE

##Append List
Model_name <- c(Model_name,"Seasonality_removed")
Short_Term_ASE_Score <- c(Short_Term_ASE_Score,Short_Term_seasonality_ASE)
Long_Term_ASE_Score <- c(Long_Term_ASE_Score,long_Term_seasonality_ASE)

##quarterly data

quarter_seasonality <- artrans.wge(Load_df$TOTAL_LOAD,c(0,0,0,1))

quarter_seasonality <- as.data.frame(quarter_seasonality)

##Train test split

test_size_quarter = nrow(quarter_seasonality)-1000
#Checking length of test_size values
test_size_quarter

Train_Load_DF_quarter <- quarter_seasonality[1:test_size_quarter,]
Test_Load_DF_quarter <- quarter_seasonality[(test_size_quarter+1):nrow(quarter_seasonality),]

#check the splits add up
((length(Train_Load_DF_quarter)+length(Test_Load_DF_quarter))==nrow(quarter_seasonality))

##Build model
aic5.wge(Train_Load_DF_quarter,p=0:5,q=0:5,type='aic')
aic5.wge(Train_Load_DF_quarter,p=0:5,q=0:5,type='bic')


#Both BIC & AIC 5,1 in their top
quarter_removed_model = est.arma.wge(Train_Load_DF_quarter,p=5,q=1)


##short term
f = fore.aruma.wge(Train_Load_DF_quarter,phi = quarter_removed_model$phi, theta = quarter_removed_model$theta, n.ahead = 30,limits = T, lastn = T)

Short_Term_quarter_ASE = mean((Test_Load_DF_quarter[1:30] - f$f)^2)

Short_Term_quarter_ASE

##long term
f = fore.aruma.wge(Train_Load_DF_quarter,phi = quarter_removed_model$phi, theta = quarter_removed_model$theta, n.ahead = 180,limits = T, lastn = T)

long_Term_quarter_ASE = mean((Test_Load_DF_quarter[1:180] - f$f)^2)

long_Term_quarter_ASE

##Append list
Model_name <- c(Model_name,"quarter_removed_model")
Short_Term_ASE_Score <- c(Short_Term_ASE_Score,Short_Term_quarter_ASE)
Long_Term_ASE_Score <- c(Long_Term_ASE_Score,long_Term_quarter_ASE)




#turn list into dataframe
Model_results <-cbind(Model_name,Short_Term_ASE_Score,Long_Term_ASE_Score)
Model_results
