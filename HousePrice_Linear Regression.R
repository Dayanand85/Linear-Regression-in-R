# Package loading
library(car)
install.packages("DescTools")
library(DescTools)
library(corrplot)

#------------------------------------------------------------------------
##### loading database

raw_data <- read.csv("C:\\Users\\tk\\Desktop\\DataScience\\R\\R_Material\\R_Material\\2_Linear_Regression\\PropertyPrice_Train.csv",stringsAsFactors = TRUE)
prediction_data <- read.csv("C:\\Users\\tk\\Desktop\\DataScience\\R\\R_Material\\R_Material\\2_Linear_Regression\\PropertyPrice_Prediction.csv",stringsAsFactors = TRUE)

#------------------------------------------------------------------------------
dim(raw_data)
dim(prediction_data)
colnames(prediction_data)
#--------------------------------------------------------------------------------
summary(raw_data)
summary(prediction_data)
#--------------------------------------------------------------------------
#----Sampling-Before we process further let us divide our raw data into train & test
#------------------------------------------------------------------------
set.seed(3456)
RowNumbers <- sample(x=1:nrow(raw_data),size=0.8*nrow(raw_data))
head(RowNumbers)#first six randomly picked numbers

train <- raw_data[RowNumbers,]
test <- raw_data[-c(RowNumbers),]

dim(train)
dim(test)
dim(prediction_data)
#----------------------------------------------------------------------------
# create source column in train,test & prediction data---------------------
train$Source <- "Train"
test$Source <- "Test"
prediction_data$Source <- "Prediction"

#-------------------------------------------------------------------
# combining all three databses--------------------------------------

all=rbind(train,test,prediction_data)
dim(all)

# remove id columns.Since it is identifier.so it will not going to help

all$Id <-NULL

# Another method to remove column or columns using subset method

#all <- subset(all,select=-ID)

#---------------------------------------------------------------------------

#check the summary of the database
summary(all[all$Source == "Train",])#summary of train data
summary(all[all$Source == "Test",])#summary of test data
summary(all[all$Source == "Prediction_data",])#summary of prediction data

#---------------------------------------------------------------------------

#######Missing value imputation-----------------------------------
#------------------------------------------------------------------------
summary(all)# to check missing values
colSums(is.na(all))

# we have missing valuein Garage,Garagr_Built_year

# Step 1: Find Median
#median(train$Garage_Built_Year)
tempmedian <- median(train$Garage_Built_Year,na.rm=TRUE)
tempmedian

# Step 2:Find missing value rows
missingValueRows <- is.na(all[,"Garage_Built_Year"])
missingValueRows
sum(missingValueRows)
# step 3: impute missing values

all[missingValueRows,"Garage_Built_Year"] <- tempmedian
sum(is.na(all$Garage_Built_Year))
summary(all)
colSums(is.na(all))
dim(all)
colnames(all)
#-----------------------------------------------------------------------
#all$Garage[is.na(all$Garage)] <- "No Garage"
#table(all$Garage)
#sum(is.na(train$Garage))

#--------------------------------------------------------------------

# Step 1: Find Mode
#mode(train$Garage,na.rm=TRUE)[1]#inference

tempMode <- mode(train$Garage)[1]
tempMode

# Step 2: Find missing value rows

missingValueRows <- is.na(all[,"Garage"])
missingValueRows

#Step 3:Impute(or fill) missing values in rows

all[missingValueRows,"Garage"] <- tempMode
sum(is.na(all$Garage))
#### Missing values computation in ane row############################

for (i in colnames(all)){
  if(i!="Sale_Price"){# Not to worth to check dependent values NA value
    if((class(all[,i])=="integer")|(class(all[,i])=="numeric")){#identify continuous variables
      cat("cont",i,"\n")
      tempMedian=median(all[all$Source=="Train",i],na.rm=TRUE)#use train rows
      #print(tempMedian)
      missingValueRows=is.na(all[,i])
      all[missingValueRows,i]=tempMedian
    }else{ # for categorical variable
        cat("categ",i,"\n")
      tempMode=mode(all[all$Source=="Train",i])[1]
      #print(tempMode)
      missingValueRows=is.na(all[,i])
      all[missingValueRows,i]=tempMode
      }
   }
}
# To check NULL Values
sum(is.na(all))
colSums(is.na(all))

########################################################################
##### Correlation check-----------------------------------------------
###########################################################################

library(corrplot)
Continuous_Variable_Check <- function(x){
  return(is.numeric(x)|is.integer(x))
}
  
Continuous_Var <- sapply(all,Continuous_Variable_Check)
Continuous_Var

corrDf <- cor(all[all$Source=="Train",Continuous_Var])
corrDf
corrplot(corrDf)            

#######################################################################
############## Dummy Variable Creation
#######################################################################

factorVars <- sapply(all,is.factor)
factorVars
help(model.matrix)

#dummyDf <- model.matrix(~ .,data=all[,factorVars])

dummyDf <- model.matrix(~ .,data=all[,factorVars])

View(dummyDf)
library(car)
dim(dummyDf)

fullDf2 <- cbind(all[,!factorVars],dummyDf[,-1])
# Check the dimensions of fullDf2
dim(fullDf2)

# Check if all variables are now numeric/integer
str(fullDf2) 

############################
# Divide the fullDf2 back into trainDf, testDf, predictionDf
############################

# Step 1: Divide trainDf into trainDf and testDf
trainDf = subset(fullDf2, subset = fullDf2$Source == "Train", select = -Source)
testDf = subset(fullDf2, subset = fullDf2$Source == "Test", select = -Source)
predictionDf = subset(fullDf2, subset = fullDf2$Source == "Prediction", select = -Source)

############################
# Multicollinearity check
############################
help(vif)

# Remove variables with VIF > 5

M1 = lm(Sale_Price ~ ., data = trainDf)
# M1 = lm(Dependent ~ x1 + x2 + x3 + x4, data = trainDf)

summary(M1)

library(car)
vif(M1)
sort(vif(M1), decreasing = TRUE)[1:3]



# Remove GarageAttchd
M2 = lm(Sale_Price ~ . - GarageAttchd, data = trainDf)

sort(vif(M2), decreasing = TRUE)[1:3]


# Remove Kitchen_QualityTA
M3 = lm(Sale_Price ~ . - GarageAttchd - Kitchen_QualityTA, data = trainDf)
sort(vif(M3), decreasing = TRUE)[1:3]

# Remove First_Floor_Area
M3 = lm(Sale_Price ~ . - GarageAttchd - Kitchen_QualityTA - First_Floor_Area, data = trainDf)
sort(vif(M3), decreasing = TRUE)[1:3]


# All variables are within the bound of VIF (VIF < 5). Now we can proceed towards model building and variable selection
summary(M3)

############################
# Model optimization (by selecting ONLY significant variables through step() function)
############################

# Use step() function to remove insignificant variables from the model iteratively
M4 = step(M3) # Step function works on the concept of reducing AIC. Lower the AIC, better the model

summary(M4) # Lets finalize this model


############################  #
#258794.308-30794.30807
# Model diagnostics
############################

# Few checks
# Homoskedasticity check
plot(M4$fitted.values, M4$residuals) 
# Should not show prominent non-constant variance (heteroskadastic) of errors against fitted values

# Normality of errors check
summary(M4$residuals) # To check the range. Will be used in histogram is next step
hist(M4$residuals, breaks = seq(-490000, 340000, 10000)) # Should be somewhat close to normal distribution

# Model Evaluation
############################

# After doing all of this, the model has to be checked against the testDf data as well
# Lets predict on testset and then calculate a metric called MAPE to estimate the errors on testing data

M4_Pred = predict(M4, testDf)
head(M4_Pred)
head(testDf$Sale_Price)


############################
Actual = testDf$Sale_Price
Prediction = M4_Pred

# RMSE (Root Mean Square Error)
sqrt(mean((Actual - Prediction)^2)) # 38709
# This means on an "average", the house price prediction would have +/- error of about $38,709

# Now, is this a good model? Probably an "Average" model. If I told you your house was going to sell for $300,000 and 
# then it actually only sold for $262,000 (Roughly $38,000 error), you would be pretty mad. $38,000 is a reasonable difference 
# when you're buying/selling a home. 
# if the prediction is $300000, then the house would be sold somewhere between $262000 and $338000 
# But what if I told you that I could predict GDP (Gross Domestic Product) 
# of the US with only an average error of $38000? Well, since the GDPs are usually around $20 trillion, 
# that difference (of $38,000) wouldn't be so big. So, an RMSE of $38,000 would be acceptable in a GDP model but not 
# in Property prediction model like ours! So, there is a bit of relativity involved in RMSE values.

# There is another metric called "MAPE"and its interpreted in percentage term.

# MAPE (Mean Absolute Percentage Error)
mean(abs((Actual - Prediction)/Actual))*100 # 16%
# This means on an "average", the house price predictionDf would have +/- error of 16%

# Generally, a MAPE under 10% is considered very good, and anything under 20% is reasonable.
# MAPE over 20% is not considered great.

############################

# Check MAPE and RMSE results using forecast package
install.packages("forecast")
library(forecast)
accuracy(Prediction, Actual)



############################
# Prediction on predictionDf (given by client)
############################

predictionDf$Sale_Price = predict(M4, predictionDf)



