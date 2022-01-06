# library loading

library(DescTools)
library(car)
library(corrplot)

getwd()
setwd("C://Users//tk//Desktop//DataScience//Analytics Vidhya//DataSet")

# loading database

trainRaw <- read.csv("train_file.csv")
predictionRaw <-  read.csv("test_file.csv")

View(trainRaw)
View(predictionRaw)

# Adding Item_Outlet_Sales in prediction file
predictionRaw$Item_Outlet_Sales <-  NA

dim(trainRaw)
dim(predictionRaw)

# Divide trainRaw into train & test datasets
set.seed(2410)
trainRow <- sample(x=1:nrow(trainRaw),size=0.7*nrow(trainRaw))
trainDf  <- trainRaw[trainRow,]
testDf   <- trainRaw[-trainRow,]
dim(trainDf)
dim(testDf)



# Adding source column in trainDf & testDf to identify
trainDf$Source <- "Train"
testDf$Source  <- "Test"
predictionRaw$Source <- "Prediction"


# combined all three datasets

fullRaw <- rbind(trainDf,testDf,predictionRaw)
dim(fullRaw)

# summary
summary(fullRaw)# we have NA's values & identifier columns

# let us drop identifier columns

fullRaw$Item_Identifier <- NULL
fullRaw$Outlet_Identifier <- NULL
dim(fullRaw)

# checking NA values

colSums(is.na(fullRaw))

# taking inference from trainDf sets for Item_Weight NA's values

tempMedian <- median(fullRaw[fullRaw$Source=="Train","Item_Weight"],na.rm=TRUE)
tempMedian

missingRows <- is.na(fullRaw[,"Item_Weight"])

fullRaw[missingRows,"Item_Weight"] <- tempMedian

sum(is.na(fullRaw$Item_Weight))

summary(fullRaw)

# computing empty values-Outlet_Size

colSums(fullRaw=="")

fullRaw$Outlet_Size[fullRaw$Outlet_Size==""]<- "Not Known"


#Outlier Detection

boxplot(fullRaw$Item_Weight)
boxplot(fullRaw$Item_MRP)

# correlation
str(fullRaw)
numericVars <- which(sapply(fullRaw,is.numeric))
str(numericVars)

all_numVar <- fullRaw[,numericVars]
str(all_numVar)
summary(all_numVar)

cor_numVar <- round(cor(fullRaw[fullRaw$Source=="Train",numericVars],use="pairwise.complete.obs"),2)
cor_numVar
corrplot(cor_numVar)# No correlation found

# Dummy Variable creations


str(fullRaw)
fullRaw$Item_Fat_Content <- as.factor(fullRaw$Item_Fat_Content)
fullRaw$Item_Type <- as.factor(fullRaw$Item_Type)
fullRaw$Outlet_Size <- as.factor(fullRaw$Outlet_Size)
fullRaw$Outlet_Location_Type <- as.factor(fullRaw$Outlet_Location_Type)
fullRaw$Outlet_Type <- as.factor(fullRaw$Outlet_Type)


factorVars <- sapply(fullRaw,is.factor)
factorVars

dummyDf <- model.matrix(~.,data=fullRaw[,factorVars])
dummyDf

fullDf <- cbind(fullRaw[,!factorVars],dummyDf[,-1])
dim(fullDf)

# divide data into train & test & source column delete

train <- subset(fullDf,subset=fullDf$Source=="Train",select=-Source)
test <- subset(fullDf,subset=fullDf$Source=="Test",select=-Source)
prediction <- subset(fullDf,subset=fullDf$Source=="Prediction",select=-Source)

dim(train)
dim(test)
dim(prediction)

# Multicollinearity check

M1 <- lm(Item_Outlet_Sales~.,data=train) 
summary(M1)

vif(M1)
sort(vif(M1), decreasing = TRUE)[1:3]

# remove variable Outlet_SizeMedium
M2=lm(Item_Outlet_Sales~.- Outlet_SizeMedium,data=train)
sort(vif(M2),decreasing=TRUE)[1:3]

# remove `Outlet_TypeSupermarket Type2'
M3=lm(Item_Outlet_Sales~.- Outlet_SizeMedium - `Outlet_TypeSupermarket Type2`,data=train)
sort(vif(M3),decreasing=TRUE)[1:3]

# remove  Item_Fat_ContentRegular
M4=lm(Item_Outlet_Sales~.- Outlet_SizeMedium - `Outlet_TypeSupermarket Type2`-Item_Fat_ContentRegular,data=train)
sort(vif(M4),decreasing=TRUE)[1:3]

# We have VIF's below 5
summary(M4)

# Model optimization using step function
# selecting only significant variables for our model

M5=step(M4)
summary(M5)

# still we can remove some insignificant variables

M6=update(M5,.~.-`Item_TypeHealth and Hygiene`)
summary(M6)

# Let us finalize M6 model with all significant variables

# Model Check
# Homoskedasticity check
plot(M6$fitted.values,M6$residuals)

# Normality check
summary(M4$residuals)
hist(M6$residuals)

# predcit
test_predict <- predict(M6,test)
summary(test_predict)

# RMSE
RMSE=sqrt(mean((test$Item_Outlet_Sales-test_predict)^2))
RMSE # 1128

# MAPE
MAPE=(mean(abs(test$Item_Outlet_Sales-test_predict))/nrow(test))*100
MAPE # 33.23%

# prediction on prediction data set
prediction$Item_Outlet_Sales <- predict(M6,prediction) 

View(prediction$Item_Outlet_Sales)

outputfile <- data.frame(predictionRaw$Item_Identifier,predictionRaw$Outlet_Identifier,prediction$Item_Outlet_Sales)

write.csv(outputfile,"Output.csv")
