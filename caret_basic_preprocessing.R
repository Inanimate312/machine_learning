library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type,p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve,main="",xlab="ave. capital run length")

mean(training$capitalAve)
# 4.31

sd(training$capitalAve)
# 16.65

# Standardizing variables given the high standard deviation
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)

mean(trainCapAveS) 
# -2.36

sd(trainCapAveS)
# 1

# Standardizing test set - note we have to use training set mean and sd
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)

mean(testCapAveS) 
# 0.21

sd(testCapAveS)
# 3.39 - This should probably be closer to 1...

# Standardizing with preProcess function
# training[,-58] passes it all the observations in the training set except the 58th, becasue the 58th is the outcome we care about
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
# -2.18

sd(trainCapAveS)
# 1

# You can use the preProcess created object to apply same process to test set
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)

# Pass preProcess directly to the train function as an argument, to standardize predictors before using them in prediction model
set.seed(32343)
modelFit <- train(type ~., data=training, preProcess=c("center","scale"),method="glm")
modelFit

# Other transformations besides centering and scaling
# Box-Cox transforms (take continuous data and try to make it look like normal data)

preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# Imputing data to address missing data
library(RANN)
set.seed(13343)

# Make some values NA as an example
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

# How close are imputed values to the values that were truly there prior to being replaced with NA above?
quantile(capAve - capAveTruth)

quantile((capAve - capAveTruth)[selectNA])

quantile((capAve - capAveTruth)[!selectNA])
