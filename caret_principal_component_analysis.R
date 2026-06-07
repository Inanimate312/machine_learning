library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# Identify correlated predictors
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind=T)

names(spam)[c(34,32)]

plot(spam[,34],spam[,32])

# PCA: weighted combination of predictors might be better than including every predictor
# Reduces predictors and reduces noise

# E.g., rotate the plot:
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

# Using principal components:
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

# Look at rotation matrix
prComp$rotation


preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

# Create training predictions and fit a model that relates a training variable to the principal component
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~., method="glm", data=trainPC)
# Error - undefined columns

# In test, you have to use the same principal components as training
testPC <- predict(preProc, log10(tesitng[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC)) # get accuracy from confusion matrix

# Can also just incorporate predict function into training exercise
modelFit <- train(training$type ~ ., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))
# Error - undefined columns

# Most useful for linear-type models and can make it harder to interpret predictors