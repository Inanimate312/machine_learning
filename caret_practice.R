# SPAM Example

# Data splitting into training and testing sets
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

#Fit a model (we're trying to predict 'type' based on all other variables)
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
modelFit

dim(modelFit)

# Final Model
modelFit <- train(type ~., data=training, method="glm")
modelFit$finalModel

# Prediction
predictions <- predict(modelFit,newdata=testing)
predictions

# Confusion Matrix
confusionMatrix(predictions,testing$type)