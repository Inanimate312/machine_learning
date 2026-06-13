# 1 
library(ElemStatLearn)

data(vowel.train)

data(vowel.test)

# Answer: The result does not fit to any possible answer. 
# (by aprox) RF Accuracy = 0.6082 
# GBM Accuracy = 0.5152 
# Agreement Accuracy = 0.6361

################################################################################
# 2

library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]
testing = adData[-inTrain,]

# Fit base models
model_rf <- train(diagnosis ~ ., data = training, method = "rf")
model_gbm <- train(diagnosis ~ ., data = training, method = "gbm", verbose = FALSE)
model_lda <- train(diagnosis ~ ., data = training, method = "lda")

# Predict on training set
pred_train_rf <- predict(model_rf, training)
pred_train_gbm <- predict(model_gbm, training)
pred_train_lda <- predict(model_lda, training)

stackData <- data.frame(
  diagnosis = training$diagnosis,
  rf = pred_train_rf,
  gbm = pred_train_gbm,
  lda = pred_train_lda
)

# Fit stacked model using random forest
stack_model <- train(diagnosis ~., data = stackData, method = "rf")

# Predict on test set
pred_test_rf = predict(model_rf, testing)
pred_test_gbm <- predict(model_gbm, testing)
pred_test_lda <- predict(model_lda, testing)

stack_test_data <- data.frame(
  rf = pred_test_rf,
  gbm = pred_test_gbm,
  lda = pred_test_lda
)

pred_stack <- predict(stack_model, stack_test_data)

# Accuracies
acc_rf <- confusionMatrix(pred_test_rf, testing$diagnosis)$overall["Accuracy"]
acc_gbm <- confusionMatrix(pred_test_gbm, testing$diagnosis)$overall["Accuracy"]
acc_lda <- confusionMatrix(pred_test_lda, testing$diagnosis)$overall["Accuracy"]
acc_stack <- confusionMatrix(pred_stack, testing$diagnosis)$overall["Accuracy"]

acc_rf # 0.902
acc_gbm # 0.878
acc_lda # 0.915
acc_stack # 0.939

################################################################################
# 3

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

library(caret)
library(elasticnet)

# Fit lasso model
lassoFit <- enet(x = as.matrix(training[, -9]),
                 y = training$CompressiveStrength,
                 lambda = 0)

# Plot coefficient paths
plot(lassoFit, xvar = "penalty", use.color = TRUE)

# Extract coefficients along regularization path
coef_path <- predict(lassoFit, type = "coefficients", s = lassoFit$lamdbda)

# Look at the coefficient paths
coef_path
# Cement

################################################################################
# 4

url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
dest <- "gaData.csv"

download.file(url, destfile = dest, method = "curl")

dat <- read.csv(dest)

library(lubridate) # For year() function below

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

# Fit a bats() model and forecast
library(forecast)
set.seed(233)

fit_bats <- bats(tstrain)

forecast <- forecast(fit_bats, h = nrow(testing))

# Count testing points within 95% prediction interval
lower <- forecast$lower[,2]
upper <- forecast$upper[,2]
actual <- testing$visitsTumblr

inside <- actual >= lower & actual <= upper

sum(inside) # 226 (~87% coverage)

################################################################################
# 5

library(e1071)
library(caret)

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

# Fit SVM with default settings
svmFit <- svm(CompressiveStrength ~ ., data = training)

# Predict on the testing set
svmPred <- predict(svmFit, newdata = testing)

# Compute RMSE
svm_rmse <- RMSE(svmPred, testing$CompressiveStrength)
svm_rmse # 7.96

# Results
#1 
# RF = 0.6082
# GBM = 0.5152
# Agreement = 0.6361

#2 
# Stacked 0.80 better than all other methods [INCORRECT]
# Stacked 0.93 better than all other methods [INCORRECT]
# Stacked 0.88 better than all other methods [INCORRECT]
# Stacked 0.80 better than rf and lda but same as boosting [CORRECT]

#3 
# Cement

#4
# 96%

#5
# 6.72