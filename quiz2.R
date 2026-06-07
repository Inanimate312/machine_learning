#1 
library(AppliedPredictiveModeling); library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

training


## 2 
# There is a non-random pattern in the plot of the outcome versus index that does not appear to be perfectly explained by any predictor suggesting a variable may be missing.

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

## 3
# There are values of zero so when you take the log() transform those values will be -Inf.

## 4 
# 9

## 5 
# Not: non-PCA=0.65, PCA=0.72

library(Hmisc)
library(ggplot2)

names(mixtures)
head(mixtures)

training$Index <- seq_len(nrow(training))

predictors <- c("Cement",
                "BlastFurnaceSlag", 
                "FlyAsh",
                "Water",
                "Superplasticizer",
                "CoarseAggregate",
                "FineAggregate",
                "Age")

plots <- lapply(predictors, function(var) {
  ggplot(training, aes(Index, CompressiveStrength,
                       color = cut2(.data[[var]], g = 4))) + 
    geom_point() +
    labs(title = paste("Colored by", var),
         color = paste(var, "(cut2 groups)")) +
    theme_minimal()
})

library(patchwork)
wrap_plots(plots)

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

il_vars <- grep("^IL", names(training), value=TRUE)

pp <- preProcess(training[, il_vars], method="pca", thresh=0.8)
pp$numComp

rm(list = ls())

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# extract new training and testing sets
IL_col_idx <- grep("^[Ii][Ll].*", names(training))
suppressMessages(library(dplyr))
new_training <- training[, c(names(training)[IL_col_idx], "diagnosis")]
names(new_training)

IL_col_idx <- grep("^[Ii][Ll].*", names(testing))
suppressMessages(library(dplyr))
new_testing <- testing[, c(names(testing)[IL_col_idx], "diagnosis")]
names(new_testing)

# compute the model with non_pca predictors
non_pca_model <- train(diagnosis ~ ., data=new_training, method="glm")
# apply the non pca model on the testing set and check the accuracy
non_pca_result <- confusionMatrix(new_testing[, 13], predict(non_pca_model, new_testing[, -13]))
non_pca_result
# 0.7561

# perform PCA extraction on the new training and testing sets
pc_training_obj <- preProcess(new_training[, -13], method=c('center', 'scale', 'pca'), thresh=0.8)
pc_training_preds <- predict(pc_training_obj, new_training[, -13])
pc_testing_preds <- predict(pc_training_obj, new_testing[, -13])

# ADD diagnosis back in
pc_training <- data.frame(diagnosis = new_training$diagnosis, pc_training_preds)
pc_testing  <- data.frame(diagnosis = new_testing$diagnosis,  pc_testing_preds)

# Now train the PCA model
pca_model <- train(diagnosis ~ ., data = pc_training, method = "glm")

# apply the PCA model on the testing set
pca_result <- confusionMatrix(new_testing[, 13], predict(pca_model, pc_testing_preds))
pca_result
# 0.7195

# But answer is not: 
# Non-PCA=0.75
# PCA=0.71

# The answer is not:
# Non-PCA=0.72
# PCA=0.65

# The answer is not:
# Non-PCA=0.72
# PCA=0.71
