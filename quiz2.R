library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

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

pp <- preProcess(training[, il_vars], method="pca", thresh=0.9)
pp$numComp

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[,grep("^IL|diagnosis", names(training))]
testingIL <- testing[,grep("^IL|diagnosis", names(testing))]

# non-PCA
model <- train(diagnosis ~., data = trainingIL, method = "glm")
predict_model <- predict(model, newdata=testingIL)
matrix_model <- confusionMatrix(predict_model,testingIL$diagnosis)
matrix_model$overall[1]

# PCA
modelPCA <- train(diagnosis ~., data = trainingIL, method = "glm", preProcess = "pca", trControl = trainControl(preProcOptions=list(thresh=0.8)))
matrix_modelPCA <- confusionMatrix(testingIL$diagnosis, predict(modelPCA, testingIL))
matrix_modelPCA$overall[1]



####


trainingIL<-training[, c(grep("^IL", names(training)), 1)]
testingIL<-testing[, c(grep("^IL", names(testing)), 1)]

# Model that uses all the predictors as they are 
mdfit1<-train(diagnosis~., method="glm", data=trainingIL)
confusionMatrix(testing$diagnosis, predict(mdfit1, testing))

# Model that uses PCA explaining 80% of variance
preProc<-preProcess(trainingIL[, -13], method="pca", thresh = 0.8)
trainPC<-predict(preProc, trainingIL[, -13])
mdfit2<-train(x=trainPC, y=trainingIL$diagnosis, method="glm")

testPC<-predict(preProc, testingIL[, -13])
confusionMatrix(testingIL$diagnosis, predict(mdfit2, testPC))

# The below method for the PCA model yields similar results 
#mdfit2<-train(diagnosis~., method="glm", preProcess="pca", data=trainingIL)
#confusionMatrix(testingIL$diagnosis, predict(mdfit2, testingIL))
