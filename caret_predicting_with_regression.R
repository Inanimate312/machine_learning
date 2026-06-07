library(ISLR); library(ggplot2); library(caret); data(Wage);

# Subset to the part of the dataset that is NOT the variable we are trying to predict (logwage)
Wage <- subset(Wage,select=-c(logwage))
summary(Wage)


inTrain <- createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

dim(training)
dim(testing)

# Feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

# Plot age vs wage
qplot(age,wage,data=training)
qplot(age,wage,color=jobclass,data=training)
qplot(age,wage,color=education,data=training)

# Fit a linear model
modFit <- train(wage ~ age + jobclass + education,
                method = "lm",
                data=training)
finMod <- modFit$finalModel
print(modFit)

# Diagnostics
# Plot residuals vs fitted
plot(finMod,1,pch=19,cex=0.5,col="#00000010")

# Color by variables not used in model
qplot(finMod$fitted, finMod$residuals, color=race, data=training)

# Plot by index (i.e., index = which row of the dataset)
plot(finMod$residuals,pch=19)


# Predicted vs truth in test set
pred <- predict(modFit, testing)
qplot(wage, pred, color=year, data=testing)

# If you want to use all covariates
modFitAll <- train(wage ~., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)
