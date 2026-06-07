# Two levels of creating covariates:
# Level 1 - Raw data to covariates depends heavily on application
# Need to balance summarization vs information loss

# Level 2 - Tidy covariates into new covariates
# More necessary for methods like regression, svms
# Should only be done on the training set

# Load example data
library(ISLR); library(caret); data(Wage);

inTrain <- createDataPartition(y=Wage$wage,p=0.7,list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# Common covariates to add: 
# 1) dummy variables (turn qualitative factor variables into indicator variables)

table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data=training)
# This turns qualitative "industrial" or "information" variables into 1/0 indicators

head(predict(dummies,newdata=training))

# 2) Removing zero covariates (variables with no variability, for example)
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

# 3) Spline basis
library(splines)
bsBasis <- bs(training$age,df=3)
# e.g., this will give age, age-squared, and age-cubed, to allow for curve fitting
bsBasis

# Fit curve with splines
lm1 <- lm(wage ~bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

# Splines on the test set
predict(bsBasis,age=testing$age)
