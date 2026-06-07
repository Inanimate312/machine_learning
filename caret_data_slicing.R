library(caret); library(kernlab); data(spam)

# 75% is assigned to training, 25% is assigned to testing
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

# Subset to training and testing sets
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# K-fold example
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE)
sapply(folds,length) # check the length of each fold
folds[[1]][1:10] # Look at samples in the first fold

# Return test
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=FALSE)
sapply(folds,length)
folds[[1]][1:10]

# Resampling (e.g., instead of k-fold)
set.seed(32323)
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds,length)
folds[[1]][1:10]

# Time slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20,horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]