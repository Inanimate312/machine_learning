## Reproducible setup
set.seed(12345)
library(caret)
library(randomForest)

## 1. Download and read data into working directory
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(train_url, destfile = "pml-training.csv", mode = "wb")
download.file(test_url,  destfile = "pml-testing.csv",  mode = "wb")

train_raw <- read.csv("pml-training.csv", na.strings = c("NA", "", "#DIV/0!"))
test_raw  <- read.csv("pml-testing.csv",  na.strings = c("NA", "", "#DIV/0!"))

## 2. Basic cleaning: remove non‑predictive columns and columns with too many NAs
# Remove first few ID/time/name columns
drop_cols <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2",
               "cvtd_timestamp", "new_window", "num_window")
train <- train_raw[, !(names(train_raw) %in% drop_cols)]
test  <- test_raw[,  !(names(test_raw)  %in% drop_cols)]

# Remove columns with almost all NAs
na_frac <- colSums(is.na(train)) / nrow(train)
keep    <- na_frac < 0.95
train   <- train[, keep]
test    <- test[,  keep[names(test)]]

# Ensure outcome is factor
train$classe <- factor(train$classe)

## 3. Create training/validation split for out‑of‑sample error estimate
set.seed(12345)
inTrain <- createDataPartition(train$classe, p = 0.7, list = FALSE)
train_set <- train[inTrain, ]
valid_set <- train[-inTrain, ]

## 4. Cross‑validated model training (Random Forest)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

set.seed(12345)
rf_fit <- train(classe ~ .,
                data = train_set,
                method = "rf",
                trControl = ctrl,
                ntree = 500,
                importance = TRUE)

rf_fit

## 5. Evaluate on validation set (estimated out‑of‑sample error)
valid_pred <- predict(rf_fit, newdata = valid_set)
cm <- confusionMatrix(valid_pred, valid_set$classe)
cm
oos_error <- 1 - cm$overall["Accuracy"]
oos_error

## 6. Refit on full training data (optional but typical for final model)
set.seed(12345)
rf_final <- train(classe ~ .,
                  data = train,
                  method = "rf",
                  trControl = ctrl,
                  ntree = 500,
                  importance = TRUE)

## 7. Predict on the 20 test cases
test_pred <- predict(rf_final, newdata = test)
test_pred

## If you need them as a character vector:
as.character(test_pred