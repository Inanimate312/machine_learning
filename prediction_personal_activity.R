# Initialize
set.seed(54321)
library(caret)
library(randomForest)

# Download and read in data
training_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(training_url, 
              destfile = "pml-training.csv", 
              mode = "wb")

download.file(testing_url, 
              destfile = "pml-testing.csv", 
              mode = "wb")

training_raw <- read.csv("pml-training.csv", 
                         na.strings = c("NA","","#DIV/0!"))

testing_raw <- read.csv("pml-testing.csv", 
                        na.strings = c("NA","","#DIV/0!"))

# Remove non-predictive columns
drop_columns <- c("X",
                  "user_name",
                  "raw_timestamp_part_1",
                  "raw_timestamp_part_2",
                  "cvtd_timestamp",
                  "new_window",
                  "num_window")

training <- training_raw[, !(names(training_raw) %in% drop_columns)]
testing <- testing_raw[, !(names(testing_raw) %in% drop_columns)]


# Remove columns with mostly NA values
na_fraction <- colSums(is.na(training)) / nrow(training)
keep_names <- names(training)[na_fraction < 0.95]
keep_names <- intersect(keep_names, names(testing))

training <- training[, keep_names]
testing <- testing[, keep_names]

# Restore the outcome variable "classe" to the training set
training$classe <- factor(training_raw$classe)

# Split training into training and validation (to estimate OOS error)
set.seed(54321)
inTraining <- createDataPartition(training$classe, p = 0.7, list = FALSE)

training_set <- training[inTraining, ]
validation_set <- training[-inTraining, ]

# Build cross-validated random forest model
control <- trainControl(method = "cv", number = 5)

set.seed(54321)
rf_fit <- train(classe ~., 
                data = training_set, 
                method = "rf",
                trControl = control,
                ntree = 500,
                importance = TRUE)

# Evaluate on validation set
validation_predictions <- predict(rf_fit, newdata = validation_set)
cm <- confusionMatrix(validation_predictions, validation_set$classe)
oos_error <- 1 - cm$overall["Accuracy"]
oos_error # 0.0076

# Refit on the full training data
set.seed(54321)
rf_final <- train(classe ~.,
                  data = training,
                  method = "rf",
                  trControl = control,
                  ntree = 500,
                  importance = TRUE)

# Predict on test data
test_predictions <- predict(rf_final, newdata = testing)
test_predictions

# Visualizations

## Variable importance plot
plot(varImp(rf_fit), top = 20, main = "Top 20 Important Predictors")

## Confusion Matrix heatmap
library(ggplot2)

cm_df <- as.data.frame(cm$table)
ggplot(cm_df, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ggtitle("Confusion Matrix Heatmap")


## PCA visualization of predictors
pca <- prcomp(training_set[, -ncol(training_set)], center = TRUE, scale. = TRUE)
pca_df <- data.frame(pca$x[, 1:2], classe = training_set$classe)

ggplot(pca_df, aes(PC1, PC2, color = classe)) +
  geom_point(alpha = 0.5) +
  ggtitle("PCA of Training Predictors")
