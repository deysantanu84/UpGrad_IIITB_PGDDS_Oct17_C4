# Support Vector Machine Assignment
# Date: 22nd April 2018
# Author: Santanu Dey (DDA1730043, Upgrad IIITB PGDDS Cohort 4)

# Load relevant libraries
library(ggplot2)
library(kernlab)
library(caret)
library(caTools)
library(gridExtra)

# Load the data
mnist_train <- read.csv("mnist_train.csv", stringsAsFactors = F, header = F)
mnist_test <- read.csv("mnist_test.csv", stringsAsFactors = F, header = F)

names(mnist_test)[1] <- "label"
names(mnist_train)[1] <- "label"

# Data cleaning
# Check for Duplicates
sum(duplicated(mnist_train)) # 0 duplicates
sum(duplicated(mnist_test)) # 0 duplicates

# Check for NAs
sum(sapply(mnist_train, function(x) sum(is.na(x)))) # 0 NAs
sum(sapply(mnist_test, function(x) sum(is.na(x)))) # 0 NAs

# Data understanding
str(mnist_train) # All columns are of type integer
str(mnist_test) # All columns are of type integer

summary(mnist_train)
summary(mnist_test)

# Data preparation
mnist_train$label <- factor(mnist_train$label)
mnist_test$label <- factor(mnist_test$label)

# Due to huge computation time, we will perform our modelling on a sample of 15% of the training and test datasets
set.seed(100)
sample_train_indices <- sample(1: nrow(mnist_train), 9000)
train <- mnist_train[sample_train_indices, ]

sample_test_indices <- sample(1: nrow(mnist_test), 1500)
test <- mnist_test[sample_test_indices, ]

# Data Scaling
max(train[, 2:ncol(train)]) # Max pixel size (255)
train[, 2:ncol(train)] <- train[, 2:ncol(train)]/255

max(test[, 2:ncol(test)]) # Max pixel size (255)
test[, 2:ncol(test)] <- test[, 2:ncol(test)]/255

# Exploratory Data Analysis
plot1 <- ggplot(mnist_train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_minimal() +
    labs(y = "Frequency", title = "mnist_train") + 
    scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
    geom_text(stat = "count", 
              aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot2 <- ggplot(mnist_test, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_minimal() +
    labs(y = "Frequency", title = "mnist_test") + 
    scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
    geom_text(stat = "count", 
              aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot3 <- ggplot(train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_minimal() +
    labs(y = "Frequency", title = "train") + 
    scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
    geom_text(stat = "count", 
              aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot4 <- ggplot(test, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_minimal() +
    labs(y = "Frequency", title = "test") + 
    scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
    geom_text(stat = "count", 
              aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

grid.arrange(plot1, plot2, plot3, plot4, nrow = 4)

# We observe that the frequencies of the digits in the original training and test datasets are similar to that of the respective sample datasets.

# Constructing Model
# Using Linear Kernel
Model_linear <- ksvm(label~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test, type = "response")

# Confusion matrix - Linear Kernel
confusionMatrix(Eval_linear, test$label)

# Observations:
# Accuracy: 92.6%
# Sensitivity: 86.275% - 99.31%
# Specificity: 98.668% - 99.63%

# Using RBF Kernel
Model_rbf <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "rbfdot",
                  C = 1, kpar = "automatic")
Eval_rbf <- predict(Model_rbf, test, type = "response")

# Confusion matrix - RBF Kernel
confusionMatrix(Eval_rbf, test$label) 

# Observations:
# Accuracy: 95.67%
# Sensitivity: 91.57% - 100%
# Specificity: 99.267% - 99.704%

# Hyperparameter tuning and Cross Validation
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )

fit.svm <- train(label ~ ., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
plot(fit.svm)
# Accuracy is highest at C = 2 and sigma = 0.025

Eval_best_fit_rbf <- predict(fit.svm, newdata = test)

# Confusion matrix - RBF Kernel - Best Fit
confusionMatrix(Eval_best_fit_rbf, test$label)

# Observations:
# Accuracy: 96.67%
# Sensitivity: 92.7% - 100%
# Specificity: 99.41% - 99.852%