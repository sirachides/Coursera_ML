library(caret)
library(randomForest)
library(rattle)
library(e1071)
library(rpart)
library(lattice)
#library(rattle) #nice tree plots
library(ggplot2)
library(ISLR)
library(rpart.plot)
library(RColorBrewer)
library(mlbench)
set.seed(1)

#1# Get data #######################################################
# Load cvs file 
trdata.raw <- read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))
tedata.raw <- read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0!"))
# printout and visual check
trdata.raw 
tedata.raw

#2# Cleans data ####################################################
# Drop the first 6 columns with timestamp and irrelevant data plus the one with NA
trdata.clean <- trdata.raw[,7:length(colnames(trdata.raw))]
tedata.clean <- tedata.raw[,7:length(colnames(tedata.raw))]
trdata.clean <- trdata.clean[, colSums(is.na(trdata.clean)) == 0]
tedata.clean <- tedata.clean[, colSums(is.na(tedata.clean)) == 0]

#3# Split Data #####################################################
in.training <- createDataPartition(trdata.clean$classe, p=0.70, list=F)
train.data <- trdata.clean[in.training, ]
validate.data <- trdata.clean[-in.training, ]

#4# Training ########################################################
#Cross validation k-fold=4
control.par <- trainControl(method="cv", 4)
# execute training, prediction for "classe" and random-forest method
rf.model <- train(classe ~ ., data=train.data, method="rf", trControl=control.par, ntree=250)
# print model
rf.model

#5# Check performance on validation data from prediction on training data for training ############################
rf.predict <- predict(rf.model, validate.data)
# Confusion matrix
confusionMatrix(validate.data$classe, rf.predict)
# Accuracy
acc.pred <- postResample(rf.predict, validate.data$classe)
acc.out <- acc.pred[1]

#6# Execute the rf model on cleaned test data #################################
fin.res <- predict(rf.model, tedata.clean[, -length(names(tedata.clean))])
# Print results with corresponding classification
fin.res


