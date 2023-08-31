# Loading Packages

library(doParallel)
library(readxl)
library(prospectr)
library(caret)
library(dplyr)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

#Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/Vis-NIR/XDS-NIR_FOSS/Estudio según Tipo de Parafina e Hidrotratamiento/NIRS_PW_Type_Hydrotreating.xlsx", 
                      sheet = "PW_Type")

# First derivative and Savitzky Golay Smoothing

pw_data$Sample <- as.factor(pw_data$Sample)

sgvec <- savitzkyGolay(X = pw_data[,-1], p = 3, w = 11, m = 1)
pw_sg <- cbind.data.frame(Sample = pw_data$Sample, sgvec)

# Data partition

set.seed(1345)

intrain <- createDataPartition(y = pw_sg$Sample, 
                               p = 0.7, 
                               list = FALSE)
pw_train <- pw_sg[intrain,]
pw_test <- pw_sg[-intrain,]

# Hyperparameters tuning and model training

set.seed(1345)

trctrl_1 <- trainControl(method = "cv", number = 5)
gridlinear_1 <- expand.grid(C = c(2^(seq(-10, 10, 0.5)))) 
                    
set.seed(1345)

start_time_1 <- Sys.time()

svm_linear <- train(Sample ~., 
                    data = pw_train,
                    method = "svmLinear",
                    trControl= trctrl_1,
                    tuneGrid = gridlinear_1,
                    metric = "Accuracy")

total_time_1 <- Sys.time() - start_time_1
total_time_1

svm_linear
svm_linear$finalModel
filter(svm_linear[['results']], 
       C == svm_linear[["bestTune"]][["C"]])

# Final SVM model

set.seed(1345)

trctrl_2 <- trainControl(method = "cv", number = 5)
gridlinear_2 <- expand.grid(C = svm_linear[["bestTune"]][["C"]]) 
                          
set.seed(1345)

start_time_2 <- Sys.time()

best_svm <- train(Sample ~.,                         
                  data = pw_train,
                  method = "svmLinear",
                  trControl= trctrl_2,
                  tuneGrid = gridlinear_2,
                  metric = "Accuracy")
best_svm

total_time_2 <- Sys.time() - start_time_2
total_time_2

# Train set predictions

training_error <- predict(best_svm, newdata = pw_train[,-1], type = "raw") 
cmatrix_training <- confusionMatrix(training_error,as.factor(pw_train$Sample))
cmatrix_training

# Test set predictions

pred_model <- predict(best_svm, newdata = pw_test[,-1])

cmatrix <- table(prediction = pred_model, reference = pw_test$Sample)
cmatrix

prop.table(cmatrix)
round(prop.table(cmatrix,1)*100, 2)

cmatrix_test <- confusionMatrix(pred_model,as.factor(pw_test$Sample))
cmatrix_test

# Stop Parallelization

stopCluster(cl)