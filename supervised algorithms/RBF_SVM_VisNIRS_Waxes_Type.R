###############################################################################
####### Application of a system based on Vis-NIRS and Machine Learning ########
################### for the analysis of petroleum waxes #######################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
########## Support Vector Machine (SVM) Radial Basis Function (RBF) ###########
###############################################################################

# Loading Packages

library(doParallel)
library(readxl)
library(prospectr)
library(caret)
library(dplyr)
library(graphics)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

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
gridradial_1 <- expand.grid(sigma = c(2^(seq(-10, 10, 0.5))), 
                            C = c(2^(seq(-10, 10, 0.5))))
set.seed(1345)

start_time_1 <- Sys.time()

svm_radial <- train(Sample ~., 
                    data = pw_train,
                    method = "svmRadial",
                    trControl= trctrl_1,
                    tuneGrid = gridradial_1,
                    metric = "Accuracy")

total_time_1 <- Sys.time() - start_time_1
total_time_1

svm_radial
svm_radial$finalModel
filter(svm_radial[['results']], 
       C == svm_radial[["bestTune"]][["C"]], 
       sigma == svm_radial[["bestTune"]][["sigma"]])

# Final SVM model

set.seed(1345)

trctrl_2 <- trainControl(method = "cv", number = 5)
gridradial_2 <- expand.grid(sigma = svm_radial[["bestTune"]][["sigma"]], 
                            C = svm_radial[["bestTune"]][["C"]])
set.seed(1345)

start_time_2 <- Sys.time()

best_svm <- train(Sample ~.,                         
                  data = pw_train,
                  method = "svmRadial",
                  trControl= trctrl_2,
                  tuneGrid = gridradial_2,
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

# Contour plot of the SVM model

accuracy_gridsearch <- matrix((svm_radial[["results"]][["Accuracy"]] * 100), ncol = 41, nrow = 41)

kappa_gridsearch <- matrix(svm_radial[["results"]][["Kappa"]], ncol = 41, nrow = 41)

cost_expression <- expression(log[2] ~ C)
gamma_expression <- expression(log[2] ~ γ)

filled.contour(x = c(seq(-10,10,length.out = 41)),
               y = c(seq(-10,10,length.out = 41)),
               z = as.matrix(accuracy_gridsearch),
               color.palette = colorRampPalette(c("blue", "purple", "red", "orange")), 
               plot.title = title(main = "", sub = "", xlab = cost_expression, ylab = gamma_expression),
               plot.axes = {axis(1,seq(-10, 10, 1),cex.axis = 1,las = 2)
                       axis(2,seq(-10, 10, 1),cex.axis = 1,las = 2)})

filled.contour(x = c(seq(-10,10,length.out = 41)),
               y = c(seq(-10,10,length.out = 41)),
               z = as.matrix(kappa_gridsearch),
               color.palette = colorRampPalette(c("blue", "purple", "red", "orange")), 
               plot.title = title(main = "", sub = "", xlab = cost_expression, ylab = gamma_expression),
               plot.axes = {axis(1,seq(-10, 10, 1),cex.axis = 1,las = 2)
                       axis(2,seq(-10, 10, 1),cex.axis = 1,las = 2)})

# Stop Parallelization

stopCluster(cl)