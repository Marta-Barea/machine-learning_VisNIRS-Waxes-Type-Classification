# Loading Packages

library(doParallel)
library(readxl)
library(prospectr)
library(caret)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggiraphExtra)

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

trctrl <- trainControl(method = "cv", number = 5)

rf_mtry <- expand.grid(.mtry = c(sqrt(ncol(pw_train[,-1]))))

ntrees <- c(seq(2,100,2))
params <- expand.grid(ntrees = ntrees)

store_maxnode <- vector("list", nrow(params))

set.seed(1345)

start_time_1 <- Sys.time()

for(i in 1:nrow(params)){
  ntree <- params[i,1]
  set.seed(1345)
  rf_model <- train(Sample ~., 
                    data = pw_train,
                    method = 'rf',
                    metric = 'Accuracy',
                    tuneGrid = rf_mtry,
                    trControl = trctrl,
                    ntree = ntree)
  store_maxnode[[i]] <- rf_model
}

names(store_maxnode) <- paste("ntrees:", params$ntrees)

rf_results <- resamples(store_maxnode)
rf_results

lapply(store_maxnode, 
       function(x) x$results[x$results$Accuracy == max(x$results$Accuracy),])

total_time_1 <- Sys.time() - start_time_1
total_time_1

# Accuracy vs ntrees

rf_plot <- cbind.data.frame(Accuracy = colMeans(rf_results$values[,c(seq(2,100,2))]),
                            Ntrees = c(seq(1,100,2)))
rf_plot <- as.vector(rf_plot)
accuracy <- (rf_plot$Accuracy)*100
ntrees <- rf_plot$Ntrees

plot(x = ntrees,
     y = accuracy,
     type = "b", 
     pch = 19, 
     lty = 2,
     col = "#4EBBBA",
     xlab = "Number of decision trees",
     ylab = "Accuracy (%) (5-Fold CV)")

# Final RF model

set.seed(1345)

start_time_2 <- Sys.time()

best_rf <- train(Sample ~., 
                 data = pw_train,
                 method = 'rf',
                 metric = 'Accuracy',
                 tuneGrid = rf_mtry,
                 trControl = trctrl,
                 ntree = 100)

total_time_2 <- Sys.time() - start_time_2
total_time_2

best_rf
best_rf$finalModel

# Train set performance

training_error <- predict(best_rf, newdata = pw_train[,-1], type = "raw") 
cmatrix_training <- confusionMatrix(training_error,as.factor(pw_train$Sample))
cmatrix_training

#Test set performance

pred_model <- predict(best_rf, newdata = pw_test[,-1])

cmatrix <- table(prediction = pred_model, reference = pw_test$Sample)
cmatrix

prop.table(cmatrix)
round(prop.table(cmatrix,1)*100, 2)

cmatrix_test <- confusionMatrix(pred_model,as.factor(pw_test$Sample))
cmatrix_test

# Variable Importance

var_imp <- varImp(object = best_rf)

var_imp_1 <- var_imp[['importance']]

names <- rownames(var_imp_1)
rownames(var_imp_1) <- NULL

var_imp_2 <- cbind(names,var_imp_1)

varimp_plot <- plot(varImp(object = best_rf), 
                    top = 20,
                    col = c("#4EBBBA"),
                    ylab = "Wavelength (nm)",
                    xlab = "Importance")

varimp_plot

sp_feat <- as.data.frame(var_imp_2[which(var_imp_2[,2] >= 70),])
sp_feat_1 <- sp_feat[,-2]

spfeatWithoutQuotes = c()

for (onespfeat in sp_feat_1) {
  onespfeat = str_replace_all(onespfeat, "`", "")
  spfeatWithoutQuotes = append(spfeatWithoutQuotes, onespfeat)
}

print(spfeatWithoutQuotes)

sp_selected <- which(names(pw_sg) %in% spfeatWithoutQuotes)
sp <- pw_sg[,c(sp_selected)]
sp$Sample <- pw_sg$Sample

# ANOVA for selected variables

spAsNumericColNames = lapply(spfeatWithoutQuotes, function (x) paste("WN", toString(x), sep=""))

sp_1 <- sp %>% dplyr::select("Sample", everything())

sp_1$Sample <- factor(sp_1$Sample, levels = c("Macro_Wax", "Micro_Wax"))
colnames(sp_1)[2:ncol(sp_1)] <- spAsNumericColNames
               
formulae <- lapply(colnames(sp_1)[2:ncol(sp_1)], function(x) as.formula(paste0(x, " ~ Sample")))

res <- lapply(formulae, function(x) summary(aov(x, data = sp_1)))
names(res) <- format(formulae)
res

# Training a variable-reduced RF model

sp_selected_1 <- which(names(pw_train) %in% spfeatWithoutQuotes)
pw_train_1 <- pw_train[,c(sp_selected_1)]
pw_train_1$Sample <- pw_train$Sample

rf_mtry_1 <- expand.grid(.mtry = c(sqrt(ncol(pw_train_1[,-length(pw_train_1)]))))

set.seed(1345)

reduced_rf <- train(Sample ~., 
                 data = pw_train_1,
                 method = 'rf',
                 metric = 'Accuracy',
                 tuneGrid = rf_mtry_1,
                 trControl = trctrl,
                 ntree = 100)

reduced_rf
reduced_rf$finalModel

# Variable-reduced train set predictions

training_error_1 <- predict(reduced_rf, newdata = pw_train[,-length(pw_train_1)], type = "raw") 
cmatrix_training_1 <- confusionMatrix(training_error,as.factor(pw_train$Sample))
cmatrix_training_1

# Variable-reduced test set predictions

sp_selected_2 <- which(names(pw_test) %in% spfeatWithoutQuotes)
pw_test_1 <- pw_test[,c(sp_selected_2)]
pw_test_1$Sample <- pw_test$Sample

pred_model <- predict(reduced_rf, newdata = pw_test_1[,-length(pw_test_1)])

cmatrix <- table(prediction = pred_model, reference = pw_test_1$Sample)
cmatrix

prop.table(cmatrix)
round(prop.table(cmatrix,1)*100, 2)

cmatrix_test <- confusionMatrix(pred_model,as.factor(pw_test$Sample))
cmatrix_test

# Spectralprint

sp_mean <- aggregate(.~ Sample, sp, mean)

sp_nor <- as.data.frame(t(apply(sp_mean[2:8], 1, function(x) ((x /(max(x)))))))
sp_nor$Sample <- sp_mean$Sample

df <- reshape2::melt(sp_nor, "Sample")

spectral_print <- ggplot(df, aes(x = variable, y = value))+
  geom_polygon(aes(group = Sample, color = Sample), 
              fill = NA, size = 2, show.legend = FALSE) +
  geom_line(aes(group = Sample, color = Sample), size = 2) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol = 2)) +
  coord_radar() +
  theme_bw()+
  annotate("text", x = 0, 
           y = seq(0, 1, 0.25), 
           label = seq(0, 1, 0.25)) +
  theme(axis.text.x = element_text(colour = "black", size = 10), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

spectral_print

# Stop Parallelization

stopCluster(cl)