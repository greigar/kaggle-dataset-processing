# https://www.kaggle.com/uciml/breast-cancer-wisconsin-data

source("10_kaggle_common.R")

bcw_data <- read_csv(paste0(data_dir, "breast-cancer-wisconsin-data.csv"))

bcw_data$id  <- NULL
bcw_data$X33 <- NULL

names(bcw_data) <- make.names(names(bcw_data))

inTrain      <- createDataPartition(y = bcw_data$diagnosis, p = 0.75, list = FALSE)
tab_training <- bcw_data[inTrain,]
tab_testing  <- bcw_data[-inTrain,]

tab_model   <- randomForest(factor(diagnosis) ~ ., data = as.data.frame(tab_training))
tab_predict <- predict(tab_model, newdata = tab_testing[,-1])

plot(tab_model)
confusionMatrix(tab_testing$diagnosis, tab_predict)


# Caret RF
tab_model_ct <- train(diagnosis ~ ., data = tab_training, method = "rf")
tab_model_ct <- train(diagnosis ~ ., data = tab_training, method = "gbm")
tab_model_ct <- train(diagnosis ~ ., data = tab_training, method = "xgbTree")

tab_predict_ct <- predict(tab_model_ct, newdata = tab_testing[,-1])

plot(tab_model_ct)
confusionMatrix(tab_testing$diagnosis, tab_predict_ct)


