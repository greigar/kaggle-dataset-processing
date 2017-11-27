# https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009

source("10_kaggle_common.R")

wine_colour <- "white"
filename    <- paste0(data_dir, "winequality-", wine_colour, ".csv")

col_spec <- cols(
  `fixed acidity` = col_double(),
  `volatile acidity` = col_double(),
  `citric acid` = col_double(),
  `residual sugar` = col_number(),
  chlorides = col_double(),
  `free sulfur dioxide` = col_number(),
  `total sulfur dioxide` = col_double(),
  density = col_double(),
  pH = col_number(),
  sulphates = col_double(),
  alcohol = col_number(),
  quality = col_integer()
)

wq_data <- read_delim(filename, col_types=col_spec, delim=';')

names(wq_data) <- make.names(names(wq_data))

inTrain      <- createDataPartition(y = wq_data$quality, p = 0.75, list = FALSE)
tab_training <- wq_data[inTrain,]
tab_testing  <- wq_data[-inTrain,]

tab_model_rf  <- randomForest(quality ~ ., data = as.data.frame(tab_training))
tab_model_crf <- train(quality ~ ., data = tab_training, method = "rf")
tab_model_gbm <- train(quality ~ ., data = tab_training, method = "gbm")
tab_model_xgb <- train(quality ~ ., data = tab_training, method = "xgbTree")

predict_test <- function(model) {
  plot(model)
  tab_predict <- predict(model, newdata = tab_testing[,-12])
  confusionMatrix(round(tab_predict), tab_testing$quality)
}
