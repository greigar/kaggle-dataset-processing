# https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009

source("10_kaggle_common.R")

col_spec <- cols(
  `fixed acidity`        = col_double(),
  `volatile acidity`     = col_double(),
  `citric acid`          = col_double(),
  `residual sugar`       = col_number(),
  chlorides              = col_double(),
  `free sulfur dioxide`  = col_number(),
  `total sulfur dioxide` = col_double(),
  density                = col_double(),
  pH                     = col_number(),
  sulphates              = col_double(),
  alcohol                = col_number(),
  quality                = col_integer()
)

read_wq_csv <- function(wine_colour) {
  filename       <- paste0(data_dir, "winequality-", wine_colour, ".csv")
  wq_data        <- read_delim(filename, col_types=col_spec, delim=';')
  wq_data$colour <- wine_colour
  wq_data
}

white_wine_data  <- read_wq_csv("white")
red_wine_data    <- read_wq_csv("red")

wine_data        <- rbind(red_wine_data, white_wine_data)
names(wine_data) <- make.names(names(wine_data))
wine_data$colour <- as.integer(wine_data$colour == "red")

stop("LPOLPOPLP")

#
# QUALITY
#
inTrain      <- createDataPartition(y = wq_data$quality, p = 0.75, list = FALSE)
tab_training <- wq_data[inTrain,]
tab_testing  <- wq_data[-inTrain,]

tab_model_rf  <- randomForest(quality ~ ., data = as.data.frame(tab_training)) # 0.664
tab_model_crf <- train(quality ~ ., data = tab_training, method = "rf")        # 0.668
tab_model_gbm <- train(quality ~ ., data = tab_training, method = "gbm")       # 0.576
tab_model_xgb <- train(quality ~ ., data = tab_training, method = "xgbTree")   # 0.554

predict_test <- function(model) {
  plot(model)
  tab_predict <- predict(model, newdata = tab_testing[,-12])
  confusionMatrix(round(tab_predict), tab_testing$quality)
}


# COLOUR

inTrain      <- createDataPartition(y = wine_data$colour, p = 0.75, list = FALSE)
tab_training <- wine_data[inTrain,]
tab_testing  <- wine_data[-inTrain,]

tab_model_rf  <- randomForest(colour ~ ., data = as.data.frame(tab_training), importance=T)
varImp(tab_model_rf)

predict_test_colour <- function(model, reference_col_name, reference_col_num) {
  plot(model)
  tab_predict <- predict(model, newdata = tab_testing[,-reference_col_num])
  confusionMatrix(round(tab_predict), eval(parse(text = reference_col_name)))
}

predict_test_colour(tab_model_rf, "tab_testing$colour", 13)
