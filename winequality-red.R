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
  filename      <- paste0(data_dir, "winequality-", wine_colour, ".csv")
  w_data        <- read_delim(filename, col_types=col_spec, delim=';')
  w_data$colour <- wine_colour
  w_data
}

white_wine_data  <- read_wq_csv("white")
red_wine_data    <- read_wq_csv("red")

wine_data        <- rbind(red_wine_data, white_wine_data)
names(wine_data) <- make.names(names(wine_data))
wine_data$colour <- as.integer(wine_data$colour == "red")

predict_test <- function(model, reference_col_name, reference_col_num) {
  plot(model)
  tab_predict <- predict(model, newdata = tab_testing[,-reference_col_num])
  confusionMatrix(round(tab_predict), eval(parse(text = reference_col_name)))
}

varimp_results <- function(model) {
  varimp_temp <- varImp(tab_model_rf)
  tibble(name = rownames(varimp_temp), value = varimp_temp$Overall ) %>% arrange(desc(value))
}

stop("here be dragons")

#
# QUALITY
#
inTrain      <- createDataPartition(y = wine_data$quality, p = 0.75, list = FALSE)
tab_training <- wine_data[inTrain,]
tab_testing  <- wine_data[-inTrain,]

tab_model_rf <- randomForest(quality ~ ., data = as.data.frame(tab_training), importance = TRUE) # 0.664
varimp_results(tab_model_rf)
predict_test(tab_model_rf, "tab_testing$quality", 12)

lm_model_1 <- lm(quality ~ alcohol, wine_data)
lm_model_2 <- lm(quality ~ alcohol + volatile.acidity, tab_training)
lm_model_3 <- lm(quality ~ alcohol + volatile.acidity + free.sulfur.dioxide, tab_training)

anova(lm_model_1, lm_model_3)
ggplot(wine_data, aes(x = alcohol, y = quality)) + geom_point() + geom_smooth(method = lm, formula = y ~ splines::ns(x, 7) )

my_wine <- data.frame(alcohol = c(10.5), volatile.acidity = c(0.34))
my_wine$quality <- round(predict(lm_model_2, newdata <- my_wine))

g <- ggplot(wine_data, aes(x = alcohol, y = quality, colour = volatile.acidity)) +
        geom_point() +
        geom_smooth(method = lm, formula = y ~ splines::ns(x, 5) )
g + geom_point(data= my_wine, aes(x = alcohol, y = quality, colour = volatile.acidity), fill = "white", shape = 21, size = 2)

# make colour guess on wine type
# use better model and then just plot lm
g + geom_point(data= my_wine, aes(x = alcohol, y = quality, fill = volatile.acidity), colour = "red", shape = 21, stroke = 2, size = 4)


my_wine <- data.frame(alcohol = c(14), volatile.acidity = c(0.54))
my_wine$quality <- round(predict(lm_model_2, newdata <- my_wine))
g + geom_point(data= my_wine, aes(x = alcohol, y = quality, colour = volatile.acidity), fill = "white", stroke = 2, shape = 21, size = 2)




#tab_model_crf <- train(quality ~ ., data = tab_training, method = "rf")        # 0.668
#tab_model_gbm <- train(quality ~ ., data = tab_training, method = "gbm")       # 0.576
#tab_model_xgb <- train(quality ~ ., data = tab_training, method = "xgbTree")   # 0.554


#
# COLOUR
#

inTrain      <- createDataPartition(y = wine_data$colour, p = 0.75, list = FALSE)
tab_training <- wine_data[inTrain,]
tab_testing  <- wine_data[-inTrain,]

tab_model_rf  <- randomForest(colour ~ ., data = as.data.frame(tab_training), importance = TRUE)
varImp(tab_model_rf)
predict_test_colour(tab_model_rf, "tab_testing$colour", 13)

