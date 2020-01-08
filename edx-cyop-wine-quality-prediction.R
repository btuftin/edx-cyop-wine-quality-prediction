

### Loading, and if necessary installing, packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# For SVMLinear
if(!require(kernlab)) install.packages("caret", repos = "http://cran.us.r-project.org")
# For ordinalForest
if(!require(ranger)) install.packages("ranger", repost = "http://cran.us.r-project.org")


### Downloading data and creating training and validation sets

# It's unnecessary to download the data every time we run
# the code, so we'll store it to a file and check
# for that file when code is run again.

f = "wine-quality-basic.RData"
if (file.exists(f)) {
  load(f)
} else {

  # Wine quality dataset from UCI Machine Learning Repository
  # https://archive.ics.uci.edu/ml/datasets/Wine+Quality
  # https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv
  
  all_data <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", delim = ";", col_types = )
  
  # We'll reserve 10% of the data to verify the algorithms at the end.
  # If executing on an older version of R use:
  # set.seed(1)
  set.seed(1, sample.kind="Rounding")
  
  # To ensure the various qualities are represented in both training set and test set
  # the quality column is duplicated as a factor.
  all_data <- all_data %>% mutate(quality_factor = as.factor(quality))
  
  # Creating an index for 10% of the data based on that factor
  test_index <- createDataPartition(y = all_data$quality_factor, times = 1, p = 0.1, list = FALSE)
  training_data <- all_data[-test_index,]
  test_data <- all_data[test_index,]
  
  # saving dataset to file 
    save(all_data, training_data, test_data, file = f)
}
    
### Exploring data    
    
## Plotting the Distribution of quality scores in the training data

training_data %>%
ggplot(aes(quality)) +
  geom_bar()


### Plotting variations in physicochemical measurements between qualities
### Normalizing the data and pivoting to long format}
# Changing quality to factor to simplify call to mutate_if and change order
# The order decides position in plot
normalized <- training_data %>% mutate(quality = factor(as.character(quality), levels = c("3", "4", "5", "6", "7", "8"))) %>% mutate_if(is.numeric, scale)

# Remove the other quality factor to simplify call to pivot_longer
# pivot and change order of factors to get them alphabetically on y-axis
normalized <- normalized[-13] %>% pivot_longer(-quality, names_to = "measurement", values_to = "value") %>% mutate(measurement = factor(measurement, rev(levels(as.factor(measurement)))))

# Plot
normalized %>% filter(quality == 5 | quality == 6) %>%
  ggplot(aes(measurement, value, fill = quality)) +
  geom_boxplot() +
  coord_flip() +
  # Reverse order of legend labels to match the flipped y-axis
  scale_fill_discrete(guide = guide_legend(reverse=TRUE))

### Plotting Variations in physicochemical measurements between quality groups
# Grouping classifications}
group_quality <- function(q) {
  if ( q == "3" | q == "4" ) return("poor")
  else {if (q == "5" | q == "6") return("middling")
  else return("good")}
}

# Adding a group quality variable to each observation
# Specifying order of quality factors to get the correct order in plot.
normalized %>% mutate(quality_group = factor(sapply(quality, group_quality), levels = c("poor", "middling", "good"))) %>%
# Plotting the different measurements by quality group
  ggplot(aes(measurement, value, fill = quality_group)) +
  geom_boxplot() +
  coord_flip() +
  # Reverse y-axis to get elements in alphabetical order from top to bottom
  scale_fill_discrete(guide = guide_legend(reverse=TRUE))


### Modeling

# Error Metrics

# Function for REC
# REC counts as a hit a prediction that falls within 'tolerance' of
# the actual observation
# prediction and observation both need to be numerical
REC <- function(pred, obsv, tolerance = 0.5, list = FALSE) {
  hits <- vector(length = length(obsv))
  for (i in 1:length(obsv)) {
    # Using = for both terms to deal with whole number
    if (pred[i] >= obsv[i] - tolerance & pred[i] <= obsv[i] + tolerance) {
      hits[i] <- TRUE
    } 
    else hits[i] <- FALSE
  }
  if(!list)
    return(mean(hits))
  else
    return(hits)
}

## Naive approaches

# Error characteristics - naive prediction
pred <- numeric(length = length(training_data$quality)) + 6
rec_5 <- REC(pred, training_data$quality, tolerance = 0.5)
rec_1 <- REC(pred, training_data$quality, tolerance = 1, list = TRUE)

results <- tibble(model = "Naive", 
                  `T=0.5` = rec_5,
                  `T=1.0` = mean(rec_1))

rec_class <- training_data %>% mutate(hits = rec_1) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class <- tibble(model = "Naive",
                           "3" = rec_class[[1,2]],
                           "4" = rec_class[[2,2]],
                           "5" = rec_class[[3,2]],
                           "6" = rec_class[[4,2]],
                           "7" = rec_class[[5,2]],
                           "8" = rec_class[[6,2]])

results %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic (tolerances 0.5 and 1.0).")
results_by_class %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic class (tolerance 1.0)")

# Linear Model

# Error characteristics I - linear regression
train_lm <- train(quality ~ ., training_data[-13], method = "lm")
lm_predictions <- predict(train_lm, training_data)

results <- results %>% add_row(model = "Linear Regression",
                  `T=0.5` = REC(lm_predictions, 
                                training_data$quality, 
                                tolerance = 0.5),
                  `T=1.0` = REC(lm_predictions, 
                                training_data$quality, 
                                tolerance = 1))

results %>% knitr::kable(digits = 3, caption = "Mean Absolute Deviation and Regression Error Characteristic (tolerances 0.5 and 1.0).")

# Error characteristics II - linear model}
rec_1 <- REC(lm_predictions, training_data$quality, tolerance = 1, list = TRUE)

rec_class <- training_data %>% mutate(hits = rec_1) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class <- results_by_class %>% add_row(model = "Linear Regression",
                           "3" = rec_class[[1,2]],
                           "4" = rec_class[[2,2]],
                           "5" = rec_class[[3,2]],
                           "6" = rec_class[[4,2]],
                           "7" = rec_class[[5,2]],
                           "8" = rec_class[[6,2]])

results_by_class %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic class (tolerance 1.0)")

# Boxplot of predictions by observed class, fig.cap="Boxplot of predictions by observed class"}
tibble(class = training_data$quality_factor,
       prediction = lm_predictions) %>%
  ggplot(aes(class, prediction)) +
  geom_boxplot()

## Support Vector Machine

# Training SVM approach with linear kernel
set.seed(1, sample.kind="Rounding")
train_svm <- train(quality ~ ., training_data[-13], method = "svmLinear", tuneGrid = data.frame(C = seq(0.001,0.02,0.0005)))
ggplot(train_svm)

# Error Characteristics - SVM
svm_predictions <- predict(train_svm, training_data)
results <- results %>% add_row(model = "SVM - linear", 
                              `T=0.5` = REC(svm_predictions, 
                                          training_data$quality, 
                                          tolerance = 0.5),
                              `T=1.0` = REC(svm_predictions, 
                                            training_data$quality, 
                                            tolerance = 1))

results %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic (tolerances 0.5 and 1.0).")

# Error characteristics II - SVM linear}
rec_2 <- REC(svm_predictions, training_data$quality, tolerance = 1, list = TRUE)

rec_2_class <- training_data %>% mutate(hits = rec_2) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class <- results_by_class %>% add_row(model = "SVM Linear",
                           "3" = rec_2_class[[1,2]],
                           "4" = rec_2_class[[2,2]],
                           "5" = rec_2_class[[3,2]],
                           "6" = rec_2_class[[4,2]],
                           "7" = rec_2_class[[5,2]],
                           "8" = rec_2_class[[6,2]])

results_by_class %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic class (tolerance 1.0)")

# Boxplot of predictions per class - linear model and svm linear
tibble(class = training_data$quality_factor,
       lm_prediction = lm_predictions,
       svm_prediction = svm_predictions) %>%
  pivot_longer(-class, names_to = "model", values_to = "prediction") %>%
  ggplot(aes(class, prediction, color = model)) +
  geom_boxplot()

### SVM Polynomial

# Training an SVM polynomial model

set.seed(1, sample.kind="Rounding")
tuneGrid = data.frame(C = seq(0.003,0.008,0.001),
                      degree = c(2, 3),
                      scale = 1)
train_svmP <- train(quality ~ ., training_data[-13], method = "svmPoly", tuneGrid = tuneGrid)
ggplot(train_svmP)


# Error characteristics - SVM poly
svmP_predictions <- predict(train_svmP, training_data)
results <- results %>% add_row(model = "SVM poly", 
                  `T=0.5` = REC(svmP_predictions, 
                                training_data$quality, 
                                tolerance = 0.5),
                  `T=1.0` = REC(svmP_predictions, 
                                training_data$quality, 
                                tolerance = 1))

results %>% knitr::kable(digits = 3, caption = "Mean Absolute Deviation and Regression Error Characteristic (tolerances 0.5 and 1.0).")

# Error characteristics II - SVM poly
rec_3 <- REC(svmP_predictions, training_data$quality, tolerance = 1, list = TRUE)

rec_3_class <- training_data %>% mutate(hits = rec_3) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class <- results_by_class %>% add_row(model = "SVM Poly",
                           "3" = rec_3_class[[1,2]],
                           "4" = rec_3_class[[2,2]],
                           "5" = rec_3_class[[3,2]],
                           "6" = rec_3_class[[4,2]],
                           "7" = rec_3_class[[5,2]],
                           "8" = rec_3_class[[6,2]])

results_by_class %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic class (tolerance 1.0)")



### Ordinal Random Forest

# tuning ordinal random forest

set.seed(100, sample.kind = "Rounding")
tuneGrid = expand.grid(nsets = seq(50, 150, 100),
                       ntreeperdiv = seq(50, 250, 100),
                       ntreefinal = seq(500, 700, 100))
train_orf <- train(quality_factor ~ ., training_data[-12], method = "ordinalRF", tuneGrid = tuneGrid)
ggplot(train_orf)

# Error characteristics - ORF
orf_predictions <- as.numeric(as.character(predict(train_orf, training_data)))
results <- results %>% add_row(model = "ORF", 
                  `T=0.5` = REC(orf_predictions, 
                                training_data$quality, 
                                tolerance = 0.5),
                  `T=1.0` = REC(orf_predictions, 
                                training_data$quality, 
                                tolerance = 1.01))

results %>% knitr::kable(digits = 3, caption = "Mean Absolute Deviation and Regression Error Characteristic (tolerances 0.5 and 1.0).")


# Box plot comparing SVM - polynomial and Ordinal Random Forest

tibble(class = training_data$quality_factor,
       svmp_prediction = svmP_predictions,
       orf_prediction = orf_predictions) %>%
  pivot_longer(-class, names_to = "model", values_to = "prediction") %>%
  ggplot(aes(class, prediction, color = model)) +
  geom_boxplot()

# Training Ordinal Random Forest - smaller forests

set.seed(100, sample.kind = "Rounding" )
tuneGrid = expand.grid(nsets = 50, 
                       ntreeperdiv = 50,
                       ntreefinal = seq(200, 500, 50))
train_orf_smaller <- train(quality_factor ~ ., training_data[-12], method = "ordinalRF", tuneGrid = tuneGrid)
ggplot(train_orf_smaller)

# Training Ordinal Random Forest - smaller forests - results with error bars, fig.cap="Training results with error bars for smaller forests"}
train_orf_smaller$results %>% ggplot(aes(x = ntreefinal, y = Accuracy,ymax = Accuracy + AccuracySD, ymin = Accuracy - AccuracySD)) +
  geom_point() +
  geom_errorbar()

# Training Ordinal Random Forest - tiny forests

set.seed(100, sample.kind = "Rounding" )
tuneGrid = expand.grid(nsets = 50, 
                       ntreeperdiv = 50,
                       ntreefinal = seq(50, 250, 25))
train_orf_tiny <- train(quality_factor ~ ., training_data[-12], method = "ordinalRF", tuneGrid = tuneGrid)
ggplot(train_orf_tiny)


# Error characteristics - smaller ORFs
orf_predictions_smaller <- as.numeric(as.character(predict(train_orf_smaller, training_data)))
results <- results %>% add_row(model = "ORF - smaller", 
                  `T=0.5` = REC(orf_predictions_smaller, 
                                training_data$quality, 
                                tolerance = 0.5),
                  `T=1.0` = REC(orf_predictions_smaller, 
                                training_data$quality, 
                                tolerance = 1.01))

orf_predictions_tiny <- as.numeric(as.character(predict(train_orf_smaller, training_data)))
results <- results %>% add_row(model = "ORF - tiny", 
                  `T=0.5` = REC(orf_predictions_tiny, 
                                training_data$quality, 
                                tolerance = 0.5),
                  `T=1.0` = REC(orf_predictions_tiny, 
                                training_data$quality, 
                                tolerance = 1.01))

results %>% knitr::kable(digits = 3, caption = "Mean Absolute Deviation and Regression Error Characteristic (tolerances 0.5 and 1.0).")



### Results

# Predictions on test data for all trained models}
# Creating tibbles for results and evaluating predictions on 
# linear model with test data
lm_predictions_test <- predict(train_lm, test_data)

results_final <- tibble(model = "Linear Regression",
                  `T=0.5` = REC(lm_predictions_test, 
                                test_data$quality, 
                                tolerance = 0.5),
                  `T=1.0` = REC(lm_predictions_test, 
                                test_data$quality, 
                                tolerance = 1))

rec_1 <- REC(lm_predictions_test, test_data$quality, tolerance = 1, list = TRUE)

rec_class <- test_data %>% mutate(hits = rec_1) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class_final <- tibble(model = "Linear Regression",
                           "3" = rec_class[[1,2]],
                           "4" = rec_class[[2,2]],
                           "5" = rec_class[[3,2]],
                           "6" = rec_class[[4,2]],
                           "7" = rec_class[[5,2]],
                           "8" = rec_class[[6,2]])

# Adding results from predicting on test data for SVM linear
svm_predictions_test <- predict(train_svm, test_data)
results_final <- results_final %>% add_row(model = "SVM - linear", 
                              `T=0.5` = REC(svm_predictions_test, 
                                          test_data$quality, 
                                          tolerance = 0.5),
                              `T=1.0` = REC(svm_predictions_test, 
                                            test_data$quality, 
                                            tolerance = 1))

rec_2 <- REC(svm_predictions_test, test_data$quality, tolerance = 1, list = TRUE)

rec_2_class <- test_data %>% mutate(hits = rec_2) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class_final <- results_by_class_final %>% add_row(model = "SVM Linear",
                           "3" = rec_2_class[[1,2]],
                           "4" = rec_2_class[[2,2]],
                           "5" = rec_2_class[[3,2]],
                           "6" = rec_2_class[[4,2]],
                           "7" = rec_2_class[[5,2]],
                           "8" = rec_2_class[[6,2]])

# Adding results from predicting on test data for SVM polynomial
svmP_predictions_test <- predict(train_svmP, test_data)
results_final <- results_final %>% add_row(model = "SVM - polynomial", 
                              `T=0.5` = REC(svmP_predictions_test, 
                                          test_data$quality, 
                                          tolerance = 0.5),
                              `T=1.0` = REC(svmP_predictions_test, 
                                            test_data$quality, 
                                            tolerance = 1))

rec_3 <- REC(svmP_predictions_test, test_data$quality, tolerance = 1, list = TRUE)

rec_3_class <- test_data %>% mutate(hits = rec_3) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class_final <- results_by_class_final %>% add_row(model = "SVM polynomial",
                           "3" = rec_3_class[[1,2]],
                           "4" = rec_3_class[[2,2]],
                           "5" = rec_3_class[[3,2]],
                           "6" = rec_3_class[[4,2]],
                           "7" = rec_3_class[[5,2]],
                           "8" = rec_3_class[[6,2]])

# Adding results from predicting on test data for ORF forest
orf_predictions_test <- as.numeric(as.character(predict(train_orf, test_data)))
results_final <- results_final %>% add_row(model = "ORF", 
                              `T=0.5` = REC(orf_predictions_test, 
                                          test_data$quality, 
                                          tolerance = 0.5),
                              `T=1.0` = REC(orf_predictions_test, 
                                            test_data$quality, 
                                            tolerance = 1))

rec_4 <- REC(orf_predictions_test, test_data$quality, tolerance = 1, list = TRUE)

rec_4_class <- test_data %>% mutate(hits = rec_4) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class_final <- results_by_class_final %>% add_row(model = "ORF",
                           "3" = rec_4_class[[1,2]],
                           "4" = rec_4_class[[2,2]],
                           "5" = rec_4_class[[3,2]],
                           "6" = rec_4_class[[4,2]],
                           "7" = rec_4_class[[5,2]],
                           "8" = rec_4_class[[6,2]])

# Adding results from predicting on test data for ORF - smaller forest
orf_predictions_smaller_test <- as.numeric(as.character(predict(train_orf_smaller, test_data)))
results_final <- results_final %>% add_row(model = "ORF - smaller", 
                              `T=0.5` = REC(orf_predictions_smaller_test, 
                                          test_data$quality, 
                                          tolerance = 0.5),
                              `T=1.0` = REC(orf_predictions_smaller_test, 
                                            test_data$quality, 
                                            tolerance = 1))

rec_5 <- REC(orf_predictions_smaller_test, test_data$quality, tolerance = 1, list = TRUE)

rec_5_class <- test_data %>% mutate(hits = rec_5) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class_final <- results_by_class_final %>% add_row(model = "ORF - smaller",
                           "3" = rec_5_class[[1,2]],
                           "4" = rec_5_class[[2,2]],
                           "5" = rec_5_class[[3,2]],
                           "6" = rec_5_class[[4,2]],
                           "7" = rec_5_class[[5,2]],
                           "8" = rec_5_class[[6,2]])

# Adding results from predicting on test data for ORF - tiny forest
orf_predictions_tiny_test <- as.numeric(as.character(predict(train_orf_tiny, test_data)))
results_final <- results_final %>% add_row(model = "ORF - tiny", 
                              `T=0.5` = REC(orf_predictions_tiny_test, 
                                          test_data$quality, 
                                          tolerance = 0.5),
                              `T=1.0` = REC(orf_predictions_tiny_test, 
                                            test_data$quality, 
                                            tolerance = 1))

rec_5 <- REC(orf_predictions_tiny_test, test_data$quality, tolerance = 1, list = TRUE)

rec_5_class <- test_data %>% mutate(hits = rec_5) %>% group_by(quality) %>% summarize(rec = mean(hits))

results_by_class_final <- results_by_class_final %>% add_row(model = "ORF - tiny",
                           "3" = rec_5_class[[1,2]],
                           "4" = rec_5_class[[2,2]],
                           "5" = rec_5_class[[3,2]],
                           "6" = rec_5_class[[4,2]],
                           "7" = rec_5_class[[5,2]],
                           "8" = rec_5_class[[6,2]])

results_final %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic (tolerances 0.5 and 1.0).")
results_by_class_final %>% knitr::kable(digits = 3, caption = "Regression Error Characteristic class (tolerance 1.0)")