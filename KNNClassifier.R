library(tidymodels)
library(embed) # for target encoding
library(vroom)
library(kknn)

# Load data
trainData <- vroom("train.csv")
testData <- vroom("test.csv")

# Convert ACTION to a factor
trainData$ACTION <- as.factor(trainData$ACTION)

# Create recipe
my_recipe <- recipe(ACTION ~ ., data = trainData) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>%  # Convert numeric predictors to factors
  step_other(all_nominal_predictors(), threshold = 0.01) %>%  # Combine rare levels in nominal predictors
  step_dummy(all_nominal_predictors()) %>%  # Dummy encoding
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) %>%  # Target encoding (mixed)
  step_lencode_glm(all_nominal_predictors(), outcome = vars(ACTION)) %>%  # Target encoding (GLM)
  step_lencode_bayes(all_nominal_predictors(), outcome = vars(ACTION))  # Target encoding (Bayesian)

# KNN model setup (manual setting of neighbors or tuning)
knn_model <- nearest_neighbor(neighbors = tune()) %>%  # Tune the neighbors parameter
  set_mode("classification") %>%
  set_engine("kknn")

# Workflow setup
knn_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(knn_model)

# Define metrics (include roc_auc)
metrics <- metric_set(roc_auc, accuracy)

# Perform tuning with roc_auc as one of the metrics
knn_res <- tune_grid(
  knn_wf,
  resamples = vfold_cv(trainData, v = 5),
  grid = 10,  # Number of grid search values
  metrics = metrics  # Include AUC and accuracy in metrics
)

# Best model based on ROC AUC
best_knn <- select_best(knn_res, metric = "roc_auc")

# Finalize workflow with the best AUC-based model
final_knn_wf <- finalize_workflow(knn_wf, best_knn)

# Fit final model on the training data
final_knn_fit <- fit(final_knn_wf, data = trainData)

# Predictions on new data (replace myNewData with your test dataset)
predictions <- predict(final_knn_fit, new_data = testData, type = "prob")

## Format the Predictions for Submission to Kaggle
kaggle_submission <- predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(id, .pred_1) |> 
  mutate(ACTION = .pred_1) |> 
  select(-.pred_1)

## Write out the file
vroom_write(x=kaggle_submission, file="./knn.csv", delim=",")
