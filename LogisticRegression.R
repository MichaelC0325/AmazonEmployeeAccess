library(tidymodels)
library(embed) #target encoding
library(vroom)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")

trainData$ACTION <- as.factor(trainData$ACTION)


my_recipe <- recipe(ACTION~., data=trainData) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_nominal_predictors(), threshold = .01) %>% # combines categorical values that occur
  step_dummy(all_nominal_predictors()) %>% # dummy variable encoding
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) |>   #target encoding (must also step_lencode_glm() and step_lencode_bayes()
  step_lencode_glm(all_nominal_predictors(), outcome = vars(ACTION)) |> 
  step_lencode_bayes(all_nominal_predictors(), outcome = vars(ACTION))


logRegModel <- logistic_reg() %>% #Type of model
  set_engine("glm")

## Put into a workflow here

log_wf <- workflow() |> 
  add_recipe(my_recipe) |> 
  add_model(logRegModel) |> 
  fit(data = trainData)

## Make predictions
amazon_predictions <- predict(log_wf,
                              new_data=testData,
                              type= "prob") # "class" or "prob" (see doc)

amazon_predictions <- amazon_predictions |> 
  mutate(ACTION = .pred_0) |> 
  select(ACTION)


## Format the Predictions for Submission to Kaggle
kaggle_submission <- amazon_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data
  select(id, ACTION)
## Write out the file
vroom_write(x=kaggle_submission, file="./logreg.csv", delim=",")
