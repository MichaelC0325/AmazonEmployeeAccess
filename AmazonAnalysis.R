
# EDA ---------------------------------------------------------------------

library(tidymodels)
library(embed) #target encoding
library(vroom)

trainData <- vroom("train.csv")
testData <- vroom("test.csv")

my_recipe <- recipe(ACTION~., data=trainData) %>%
step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_nominal_predictors(), threshold = .001) %>% # combines categorical values that occur
  step_dummy(all_nominal_predictors()) %>% # dummy variable encoding
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(ACTION)) |>   #target encoding (must also step_lencode_glm() and step_lencode_bayes()
  step_lencode_glm(all_nominal_predictors(), outcome = vars(ACTION)) |> 
  step_lencode_bayes(all_nominal_predictors(), outcome = vars(ACTION))

# NOTE: some of these step functions are not appropriate to use together

# apply the recipe to your data
prep <- prep(my_recipe)
baked <- bake(prep, new_data = trainData)
ncol(baked) # I think it should be 1050

#Exploratory Plots
library(ggmosaic)
library(ggplot2)

ggplot(data= trainData) + geom_mosaic(aes(x=product(ROLE_TITLE, RESOURCE), fill=ACTION))

ggplot(trainData, aes(x = ROLE_TITLE, y = ACTION, fill = ROLE_TITLE)) +
  geom_boxplot()

