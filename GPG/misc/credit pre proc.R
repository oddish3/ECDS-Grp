rm(list=ls())
library(pacman)
library(visdat)
p_load(
  tidyverse, modeldata, skimr, janitor,
  kknn, tidymodels, 
  magrittr
)
data(credit_data)
credit_df = credit_data
credit_df %>% glimpse()
# 'Fix' names
credit_df %<>% clean_names()
credit_df %>% skim()

#engineering - dummy for good credit status 
credit_df %<>% mutate(status_good = 1 * (status == "good"))
# Drop the old status variable
credit_df %<>% select(-status)
# Skim result
credit_df %>% skim()

#engineering w tidymodels 
#defining a recipe
recipe_all = recipe(status_good ~ ., data = credit_df)
# What is this 'recipe' thing?
recipe_all
recipe_all %>% str()

#clean/preprocess/engineer
# Mean imputation for all numeric predictors - no more missing values for numeric pred
credit_clean = recipe_all %>% step_impute_mean(all_predictors() & all_numeric()) %>% 
  prep() %>% juice()
credit_clean %>% skim()

# Remembering what 'recipe_all' is
recipe_all = recipe(status_good ~ ., data = credit_df)
# Putting it all together
credit_clean = recipe_all %>% 
  # Mean imputation for numeric predictors
  step_impute_mean(all_predictors() & all_numeric()) %>% 
  # KNN imputation for categorical predictors
  step_impute_knn(all_predictors() & all_nominal(), neighbors = 5) %>%
  # Create dummies for categorical variables
  step_dummy(all_predictors() & all_nominal()) %>%
  # Interactions
  step_interact(~income:starts_with("home")) %>% 
  # Prep and juice!
  prep() %>% juice()
# Skim the final result (it's a data.frame)
credit_clean %>% skim()

credit_clean %>% visdat::vis_dat()
