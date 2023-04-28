rm(list=ls())
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")
library(haven)
#data <- read_dta("h_indresp.dta")
#data <- zap_labels(data)
#save(data, file = "data.Rdata")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/data.Rdata")
#data <- as.data.frame(data)
library(pacman)
p_load(tidyverse, caret,magrittr,labelled)
data %<>% filter(h_basrate>0)

data$h_hiqual_dv = ifelse(data$h_hiqual_dv==4|5|9, 1,0) #gcse or lower
data$h_jbstat = ifelse(data$h_jbstat==2, 1,0) #paid - ft/pt emp
#data$jbterm1 = ifelse(data$jbterm1==1, 1,0) #perm
data$h_jbsizes = ifelse(data$h_jbsize==1|2|3|4, 1,0) #small
data$h_jbsizem = ifelse(data$h_jbsize==5|6|7, 1,0) #med
data$h_jbsizel = ifelse(data$h_jbsize==8|9, 1,0) #large
data$h_tujbpl = ifelse(data$h_tujbpl==1, 1,0) #union
data$h_marstat_dv = ifelse(data$h_marstat_dv==2, 1,0) #married


+ h_scsf1

#classification --------
# Logistic regression --------------------------------------------------------------------
#is woman in bottom 25% of wage distribution 

# Set model
data_split$h_sex <- as.numeric(unclass(data_split$h_sex)) 
data_split$h_sex <- factor(data_split$h_sex)

#data_split$h_sex %>%  dplyr::filter(h_sex>0)
logistic_model = logistic_reg(
  mode = 'classification'
) %>% set_engine('glm')
# Set up recipe
logistic_rec = recipe(h_sex ~ ., data_split = data_split) %>%
  update_role(1,14, new_role = 'id variable') %>% 
  step_zv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_lincomb(all_predictors())
# Set up CV
set.seed(12345)
folds = vfold_cv(data_split, v = 5)
# Workflow
logistic_wf = workflow() %>% 
  add_recipe(logistic_rec) %>% 
  add_model(logistic_model)
# CV
logistic_cv = logistic_wf %>%
  fit_resamples(
    resamples = folds,
    metrics = metric_set(accuracy, roc_auc, sens, spec, precision)
  )
# Find estimated model performance
logistic_cv %>% collect_metrics()


# Logistic lasso -------------------------------------------------------------------------
# Set model
ll_model = logistic_reg(
  mode = 'classification',
  # mixture = tune(),
  penalty = tune()
) %>% set_engine('glmnet')
# Set up recipe
logistic_rec = recipe(h_sex ~ ., data = data) %>%
  update_role(1,13, new_role = 'id variable') %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_lincomb(all_predictors())
# Set up CV
set.seed(12345)
folds = vfold_cv(data, v = 5)
# Workflow
ll_wf = workflow() %>% 
  add_recipe(ll_rec) %>% 
  add_model(ll_model)
# CV
ll_cv = ll_wf %>% tune_grid(
  resamples = folds,
  metrics = metric_set(accuracy, roc_auc, sens, spec, precision),
  grid = grid_latin_hypercube(penalty(), size = 5),
  # grid = grid_latin_hypercube(penalty() , mixture(), size = 20),
  control = control_grid(parallel_over = 'resamples')
)
# Find best models
ll_cv %>% collect_metrics() %>% arrange(mean)



#tidymodels ridge --------

#tidymodels elasticnet ---------
# Our range of λ and α
lambdas = 10^seq(from = 5, to = -2, length = 1e2)
alphas = seq(from = 0, to = 1, by = 0.1)
# Define the 5-fold split
set.seed(12345)
data_cv = data_train %>% vfold_cv(v = 5)
# Define the elasticnet model
model_net = linear_reg(
  penalty = tune(), mixture = 0.5
) %>% set_engine("glmnet")
# Define our workflow
workflow_net = workflow() %>%
  add_model(model_net) %>% add_recipe(ridge_rec)
# CV elasticnet with our range of lambdas
data_net = 
  workflow_net %>%
  tune_grid(
    data_cv,
    grid = grid_regular(penalty(), levels = 100),
    metrics = metric_set(rmse)
  )

autoplot(data_net) #mae and rmse similar = good
data_net %>% collect_metrics()
lowest_rmse <- data_net %>%
  select_best("rmse")
final_net = workflow_net %>% finalize_workflow(select_best(data_net, metric = 'rmse'))
final_net

# Write over 'final_fit_knn' with this last_fit() approach
final_fit_net = final_net %>% last_fit(data_split)
# Collect metrics on the test data!
library(vip)
final_fit_net %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  )  %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

assign("net coef", final_fit_net %>% extract_fit_parsnip() %>% tidy())
`net coef` %<>% filter(!estimate==0)

final_fit_net %>% collect_metrics()
final_fit_net %>% collect_predictions() %>% head()
final_fit_net %>% extract_fit_parsnip() %>% tidy()























