rm(list=ls())
library(tidymodels)
library(tidyverse)
library(haven)
data<-mtcars

#tidymodels lasso --------
# Create the split (80-20 split)
data_split = data %>% initial_split(prop = 0.8) 
data_train = data_split %>% training() 
data_test = data_split %>% testing() 

# Lasso
# Set model
lasso_model = linear_reg(
  penalty = tune(),
  mixture = 1
) %>% set_engine('glmnet')
# Set up recipe
lasso_rec = recipe(mpg ~., data = data_train) %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors())
# Set up CV
folds = vfold_cv(data_train, v = 5)
# Workflow
lasso_wf = workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso_model)
# CV
lasso_cv = lasso_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tibble(penalty = c(0, 10^seq(-3, 2, length = 20)) %>% rev()),
    metrics = metric_set(rmse,rsq,mae)
  )

lowest_rmse <- lasso_cv %>%
  select_best("rmse")

autoplot(lasso_cv) #mae and rmse similar = good

final_lasso = lasso_wf %>% finalize_workflow(select_best(lasso_cv, metric = 'rmse'))
final_lasso
# Write over 'final_fit_knn' with this last_fit() approach
final_fit_lasso = final_lasso %>% last_fit(data_split)

#####
#backup code
####-----
rm(list=ls())
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")

# libraries --------
library(car)
library(carData)
library(curl)
library(AER)
library(glmnet)
library(hdm)
library(stats)
require(graphics)
library(leaps)
library(tidyverse)
library(haven)
library(sandwich)
library(coefplot)
library(vip)
library(haven)
library(dplyr)
library(fastverse)
library(pacman)
library(visdat)
library(glmnet)
library(naniar)
p_load(
  tidyverse, modeldata, skimr, janitor,
  kknn, tidymodels, 
  magrittr
)
library(AER)
library(glmnet)
library(hdm)
library(stats)
require(graphics)
library(leaps)
library(tidyverse)
library(haven)
library(sandwich)
library(foreign)
library(plm)
set.seed(12345)
# read data --------
data <- read_dta("h_indresp.dta")
data <- zap_labels(data)
data <- as.data.frame(data)

#####
#data <- pdata.frame(data,drop.index=FALSE)
#data <- pdata.frame(data,index=c("pidp","h_istrtdaty"),drop.index=FALSE)
#options(max.print=10000)

#quantile(data$h_basrate, 0.01)
#quantile(data$h_basrate, 0.99)
data %<>% filter(h_basrate>0 & h_basrate>4.208 & h_basrate<26.952) #5115/5221
data = select(data, -2,-17:-23)
#data$h_indinub_lw
#ggplot(data, aes(x=h_basrate, colour=as.factor(h_sex)))+geom_density() +theme_classic()
data[data < 0] <- 0 

data <- data[,-(2:12)]
data = data[,!grepl("*ind",names(data))] #weighting
data = data[,!grepl("*pid",names(data))]
data = data[,!grepl("*h_casiintno",names(data))] #itrtdath + datmm
set.seed(12345)
data<-slice_sample(data, n = 50)
#vis_miss(data1)

data$male <- ifelse(data$h_sex == 1, 1, 0)
data$female <- ifelse(data$h_sex == 2, 1, 0)
#data$h_basrate <- as.numeric(unclass(data$h_basrate)) 
#data$h_basrate <- log(data$h_basrate)
#data %<>% filter(female == 1)
#n=nrow(data)
#Index <- 1:n




#reprex --------
# Create the split (80-20 split)
data_split = data %>% initial_split(prop = 0.8) 
data_train = data_split %>% training() 
data_test = data_split %>% testing() 

# Lasso
# Set model
lasso_model = linear_reg(
  penalty = tune(),
  mixture = 1
) %>% set_engine('glmnet')
# Set up recipe
lasso_rec = recipe(h_basrate ~., data = data_train) %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors())
# Set up CV
folds = vfold_cv(data_train, v = 5)
# Workflow
lasso_wf = workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso_model)
# CV
lasso_cv = lasso_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tibble(penalty = c(0, 10^seq(-3, 2, length = 20)) %>% rev()),
    metrics = metric_set(rmse,rsq,mae)
  )

lowest_rmse <- lasso_cv %>%
  select_best("rmse")

autoplot(lasso_cv) #mae and rmse similar = good

final_lasso = lasso_wf %>% finalize_workflow(select_best(lasso_cv, metric = 'rmse'))
final_lasso
# Write over 'final_fit_knn' with this last_fit() approach
final_fit_lasso = final_lasso %>% last_fit(data_split)


#reprex --------
# Create the split (80-20 split)
data_split = data %>% initial_split(prop = 0.8) 
data_train = data_split %>% training() 
data_test = data_split %>% testing() 

# Lasso
# Set model
lasso_model = linear_reg(
  penalty = tune(),
  mixture = 1
) %>% set_engine('glmnet')
# Set up recipe
lasso_rec = recipe(h_basrate ~., data = data_train) %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors())
# Set up CV
folds = vfold_cv(data_train, v = 5)
# Workflow
lasso_wf = workflow() %>% 
  add_recipe(lasso_rec) %>% 
  add_model(lasso_model)
# CV
lasso_cv = lasso_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tibble(penalty = c(0, 10^seq(-3, 2, length = 20)) %>% rev()),
    metrics = metric_set(rmse,rsq,mae)
  )

lowest_rmse <- lasso_cv %>%
  select_best("rmse")

autoplot(lasso_cv) #mae and rmse similar = good

final_lasso = lasso_wf %>% finalize_workflow(select_best(lasso_cv, metric = 'rmse'))
final_lasso
# Write over 'final_fit_knn' with this last_fit() approach
final_fit_lasso = final_lasso %>% last_fit(data_split)
