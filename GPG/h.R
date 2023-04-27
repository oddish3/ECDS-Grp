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
data %<>% filter(h_basrate>0 & h_basrate>4.4242 & h_basrate<26.9664 &h_dvage>0  &h_hiqual_dv>0& h_jbstat>0&
                   h_jbnssec8_dv>0 & h_scsf1>0)

data = select(data, -2,-17:-23)


#ggplot(data, aes(x=h_basrate, colour=as.factor(h_sex)))+geom_density() +theme_classic()
data[data<0] <- NA
not_all_na <- function(x) any(!is.na(x))
data %<>% select(where(not_all_na))
data[is.na(data)] <- 0
#data1<-slice_sample(data, n = 100)
#vis_miss(data1, sort = TRUE)
#gg_miss_var(data)
data <- data[,-(2:12)]
data = data[,!grepl("*ind",names(data))] #weighting
data = data[,!grepl("*pid",names(data))]
data = data[,!grepl("*h_casiintno",names(data))] #itrtdath + datmm
set.seed(12345)


data$male <- ifelse(data$h_sex == 1, 1, 0)
data$female <- ifelse(data$h_sex == 2, 1, 0)
data$h_basrate <- log(data$h_basrate)
#data %<>% filter(female == 1)
#n=nrow(data)
#Index <- 1:n

# linear regression --------
Index <- 1:nrow(data)
plot(Index, data$h_basrate, xlab = "Index", ylab = "hourly wage") # plotting distribution of wage by index number

# (a)  run simple linear regression of wage on gradcoll, and then for all controls #
OLS1<-lm(h_basrate ~female , data)
summary(OLS1)

# run multiple linear regression on all controls 
OLS2<-lm(log(h_basrate) ~ female*(h_dvage +I(h_dvage^2) + h_nchild_dv + h_hiqual_dv + h_jbstat +
                                 h_jbnssec8_dv+ h_scsf1),data)
summary(OLS2)



linearHypothesis(OLS2, "female = 0") #perfect multicolinearity in model 
#f stat 0.71. do not reject joint significance. null hypoth that all =0

OLS_pretest<-lm(h_basrate ~ female +., olsall)
summary(OLS_pretest)

#hdm lasso --------
library(hdm)
data %<>% relocate(h_basrate)
y=as.matrix(data[,1])
x=as.matrix(data[,2:2092])
rlasso <- rlasso(x,y, relax = TRUE)
summary(ylasso, all = FALSE)
ylasso <- rlasso(y=y,x=x,index=c(2145),method="double selection",
                 I3=NULL,post=TRUE)
summary(ylasso, all = FALSE)
 
#tidymodels lasso --------
# Create the split (80-20 split)
data_split = data %>% initial_split(prop = 0.8) 
data_train = data_split %>% training() %>% as.data.frame()
data_test = data_split %>% testing() %>% as.data.frame()
data_train <- as.data.frame(lapply(data_train, as.numeric))
#data_train$female <- factor(data_train$female)
# Lasso
# Set model
lasso_model = linear_reg(
  penalty = tune(),
  mixture = 1
) %>% set_engine('glmnet')
# Set up recipe
lasso_rec = recipe(h_basrate ~female+., data = data_train) %>%
  update_role(1, new_role = 'id variable') %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_interact(~ h_dvage + h_nchild_dv + h_hiqual_dv + h_jbstat +
                  h_jbnssec8_dv+ h_scsf1 : all_numeric_predictors()) %>% 
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

final_fit_lasso = final_lasso %>% last_fit(data_split)

# Collect metrics on the test data!
final_fit_lasso %>% collect_metrics()

final_fit_lasso %>% 
  extract_fit_parsnip() %>% 
  vip()

assign("lasso coef", final_fit_lasso %>% extract_fit_parsnip() %>% tidy())
`lasso coef` %<>% filter(!estimate==0)

final_fit_lasso %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  )  %>% filter(Importance>0.00005) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

# Find best models
lasso_cv %>% collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")


#tidymodels ridge --------
# Create the split (80-20 split)

data_split = data %>% initial_split(prop = 0.8)
data_train = data_split %>% training()
data_test = data_split %>% testing()
#data_train$female <- factor(data_train$female)
# ridge
# Set model
ridge_model = linear_reg(
  penalty = tune(),
  mixture = 0
) %>% set_engine('glmnet')

# Set up recipe
ridge_rec = recipe(h_basrate ~., data = data_train) %>%
  update_role(1, new_role = 'id variable') %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_interact(~ female : all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors())
# Set up CV
set.seed(12345)
folds = vfold_cv(data_train, v = 5)
# Workflow
ridge_wf = workflow() %>% 
  add_recipe(ridge_rec) %>% 
  add_model(ridge_model)
# CV
ridge_cv = ridge_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tibble(penalty = c(0, 10^seq(-3, 2, length = 20)) %>% rev()),
    metrics = metric_set(rmse,rsq,mae)
  )
# Find best models
#ridge_cv %>% collect_metrics(.metric = 'rmse') %>% arrange(mean)
#ridge_cv %>% collect_metrics()
#ridge_cv %>% collect_metrics(summarize = F)
#ridge_cv %>% show_best(metric = 'rmse', n = 3)
#ridge_cv %>% show_best(metric = 'rsq', n = 3)
autoplot(ridge_cv) #mae and rmse similar = good
ridge_cv %>% collect_metrics()

final_ridge = ridge_wf %>% finalize_workflow(select_best(ridge_cv, metric = 'rmse'))
final_ridge
lowest_rmse <- ridge_cv %>%
  select_best("rmse")
# Write over 'final_fit_knn' with this last_fit() approach
final_fit_ridge = final_ridge %>% last_fit(data_split)
# Collect metrics on the test data!
final_fit_ridge %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  )  %>% filter(Importance>0.01) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

assign("ridge coef", final_fit_ridge %>% extract_fit_parsnip() %>% tidy())
`ridge coef` %<>% filter(!estimate==0)

final_fit_ridge %>% collect_metrics()
final_fit_ridge %>% collect_predictions() %>% head()
final_fit_ridge %>% extract_fit_parsnip() %>% tidy()

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

#PCA -------
#tidymodels 


#length(which(apply(data, 2, var)==0)) = 578
# PCA ------
library(stats)

data %<>% relocate(h_basrate)
data=data[ , which(apply(data, 2, var) != 0)] #remove zv col
y=as.matrix(data[,1]) #wage
x=as.matrix(data[,2:1514])
d=as.matrix(data[,1516]) #female

xpc_conf=prcomp(x,center = TRUE, scale. = TRUE)
xfactconf=xpc_conf$x

names(xpc_conf)
summary(xpc_conf)
xpc_conf$rotation[,1:3]

# print eigenvalues lambda, which are also the standard dev. of factors
std_dev <- xpc_conf$sdev
xpc_var <- std_dev^2
#proportion of the variance explained
xpc_varexpl <- xpc_var/sum(xpc_var)
xpc_varexpl[1:2]

# plot of the first two principal components with their loadings
dev.off()
par(mar = c(1, 1, 1, 1))
biplot(xpc_conf,scale=0)

par(mfrow=c(1,1))
screeplot(xpc_conf,npcs=length(xpc_conf$sdev),type="l")

plot(xpc_varexpl,xlab="Principal Component",ylab="Proportion of Variance Explained", 
     type="b")

summary(lm(y~d+x))
nfact=1516
fact_coef = matrix(0,nrow=nfact,ncol=1)
fact_se   = matrix(0,nrow=nfact,ncol=1)
fact_aic  = matrix(0,nrow=nfact,ncol=1)
fact_aicc = matrix(0,nrow=nfact,ncol=1)

for (m in (1:nfact))
{
  
  ry=resid(lm(y ~ xfactconf[,1:m]))
  rd=resid(lm(GRADCOLL ~ xfactconf[,1:m]))
  
  #  or equivalently
  #  OLS_fact <- lm(WAGE~GRADCOLL + xfactconf[,1:m])
  #  fact_coef[m,1]=OLS_fact$coefficients[2]
  #  fact_se[m,1]= sqrt(vcov(OLS_fact)[2,2])
  
  OLS_fact <- lm(ry~ -1 +rd)
  e=OLS_fact$residuals
  ssr=(t(e)%*%e)/(n-m-2)
  fact_aic[m,1]=n*(log(ssr)+1)+2*(m+1)
  fact_aicc[m,1]=n*log(ssr)+n*(1+m/n)/(1-(m+2)/n)
  
  fact_coef[m,1]=OLS_fact$coefficients[1]
  fact_se[m,1]= sqrt(vcov(OLS_fact)[1,1])
  
}

numfact=which.min(fact_aicc[,1])
print(numfact)

factprint=cbind(fact_coef,fact_se,fact_aic,fact_aicc)
factprint


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


























