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
# read data -------- uncomment if first time
#data <- read_dta("h_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "data.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/data.Rda")

#####
datab  <- read_dta("g_indresp.dta")
before<-datab[,c(1,1713)] %>% filter(g_basrate>0)
dataa <- read_dta("i_indresp.dta")
after<-dataa[,c(1,1675)] %>% filter(i_basrate>0) 
pres<-data[,c(1,626)] %>% filter(h_basrate>0)
colnames(before) <- c("pidp", "h_basrate")
colnames(pres) <- c("pidp", "h_basrate")
colnames(after) <- c("pidp", "h_basrate")
avg <- rbind(before, pres, after)
mean_by_pidp <- aggregate(h_basrate ~  pidp, data = avg, mean)
data <- merge(data, mean_by_pidp, by = "pidp", all.x = TRUE)
data$h_basrate <- ifelse(!is.na(data$h_basrate.y), data$h_basrate.y, data$h_basrate)

#save(data, file = "data.Rda")
#load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/data.Rda")

quantile(data$h_fimnlabnet_dv, 0.01)
quantile(data$h_fimnlabnet_dv, 0.99)
data %<>% filter(h_fimnlabnet_dv>0 & h_fimnlabnet_dv>50.0304 & h_fimnlabnet_dv<5400 &h_dvage>0  &h_hiqual_dv>0& h_jbstat>0&
                   h_jbnssec8_dv>0 & h_scsf1>0 & h_jbsize>0 &h_tujbpl>0&h_marstat_dv>0) #4414/39k
#dummies
data$h_hiqual_dv = ifelse(data$h_hiqual_dv==4&5&9, 1,0) #gcse or lower
data$h_jbstat = ifelse(data$h_jbstat==2, 1,0) #paid - ft/pt emp
#data$jbterm1 = ifelse(data$jbterm1==1, 1,0) #perm
data$h_jbsizes = ifelse(data$h_jbsize==1&2&3&4, 1,0) #small
data$h_jbsizem = ifelse(data$h_jbsize==5&6&7, 1,0) #med
data$h_jbsizel = ifelse(data$h_jbsize==8&9, 1,0) #large
data$h_tujbpl = ifelse(data$h_tujbpl==1, 1,0) #union
data$h_marstat_dv = ifelse(data$h_marstat_dv==2, 1,0) #married
data$male <- ifelse(data$h_sex == 1, 1, 0)
data$female <- ifelse(data$h_sex == 2, 1, 0)
data$h_fimnlabnet_dv <- log(data$h_fimnlabnet_dv)
data = select(data, -2,-17:-23)

ggplot(data, aes(x=h_fimnlabnet_dv, colour=as.factor(h_sex)))+geom_density() +theme_classic()
data[data<0] <- 0
#not_all_na <- function(x) any(!is.na(x))
#data %<>% select(where(not_all_na))
#data[is.na(data)] <- 0

data <- data[,-(2:12)]
data = data[,!grepl("*ind",names(data))] #weighting
data = data[,!grepl("*pid",names(data))]
data = data[,!grepl("*h_casiintno",names(data))] #itrtdath + datmm
set.seed(12345)

#logit regression ----
#female 
summary(data$h_fimnlabnet_dv[data$female==1]) # median 2.054
data$amm <- ifelse(data$female == 1 & data$h_fimnlabnet_dv >=7.107,1,0) #females above median
data1 <- data %>% filter(female==1)
mylogit <- glm(amm ~ h_dvage +I(h_dvage^2) + h_nchild_dv + h_hiqual_dv + h_jbstat + 
                 h_jbsizes+h_jbsizem + h_jbsizel + h_tujbpl + h_marstat_dv, 
               data = data1, family = "binomial")
summary(mylogit)
#male 
summary(data$h_fimnlabnet_dv[data$male==1]) # median 2.054
data$amm <- ifelse(data$male == 1 & data$h_fimnlabnet_dv >= 7.443,1,0) #females above median
data1 <- data %>% filter(male==1)
mylogit <- glm(amm ~ h_dvage +I(h_dvage^2) + h_nchild_dv + h_hiqual_dv + h_jbstat + 
                 h_jbsizes+h_jbsizem + h_jbsizel + h_tujbpl + h_marstat_dv, 
               data = data1, family = "binomial")
summary(mylogit)

# linear regression --------
#  run simple linear regression of wage on gradcoll, and then for all controls #
OLS1<-lm(h_fimnlabnet_dv ~female , data)
summary(OLS1)

# run multiple linear regression on all controls 
OLS2<-lm(h_fimnlabnet_dv ~ female*(h_dvage +I(h_dvage^2) + h_nchild_dv + h_hiqual_dv + h_jbstat + 
                               h_jbsizes+h_jbsizem + h_jbsizel + h_tujbpl + h_marstat_dv), data)
summary(OLS2)

#naive OLS regression to predict wage gap
#OLS3<-lm(h_fimnlabnet_dv ~ female*., data) #negative insignificant
#summary(OLS3)

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
lasso_rec = recipe(h_fimnlabnet_dv ~female+., data = data_train) %>%
  update_role(1:2, new_role = 'id variable') %>% 
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_poly(h_dvage) %>% 
  step_interact(~female:all_numeric_predictors()) %>% 
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
    grid = tibble(penalty = c(0, 10^seq(-3, 2, length = 20)) %>% rev())) #metrics = metric_set(rmse,rsq,mae)

lowest_rmse <- lasso_cv %>%
  select_best("rmse")

autoplot(lasso_cv) #mae and rmse similar = good

final_lasso = lasso_wf %>% finalize_workflow(select_best(lasso_cv, metric = 'rmse'))

final_fit_lasso = final_lasso %>% last_fit(data_split)

final_fit_lasso %>% 
  extract_fit_parsnip() %>% 
  vip()

assign("lasso coef", final_fit_lasso %>% extract_fit_parsnip() %>% tidy())
`lasso coef` %>% filter(term == 'female')
`lasso coef` %<>% filter(!estimate==0)

final_fit_lasso %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  )  %>% filter(Importance>0.01) %>% # & str_detect(Variable, '^female')) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)
final_fit_lasso %>% extract_fit_parsnip() %>% 
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  )  %>% filter(Importance>0.001 & str_detect(Variable, '^female')) %>% 
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

#hdm lasso --------
library(hdm)
data %<>% relocate(h_dvage, h_nchild_dv, h_hiqual_dv , h_jbstat , h_jbsizes,h_jbsizem , h_jbsizel , h_tujbpl , h_marstat_dv)
d=as.matrix(data[,9])
x=as.matrix(data[,10:2095])[,-613]
y=as.matrix(data[,613])

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)
table = rbind(summary(ylasso1)$coef[,1:2], summary(ylasso)$coef[,1:2])
tab = xtable(table, digits = c(19, 19, 5))
tab
# PCA ------
library(stats)

data %<>% relocate(h_fimnlabnet_dv)
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



