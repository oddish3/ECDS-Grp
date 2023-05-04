####-----
rm(list=ls())
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")

# libraries --------
library(xtable)
library(labelled)
library(curl)
library(AER)
library(glmnet)
library(hdm)
library(stats)
library(leaps)
library(haven)
library(sandwich)
library(coefplot)
library(dplyr)
library(fastverse)
library(pacman)
p_load(
  tidyverse, modeldata, skimr, janitor,
  kknn, tidymodels, 
  magrittr
)
library(naniar)
library(foreign)
library(plm)
set.seed(12345)
##### read data --------
#uncomment if first time
#data <- read_dta("h_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "data.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/data.Rda")
#load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/mean_by_pidp.rda")
#data %<>% select(-h_basrate) %>% left_join(mean_by_pidp, by = "pidp")
#####
data %<>% filter(h_basrate>0 &h_dvage>0  & h_ivfio == 1 & h_ioutcome == 11) #4414/39k # &h_hiqual_dv>0& h_jbstat>0& h_jbnssec8_dv>0 & h_scsf1>0 & h_jbsize>0 &h_tujbpl>0&h_marstat_dv>0
data %<>% select(-starts_with(c("h_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("h_ind")))
data <- data[,-(2:12)]

# Identify variables with more than 50% missing values
data[data < 0] <- NA
missing_prop <- colMeans(is.na(data))
vars_to_remove <- names(missing_prop[missing_prop > 0.5])
# Remove variables with more than 50% missing values
data <- data[, !names(data) %in% vars_to_remove]

#code everything as categorical
missing_prop <- colMeans(is.na(data))
vars_to_remove <- names(missing_prop[missing_prop >0.4])
data <- data %>%
  mutate(across(one_of(vars_to_remove), as.factor))

for (var in vars_to_remove) {
  data[[var]] <- factor(data[[var]], levels = c(unique(data[[var]])))
  data[[var]] <- addNA(data[[var]])
  data[[var]] <- fct_na_value_to_level(data[[var]], "0")
}

rm(var)

data[is.na(data)] <- 0

quantile(data$h_basrate, 0.01)
quantile(data$h_basrate, 0.99)
#h_basrate>4.8 & h_basrate<27.4296

#exclude_cols <- c("h_dvage", "h_basrate", "h_birthy", "h_ioutcome")
# Convert all numeric columns to factors, except for excluded columns
#data[, sapply(data, is.numeric) & !(names(data) %in% exclude_cols)] <- 
#lapply(data[, sapply(data, is.numeric) & !(names(data) %in% exclude_cols)], factor)

#dummies
data$h_dvage2 <- data$h_dvage^2
data$h_hiqual_dv = ifelse(data$h_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$h_jbstat = ifelse(data$h_jbstat==2, 1,0) #paid - ft/pt emp
#data$jbterm1 = ifelse(data$jbterm1==1, 1,0) #perm
data$h_jbsizes = ifelse(data$h_jbsize==1&2&3&4, 1,0) #small
data$h_jbsizem = ifelse(data$h_jbsize==5&6&7, 1,0) #med
data$h_jbsizel = ifelse(data$h_jbsize==8&9, 1,0) #large
data$h_tujbpl = ifelse(data$h_tujbpl==1, 1,0) #union
data$h_marstat_dv = ifelse(data$h_marstat_dv==2, 1,0) #married
data <- mutate(data,
               h_jbnssec8_dvman = case_when( 
                 h_jbnssec8_dv == 1 ~ 1,
                 TRUE ~ 0
               ),
               h_jbnssec8_dvint = case_when(
                 h_jbnssec8_dv == 2 ~ 1,
                 TRUE ~ 0
               ),
               h_jbnssec8_dvsma = case_when(
                 h_jbnssec8_dv == 3 ~ 1,
                 TRUE ~ 0
               ),
               h_jbnssec8_dvlow = case_when(
                 h_jbnssec8_dv == 4 ~ 1,
                 TRUE ~ 0
               ),
               h_jbnssec8_dvrou = case_when(
                 h_jbnssec8_dv == 5 ~ 1,
                 TRUE ~ 0
               )
)
data$female <- ifelse(data$h_sex == 2, 1, 0)
data$h_basrate <- log(data$h_basrate)
data = select(data, -1,-17:-23, )
data %<>% select(-(c("h_jbnssec8_dv", "h_sex", "h_chkresp4","h_sex_dv")))
data %<>% select(-contains(c("h_jbnssec3","h_jbnssec5", "jbsoc00_cc")))
ggplot(data, aes(x=h_basrate, colour=as.factor(female)))+geom_density() +theme_classic()
set.seed(12345)
con = model.matrix(~-1 + female*(h_dvage + h_dvage2 + h_nchild_dv + h_hiqual_dv + h_jbstat + 
                                           h_jbsizes + h_jbsizel + h_tujbpl + h_marstat_dv + 
                                           h_jbnssec8_dvman + h_jbnssec8_dvint + h_jbnssec8_dvsma + h_jbnssec8_dvlow) + 
                   (h_dvage + h_dvage2 + h_nchild_dv + h_hiqual_dv + h_jbstat + 
                       h_jbsizes + h_jbsizel + h_tujbpl + h_marstat_dv + 
                       h_jbnssec8_dvman + h_jbnssec8_dvint + h_jbnssec8_dvsma + h_jbnssec8_dvlow)^2, data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
#table ----
Z <- data %>% select(c("h_basrate","female", "h_dvage","h_dvage2","h_nchild_dv","h_hiqual_dv","h_jbstat",
                                      "h_jbsizes","h_jbsizel","h_tujbpl","h_marstat_dv","h_jbnssec8_dvman",
                                      "h_jbnssec8_dvint", "h_jbnssec8_dvsma", "h_jbnssec8_dvlow"))
ZMale <- Z %>% filter(female == 0)
ZFemale<- Z %>% filter(female == 1)

Z %>% split(. $female) %>% walk(~ stargazer(., type = "latex"))
# linear regression --------

OLS1<-lm(h_basrate ~female, data)
summary(OLS1)

# run OLS regression with somecontrols
OLS3<-lm(h_basrate ~ con, data)
summary(OLS3)

#conventional controls
OLS2<-lm(h_basrate ~ female*(h_dvage + h_dvage2 + h_nchild_dv + h_hiqual_dv + h_jbstat + 
                               h_jbsizes + h_jbsizel + h_tujbpl + h_marstat_dv + 
                               h_jbnssec8_dvman + h_jbnssec8_dvint + h_jbnssec8_dvsma + h_jbnssec8_dvlow), data)
summary(OLS2)


#hdm lasso ----- 
#restricted female 
#partial int

con = model.matrix(~-1 + female + female : (h_dvage + h_dvage2 + h_nchild_dv + h_hiqual_dv + h_jbstat + 
          h_jbsizes + h_jbsizel + h_tujbpl + h_marstat_dv + 
          h_jbnssec8_dvman + h_jbnssec8_dvint + h_jbnssec8_dvsma + h_jbnssec8_dvlow),
           data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$h_basrate
d <- data$female

one<- lm(y ~ con, data)
summary(one)

effects.female <- rlassoEffects(x = con, y = y, index = index.female)
summary(effects.female)


ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)


#full int
con = model.matrix(~-1 + female*(h_dvage + h_dvage2 + h_nchild_dv + h_hiqual_dv + h_jbstat + 
                                   h_jbsizes + h_jbsizel + h_tujbpl + h_marstat_dv + 
                                   h_jbnssec8_dvman + h_jbnssec8_dvint + h_jbnssec8_dvsma + h_jbnssec8_dvlow) 
                     , data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(con)
index.gender <- grep("female", colnames(con))
y <- data$h_basrate
effects.female <- rlassoEffects(x = con, y = y, index = index.gender)
summary(effects.female)
one<- lm(h_basrate ~ con, data)
summary(one)
x=con[,-1] # covariates

y <- data$h_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)

##########

plot(effects.female, oldNames = TRUE, joint=TRUE, level=0.95)
joint.CI <- confint(effects.female, level = 0.95, joint = TRUE, las = 2)
joint.CI


### unrestricted
con = model.matrix(~-1 + female+ (.), data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$h_basrate
d <- data$female

one<- lm(y ~ con, data)
summary(one)

effects.female <- rlassoEffects(x = con, y = y, index = index.female)
summary(effects.female)


ylasso1 <- rlassoEffect(x=x,y=y,d=d,index = index.female, method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,index = index.female,method="double selection")
summary(ylasso)





# PCA ------
library(stats)
n=nrow(data)
Index <- 1:n
data %<>% relocate(h_basrate)
data=data[ , which(apply(data, 2, var) != 0)] #remove zv col
y=as.matrix(data[,1]) #wage
x=as.matrix(data[,2:564]) #confounders
d=as.matrix(data[,566]) #female

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
nfact=583
fact_coef = matrix(0,nrow=nfact,ncol=1)
fact_se   = matrix(0,nrow=nfact,ncol=1)
fact_aic  = matrix(0,nrow=nfact,ncol=1)
fact_aicc = matrix(0,nrow=nfact,ncol=1)

for (m in (1:nfact))
{
  
  ry=resid(lm(y ~ xfactconf[,1:m]))
  rd=resid(lm(d ~ xfactconf[,1:m]))
  
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
 
#logit regression ----
#female 
#summary(data$h_basrate[data$female==1]) # median 2.054
#data$amm <- ifelse(data$female == 1 & data$h_basrate >=2.054,1,0) #females above median
#data1 <- data %>% filter(female==1)
#mylogit <- glm(amm ~ h_dvage +I(h_dvage^2) + h_nchild_dv + h_hiqual_dv + h_jbstat + 
#                 h_jbsizes+h_jbsizem + h_jbsizel + h_tujbpl + h_marstat_dv, 
#               data = data1, family = "binomial")
#summary(mylogit)
#male 
#summary(data$h_basrate[data$male==1]) # median 2.054
#data$amm <- ifelse(data$male == 1 & data$h_basrate >= 2.134,1,0) #males above median
#data1 <- data %>% filter(male==1)
#mylogit <- glm(amm ~ h_dvage +I(h_dvage^2) + h_nchild_dv + h_hiqual_dv + h_jbstat + 
#                 h_jbsizes+h_jbsizem + h_jbsizel + h_tujbpl + h_marstat_dv, 
#               data = data1, family = "binomial")
#summary(mylogit)




