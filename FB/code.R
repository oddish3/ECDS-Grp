#############################

set.seed(1)
rm(list=ls())
setwd("C:/R folder/Game/dataverse_files/FB")
##########
library(dplyr)
library(labelled)
library(ggplot2)
library(haven)
library(stargazer)
library(bacondecomp)
library(tidyverse)
library(haven)
library(lfe)
library(ddpcr)
library(data.table)
library(fixest)
library(lubridate)
library(ggiplot)
##########
#alt<-read_dta('mrc_table5.dta')
data<-read_dta('mrc_table3.dta')
#look_for(data, income) tier type
data$a2004 <- 2004 - data$cohort
data1 <- data %>% dplyr::select(super_opeid, cohort,name, type, tier, tier_name, region, state,cz, czname, a2004, k_mean, par_mean, multi) %>% 
  filter(super_opeid>0 & multi == 0 & tier <12)

data2<- read.csv('IntroDatesACHA.csv')
data2$name <- data2$instnm
data3trt<-merge(data1,data2, by = "name")
data1 <- data1 %>%
  mutate(id = 0,
         fbname = as.character(NA),
         instnm = as.character(NA),
         unitid = 0,
         expansion_batch = 0,
         datejoinedfb = as.character(NA))

datamerge<-merge_dfs_overwrite_col(data1, data3trt, bycol = "name")
datamerge <- datamerge[!is.na(datamerge$k_mean),]
Index <- 1:nrow(datamerge)
plot(Index, datamerge$k_mean, xlab = "Index", ylab = "WAGE")


data4 <- datamerge %>%
  mutate(treat = ifelse(a2004== 19 & expansion_batch == 1, 1,
                         ifelse(a2004 == 19 & expansion_batch == 2, 2,
                                ifelse(a2004 == 19 & expansion_batch == 3, 3,
                                       ifelse(a2004 == 19 & expansion_batch == 4, 4,
                                              ifelse(a2004==18 & expansion_batch == 0,4, 0))))),
         control = ifelse(treat == 0, 1, 0)) %>% filter(k_mean<700000)
#ggplot(data4, aes(x=treat, y=k_mean)) + geom_point()
#ggplot(data4, aes(x=a2004, y=k_mean, color=factor(expansion_batch))) +
 # geom_point()
#prob a stat ecdf needed
Index <- 1:nrow(data4)
plot(Index, data4$k_mean, xlab = "Index", ylab = "WAGE")
### simple OLS
data5<- data4 %>% mutate(treat = ifelse(treat>=1 & a2004 %in% c('18','19'), 1,0))
attach(data5)
OLS1<- lm(k_mean ~ treat)
detach(data5)
summary(OLS1) # 1202.2
#naive_regression <- lm(k_mean ~ a2004 + treat + control + factor(expansion_batch) + I(a2004^2) + I(a2004^3) + I(a2004^4) + factor(name) + type + region + par_mean, data = data4)
#stargazer(naive_regression, type = 'text', keep = 1:9)

subsample <- data4 %>% 
  sample_n(3600, replace = FALSE) %>% filter(k_mean<700000)
#ggplot(subsample, aes(x=treat, y=k_mean)) + geom_point()
#ggplot(subsample, aes(x=a2004, y=k_mean, color=factor(expansion_batch))) +
#  geom_point()
Index <- 1:nrow(subsample)
plot(Index, subsample$k_mean, xlab = "Index", ylab = "WAGE")
#naive_regression <- lm(k_mean ~ a2004 + treat + factor(expansion_batch) + I(a2004^2) + I(a2004^3) + I(a2004^4) + factor(name) + type + region + par_mean + super_opeid + id + cohort, data = subsample)
#stargazer(naive_regression, type = 'text', keep = 1:9)

#########
#attempt at event study
##define pre treatment leads and lags
####
treatment.period <- 18
treatment.definition <- 1:4

###time to treatment variable 
# create a new variable with the difference between a2004 and 18 for treat==1
subsample$diff <- ifelse(subsample$treat >= 1 | subsample$control <=1, subsample$a2004 - 18, 0)



mod_twfe = feols(k_mean ~ i(diff + treat, ref = 0) + ## Our key interaction: time × treatment status
                     cohort + super_opeid + tier + type + a2004 + region + state + cz + par_mean + unitid,                             ## FEs
                 cluster = ~super_opeid,                          ## Clustered SEs
                 data = subsample)
summary(mod_twfe)
iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (2WFE)')

mod_twfe = feols(k_mean ~ i(diff, treat, ref = 0)  ## Our key interaction: time × treatment status
                 + a2004 + type + region + par_mean |          ## Other controls
                  super_opeid + cohort,                             ## FEs
                 cluster = ~super_opeid,                          ## Clustered SEs
                  data = subsample)
summary(mod_twfe)

#library(corrplot)
# Calculate the correlation matrix for the variables in the model
#cor_matrix <- cor(subsample[c("k_mean","diff","a2004","super_opeid",)])

# Visualize the correlation matrix using corrplot
#corrplot(cor_matrix, method = "circle", type = "lower",
        # tl.col = "black", tl.srt = 45, tl.cex = 0.8, tl.offset = 0.5)



est_sa20_grp = feols(k_mean ~ sunab(diff, treat, 0) + ## Our key interaction: time × treatment status
                   a2004 + type + region + par_mean |          ## Other controls
                   super_opeid + cohort ,                             ## FEs
                 cluster = ~super_opeid,                          ## Clustered SEs
                 data = subsample)
summary(est_sa20_grp)
iplot(est_sa20_grp)



ggiplot(
  list("TWFE" = mod_twfe, "Sun & Abraham (2020)" = est_sa20_grp),
  ref.line = -1,
  main = "Staggered treatment: Split mutli-sample",
  xlab = "Time to treatment",
  multi_style = "facet",
  geom_style = "ribbon",
  facet_args = list(labeller = labeller(id = \(x) gsub(".*: ", "", x))),
  theme = theme_minimal() +
    theme(
      text = element_text(family = "HersheySans"),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
)
ggiplot(list('TWFE' = mod_twfe, 'Sun & Abraham (2020)' = est_sa20_grp),
        main = 'Staggered treatment', ref.line = -1, pt.join = TRUE, theme = theme_minimal()) +
  scale_x_continuous(breaks = seq(-5, 5, by = 1))  # Set breaks from -1 to 3, in increments of 0.5


######
#matching

library(Matching)
library(here)
library(ebal)
library(xtable)
library(readr)
######
subsample1 <- subsample %>% mutate(treat = ifelse(treat>=1 & a2004 %in% c('18','19'), 1,0))

#estimate ATE as diff in means

ate = mean(subsample1$k_mean[subsample1$treat==1]) - mean(subsample1$k_mean[subsample1$treat==0])
ate.se = sqrt(var(subsample1$k_mean[subsample1$treat==1])/length(subsample1$k_mean[subsample1$treat==1]) +
                var(subsample1$k_mean[subsample1$treat==0])/length(subsample1$k_mean[subsample1$treat==0]))
# ate = 43212 (1496)
# Check balance in unmatched data
bal.formula = formula(treat ~ cohort  
                  + type + tier + region + cz + par_mean,
                  family = binomial(link="logit"))

# Print a balance table
mb.unmatched = MatchBalance(bal.formula, data = subsample1)
tab.unmatched = baltest.collect(mb.unmatched,
                                var.names = colnames(subsample1)[-1], after=F) #just need to mess wi col names a bit in subsample in og -1 = all 16 variables they used

#naive_regression <- lm(k_mean ~ a2004 + treat + factor(expansion_batch) +
                     #    I(a2004^2) + I(a2004^3) + I(a2004^4) + factor(name) + type + region + par_mean + super_opeid + id + cohort, data = subsample)
#stargazer(naive_regression, type = 'text', keep = 1:9)    2551 causal impact 

# Match on a set of covariates
vars = c('cohort','type', 'tier', 'region', 'cz' ,'par_mean')
# Function defaults will estimate the ATT with M=1 and this is mahaloobis distance too
m.first = Match(Y=subsample1$k_mean, Tr=subsample1$treat, X = subsample1[, vars], Weight=2)
# 2 denotes the Mahalanobis distance metric
summary(m.first) #15096 (3131.9)

# Check balance
mb.matched = MatchBalance(bal.formula, data=subsample1, match.out=m.first)
# ATT and its SE can be found in the est and se elements of m.first
# Print a balance table
tab.matched = baltest.collect(mb.matched, var.names = colnames(subsamplel)[-1])
print(xtable(tab.matched[, 1:7], caption='Covariate Balance in Matched Data',
             label='tab:matched-bal'), caption.placement='top')


# exact matching
m.exact = Match(Y=subsample1$k_mean, Tr=subsample1$treat, X = subsample1[,2,4,5,7,9,11,18], exact=TRUE)
summary(m.exact) #36541 (1603.6)
# Check balance
mb.exact = MatchBalance(bal.formula, data=subsample1, match.out=m.exact)
# ATT and its SE can be found in the est and se elements of m.exact
# Print the table
tab.exact = baltest.collect(mb.exact, var.names = colnames(subsample1))
print(xtable(tab.exact[, 1:7], caption='Covariate Balance in Exactly Matched Data',
             label='tab:exact-bal'), 
      caption.placement='top')
#
#one to many matching

# Match with M=2
m.m2 = Match(Y=subsample1$k_mean, Tr=subsample1$treat, X = subsample1[, vars], M=2)
summary(m.m2) ## 12303 (2705.8)

# Check balance
mb.m2 = MatchBalance(bal.formula, data=subsample1, match.out=m.m2)
# Print a balance table
tab.m2 = baltest.collect(mb.m2, var.names = colnames(csol)[-1])
print(xtable(tab.m2[, 1:7], caption=
               'Covariate Balance in Matched, M=2',
             label='tab:m2-bal'), 
      caption.placement='top')
# ATT and its SE can be found in the est and se elements of m.m2

# Match with M=10
m.m10 = Match(Y=subsample1$k_mean, Tr=subsample1$treat, X = subsample1[, vars], M=10)
summary(m.m10) #18110 (2003)

# Check balance
mb.m10 = MatchBalance(bal.formula, data=csol, match.out=m.m10)
# Print a balance table
tab.m10 = baltest.collect(mb.m10, var.names = colnames(csol)[-1])
print(xtable(tab.m10[, 1:7], caption=
               'Covariate Balance in Matched, M=10',
             label='tab:m2-bal'), 
      caption.placement='top')
# ATT and its SE can be found in the est and se elements of m.m10



###
##  propensity score matching


#estimating the probability of being in treatment group
# Run logit regression to get predictions of probability of treatment
#matching to those who are yet treated

Prob_Treat = glm(data = subsample1, treat ~ cohort  
                    + type + tier + region + cz + par_mean,
                 family = binomial(link="logit"))
subsample1$pscore = predict(Prob_Treat, type="response")
plot(density(subsample1$pscore[subsample1$treat==1]))
lines(density(subsample1$pscore[subsample1$treat==0]), lty=2)
legend(0.05,40, lty=c(1,2),legend = c("Treated", "Control"))
#########
# model
## Estimate a model 
library(broom)
ols1_train = lm(treat ~ k_mean, data = subsample1)

## intervals and options.
predict(ols1_train, newdata = subsample1, interval = "prediction") %>%
  head(5) ## Just print the first few rows
subsample1= augment(ols1_train, newdata = subsample1, interval = "prediction")
subsample1 %>% select(contains("."), everything()) %>% head()

subsample1 %>%
  ggplot(aes(x =  treat, y =k_mean, col = factor(treat), fill = factor(treat))) +
  geom_point(alpha = 0.7) +
  geom_line(aes(y = .fitted)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.3, col = NA) +
  scale_color_discrete(name = "Training sample?", aesthetics = c("colour", "fill")) +
  labs(
    title = "Predicting mass from height",
    caption = "Line of best fit, with shaded regions denoting 95% prediction interval."
  )



##########
##  one to one prop score matching
m.pscore <- Match(Y=subsample1$k_mean, Tr=subsample1$treat, X = subsample1$pscore)
summary(m.pscore) #13368 (3279.8)

#Plot the p-scores after matching
plot(density(m.pscore$mdata$X[1,]))
lines(density(m.pscore$mdata$X[2,]), lty=2)
legend(0.05,49, lty=c(1,2),legend = c("Treated", "Control"))

mb.pscore <- MatchBalance(bal.formula, data = csol, match.out = m.pscore)
# Print
tab.pscore <- baltest.collect(mb.pscore,
                              var.names = colnames(csol)[-c(1,17)])
print(xtable(tab.pscore[, 1:7], caption = "Covariate Balance
in Propensity Score Matched Data",
             label = "tab:pscore-bal"), caption.placement = "top")

#summary(Prob_Treat)
#a2004 defines treat so is perfectly multicolinear
subsample1$Prop_Score = Prob_Treat$fitted.values  # Save propensity scores
plot(hist(subsample1$Prop_Score[subsample1$treat==0]))
plot(hist(subsample1$Prop_Score[subsample1$treat==1]))

ATT1 = Match(Y = subsample1$k_mean,                  # outcome
             Tr = subsample1$treat,            # treatment assignment
             X = as.matrix(subsample1$Prop_Score), # Covariate, here ps
             ties = F,                       # If one treated obs matches more than one control. Either average (T) or randomly broken ties (F)
             estimand = "ATT")               # causal effect to be estimated
summary(ATT1) # 13308 (1346.7)
#matching puts most weight on those who are most likely to be treated
#those who are most likely to be treated are also those who happen to gain most from treatment

# Below, I run the analysis 100 times, collect the results in the matrix 'Ans'
# and then take the average of these estimates
Ans = matrix(NA, 100, 1)
for (ii in 1:100) {
  ATT = Match(Y = subsample1$k_mean,                  # outcome
               Tr = subsample1$treat,            # treatment assignment
               X = as.matrix(subsample1$Prop_Score), # Covariate, here ps
               ties = F,                       # If one treated obs matches more than one control. Either average (T) or randomly broken ties (F)
               estimand = "ATT")               # causal effect to be estimated
  summary(ATT)
  Ans[ii] = ATT$est
}
mean(Ans) #13399.52 (1349)

#can this be explained by poor matching and thus unbalanced prop scores?
MatchBalance(treat ~ Prop_Score, data = subsample1, match.out = ATT1)
#quite similar but bootstrap p value of 0.714, so maybe, but treat 0 group has v low propensity of treatment

A = as.data.frame(table(ATT1$index.control))
max(A$Freq)
#one was used 50 times!


##matching lecture file
# ATET
ATT = Match(Y = subsample1$k_mean, 
            Tr = subsample1$treat,
            X = as.matrix(cbind(subsample1$par_mean,subsample1$type, subsample1$tier)),
            ties = F,
            estimand = "ATT")
summary(ATT) #29537 (1316)
summary(lm(data = subsample1, k_mean ~ treat + par_mean + type + tier))

# ATE - discrete?
ATE = Match(Y = subsample1$k_mean, 
            Tr = subsample1$treat,
            X = as.matrix(cbind(subsample1$par_mean,subsample1$type, subsample1$tier)),
            ties = F,
            estimand = "ATE")
summary(ATE) #18480 (344.13)
 
# Including more/continuous controls?
# ATE
ATE = Match(Y = subsample1$k_mean, 
            Tr = subsample1$treat,
            X = as.matrix(cbind(subsample1$par_mean,subsample1$type, subsample1$tier,subsample1$cohort, subsample1$super_opeid, subsample1$region,subsample1$expansion_batch)),
            ties = F,
            estimand = "ATE")
summary(ATE) #14764 (283.75)

# First stage, estimate probability of attending good university
uni = glm(treat ~ cohort + par_mean + type + region + tier, family = binomial(link="logit"), data=subsample1)
subsample1$Prop_Score = Uni$fitted.values

# Second stage, matching estimate
PS = Match(Y = subsample1$k_mean, 
           Tr = subsample1$treat,
           X = as.matrix(subsample1$Prop_Score),
           ties = F,
           estimand = "ATE")
summary(PS) #17 283

# Checking for balance in discrete regressors case
MatchBalance(Guilty.x ~ White + Male, data = data_merge_new, match.out = ATE)

# And with one continous regressor
MatchBalance(Guilty.x ~ White + Male + PrevCrim, data = data_merge_new, match.out = ATE2)

# And the propensity score
MatchBalance(Guilty.x ~ Prop_Score, data = data_merge_new, match.out = PS)


# Fancy genetic matching algorithm
library(rgenoud)

# Determine the optimal matches
Gen = GenMatch(Tr = subsample1$treat, 
               X = subsample1$par_mean,
               BalanceMatrix = subsample1$par_mean,
               pop.size = 20,
               max.generations = 10)

# Estimate ATET using this optimal match
GenMatch = Match(Y = subsample1$k_mean,
                 Tr = subsample1$treat,
                 X = subsample1$par_mean,
                 Weight.matrix = Gen)
summary(GenMatch) #34 866

# Check how good the match is
MatchBalance(treat ~ par_mean, data = subsample1, match.out = GenMatch)


#########
### prediction
#########
library(glmnet)
library(coefplot)
y=as.matrix(subsample1[,12])# was wage, so here k_mean
x <- as.matrix(subsample1[, c(2, 4, 5,7,11, 13, 18)])


lasso=glmnet(x,y,family="gaussian",alpha=1,standardize=TRUE,intercept=TRUE) 
par(mfrow=c(1,1))
plot(lasso,main="Lasso and coefficients path")
lasso25=glmnet(x,y,family="gaussian",alpha=1,lambda=lasso$lambda[25])
lasso50=glmnet(x,y,family="gaussian",alpha=1,lambda=lasso$lambda[50])
lasso75=glmnet(x,y,family="gaussian",alpha=1,lambda=lasso$lambda[75])

dev.off()
coefplot(lasso75,intercept=FALSE,sort="magnitude",ylab="Variable",xlab="Coefficient",title="LASSO Coefficient Plot (small lambda)")
coefplot(lasso50,intercept=FALSE,sort="magnitude",ylab="Variable",xlab="Coefficient",title="LASSO Coefficient Plot (mid lambda)")
coefplot(lasso25,intercept=FALSE,sort="magnitude",ylab="Variable",xlab="Coefficient",title="LASSO Coefficient Plot (large lambda)")
rm(lasso25,lasso50,lasso75)

#choice of lambda. in glmnet. 
# We can choose lambda by cross-validation using cv.glmnet
# by default it uses a 10-fold cross validation and a grid of 100 lambda values
# lambda.min: the value that gives the min CV criterion
# lambda.1se, the largest value of lambda such that the error is within 
#      1 standard dev from the minimum  CV criterion

set.seed(1)
cv_lasso=cv.glmnet(x,y,family="gaussian",nfolds=10,alpha=1)
# Plot CV criterion
par(mfrow=c(1,1))
plot(cv_lasso,main="Lasso and CV criterion") #horizontal log lambda. looks constatnt in beginning but is not, dotted line with smallest CV criteria on this lambda
#if we want to choose this we use lambda.min below

# list estimated coefficients with lambda.min
cv_lasso$lambda.min
lasso.coef.lambda.min=predict(cv_lasso, x, type="coeff",s=cv_lasso$lambda.min)[1:8,]
lasso.coef.lambda.min

# list estimated coefficients with lambda.1se
cv_lasso$lambda.1se
lasso.coef.lambda.1se=predict(cv_lasso, x, type="coeff",s=cv_lasso$lambda.1se)[1:8,]
lasso.coef.lambda.1se

#whole point of lasso is to select regressors and enforce some coeff to be 0. but lambda not large enough, v small penalty. 
#choose as large as lambda withhin 1 SD lambda. 1 se (Above)
#now much better, regarding what we expect lasso to do

#-------------------------------------------------------------------------#
# RIDGE shrinks parameters rather than selecting them (LASSO)
#-------------------------------------------------------------------------#

# In glmnet setting alpha=0 ensures we apply RIDGE

ridge=glmnet(x,y,family="gaussian",alpha=0) #=0 to incidcate ridge
plot(ridge,main="Ridge and coefficients path")

ridge25=glmnet(x,y,family="gaussian",alpha=0,lambda=ridge$lambda[25])
ridge50=glmnet(x,y,family="gaussian",alpha=0,lambda=ridge$lambda[50])
ridge75=glmnet(x,y,family="gaussian",alpha=0,lambda=ridge$lambda[75])

coefplot(ridge75,intercept=FALSE,sort="magnitude",ylab="Variable",xlab="Coefficient",title="RIDGE Coefficient Plot (small lambda)")
coefplot(ridge50,intercept=FALSE,sort="magnitude",ylab="Variable",xlab="Coefficient",title="RIDGE Coefficient Plot (mid lambda)")
coefplot(ridge25,intercept=FALSE,sort="magnitude",ylab="Variable",xlab="Coefficient",title="RIDGE Coefficient Plot (large lambda)")
rm(ridge25,ridge50,ridge75)

set.seed(1)
cvridge=cv.glmnet(x,y,family="gaussian",alpha=0)
plot(cvridge,main="Ridge and CV criterion")

# list estimated coefficients with lambda.min
cvridge$lambda.min
ridge.coef.lambda.min=predict(cvridge, x, type="coeff",s=cvridge$lambda.min)[1:8,]
ridge.coef.lambda.min

# list estimated coefficients with lambda.1se
cvridge$lambda.1st
ridge.coef.lambda.1se=predict(cvridge, x, type="coeff",s=cvridge$lambda.1se)[1:8,]
ridge.coef.lambda.1se

###########
library(hdm)
y=as.matrix(subsample1[,12])# was wage, so here k_mean
x <- as.matrix(subsample1[, c(2, 4, 5,7,11, 13, 18)])
ylasso <- rlasso(y=y,x=x,post=TRUE)
# ylasso <- rlasso(y=y,x=x,post=TRUE,intercept=TRUE,
#              penalty = list(homoscedastic=FALSE,X.dependent.lambda = FALSE, 
#                             Lambda.start = NULL, c=1.1, gamma = 0.1/log(n)),
#              control = list(numIter=15, tol = 10^-5, threshold = NULL))

# index indicating selected variables
Iy=ylasso$index
# coefficients obtained by lasso
ylasso$coefficients

# Note: these results are very similar to lasso.coef.lambda.1se
# Need to still see how to change the automated selection of lambda here  


#inference. say keep variable in model + analysing everything else

#---------------------------------------------------------------------------------#
# clearly in this model, we are in particular interested in estimating and 
# performing inference on the variable GRADCOLL that accounts for all confounders
#---------------------------------------------------------------------------------#

lasso_effect_p = rlassoEffects(x=x,y=y,index=c(7),method="partialling out",I3=NULL,post=TRUE)
summary(lasso_effect_p)
lasso_effect_d = rlassoEffects(x=x,y=y,index=c(7),method="double selection",I3=NULL,post=TRUE)
summary(lasso_effect_d)



