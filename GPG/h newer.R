####-----
rm(list=ls())
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")

# libraries --------
library(vtable)
library(tictoc)
library(xtable)
library(stargazer)
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
data <- read_dta("j_indresp.dta")
data <- zap_labels(data)
data <- as.data.frame(data)
save(data, file = "jdata.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/jdata.Rda")
#load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/mean_by_pidp.rda")
#data %<>% select(-l_basrate) %>% left_join(mean_by_pidp, by = "pidp")
#####
data %<>% filter(j_ivfio == 1 & j_ioutcome == 11 & j_basrate > 0) 
data %<>% select(-starts_with(c("j_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("j_ind")))
data <- data[,-(2:12)]

#conventional dummies
data$j_nchild_dv1 <-ifelse(data$j_nchild_dv>0,1,0) #have children
data$j_ukborn <- ifelse(data$j_ukborn==5,1,0)#not born in uk
data$j_dvage <- ifelse(data$j_dvage>0,data$j_dvage,0) #age 
data$j_dvage2 <- ifelse(data$j_dvage, data$j_dvage^2,0) #age quad
data$j_hiqual_dv = ifelse(data$j_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$j_jbstatp = ifelse(data$j_jbstat==2, 1,0) #in ft or pt emp
data$j_jbstatf = ifelse(data$j_jbstat==12&13, 1,0) #furlough/temp laid off
data$j_jbsizes = ifelse(data$j_jbsize==1&2&3&4, 1,0) #small firm 
data$j_jbsizel = ifelse(data$j_jbsize==8&9, 1,0) #large firm
data$j_tujbpl = ifelse(data$j_tujbpl==1, 1,0) #in a union
data$j_marstat_dv = ifelse(data$j_marstat_dv==2, 1,0) #married
data <- mutate(data,
               j_jbnssec8_dvhiman = case_when( #large employers/higher man
                 j_jbnssec8_dv == 1 ~ 1,
                 TRUE ~ 0
               ),
               j_jbnssec8_dvhiprof = case_when( #higher prof
                 j_jbnssec8_dv == 2 ~ 1,
                 TRUE ~ 0
               ),
               j_jbnssec8_dvlowma = case_when( #lower man
                 j_jbnssec8_dv == 3 ~ 1,
                 TRUE ~ 0
               ),
               j_jbnssec8_dvrout = case_when( #low sup/semi rout/rout
                 j_jbnssec8_dv == 6&7&8 ~ 1,
                 TRUE ~ 0
               ) #industry dummies
)
data$female <- ifelse(data$j_sex == 2, 1, 0) #female
data$j_basrate <- log(data$j_basrate) #log pay var
data = select(data, -1,-17:-23, ) #remove id var
data %<>% select(-(c("j_jbnssec8_dv", "j_sex","j_sex_dv"))) #remove highly related vars
data %<>% select(-contains(c("j_jbnssec3","j_jbnssec5", "jbsoc00_cc"))) #ditto

#unconventional dummies  
data$j_urban_dv <- ifelse(data$j_urban_dv==1, 1, 0) #urban
data$j_gor_dv <- ifelse(data$j_gor_dv==7&8, 1, 0) #ldn + SE
data$j_scghqh <- ifelse(data$j_scghqh==3&4,1,0) #ability to face problems less than usual
data$j_ivlitrans <- ifelse(data$j_ivlitrans != 0 & data$j_ivlitrans != 9, 1, 0) #interview language wasnt english or welsh
data$j_mhealthtypn1 <- ifelse(data$j_mhealthtypn1==1,1,0) #has anxiety 
data$j_hconda37 <- ifelse(data$j_hconda37>0,data$j_hconda37,0) #know age had anxiety at
data$j_hconda372 <- ifelse(data$j_hconda37>0, data$j_hconda37^2,0) #^ sqrd
data$j_ftquals <- ifelse(data$j_ftquals==1,1,0) #recent educ qual
data$j_feend <- ifelse(data$j_feend>0, data$j_feend,0) #age left educ
data$j_feend2 <- ifelse(data$j_feend>0, data$j_feend^2,0) #left educ sqr
data$j_hedlik <- ifelse(data$j_hedlik==3&4&5,1,0) #feels they were not likely to purs FE
data$j_qfhigh_dvd <- ifelse(data$j_qfhigh_dv == 1&2&3&4&5&6, 1,0) #degree educated
data$j_qfhigh_dva <- ifelse(data$j_qfhigh_dv == 7&8&9&10&11, 1,0) #a level educatrd
data$j_qfhigh_dvn <- ifelse(data$j_qfhigh_dv == 96, 1,0) #no formal quals
data$j_ladopt <- ifelse(data$j_ladopt>0, data$j_lprnt,0) #has adopted
data$j_ladopt2 <- ifelse(data$j_ladopt>0, data$j_lprnt^2,0) #has adopted sqr
data$j_jspl <-  ifelse(data$j_lprnt>2, 1,0) #dont work at home 
data$j_jsttwtb <- ifelse(data$j_jsttwtb>0, data$j_jsttwtb,0) #time to work
data$j_jsttwtb2 <- ifelse(data$j_jsttwtb>0, data$j_jsttwtb^2,0) #time to work sq
data$j_fivealcdr <-  ifelse(data$j_fivealcdr>3, 1,0) #2 or more 5 alc drink in last 4 wk
data$j_ypnetcht <-  ifelse(data$j_ypnetcht>=3, 1,0) #more than 1 hour social media
data$j_scsf7 <-  ifelse(data$j_scsf7==1&2&3, 1,0) #p/m health hurt social life
data$j_yr2uk4 <-  ifelse(data$j_yr2uk4>0, data$j_yr2uk4,0) #year came to uk
data$j_yr2uk42 <-  ifelse(data$j_yr2uk4>0, data$j_yr2uk4^2,0) #year came to uk sqr 
data$j_eumem <-  ifelse(data$j_eumem==2, 1,0) #leave eu
data$j_ncrr8 <-  ifelse(data$j_ncrr8>1, 1,0) #partner live far away
data$j_smoker <- ifelse(data$j_smoker==1,1,0) #smokes
data$j_nxtendreas3 <-ifelse(data$j_nxtendreas3==1 & data$j_nxtendreas4 ==1 & 
                              data$j_nxtendreas5 == 1& data$j_nxtendreas7==1, 1,0) # made redundant,sacked, temp job loss or for health reason left job
data$j_sasian <- ifelse(data$j_racel ==9 & 10 &11,1,0) #south asian
data$j_carr <- ifelse(data$j_racel ==14,1,0) #carribean
data$j_afr <- ifelse(data$j_racel ==15,1,0) #african
data$j_upset <-ifelse(data$j_upset>1,1,0) #dont turn to mum when upset = not close with mum 
data$j_bensta2 <- ifelse(data$j_bensta2==1,1,0) #educ grant
data$j_nchild_dv2 <- ifelse(data$j_nchild_dv>0, data$j_nchild_dv^2,0) #number of children sqr
data$j_scsf1 <- ifelse(data$j_scsf1>3, 1,0) #health is fair or poor

ggplot(data, aes(x=j_basrate, colour=as.factor(female)))+geom_density() +theme_classic()




# Identify variables with more than 50% missing values
data[data < 0] <- NA
missing_prop <- colMeans(is.na(data))
vars_to_remove <- names(missing_prop[missing_prop > 0.6])
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

quantile(data$j_basrate, 0.01)
quantile(data$j_basrate, 0.99)
data %<>% filter(j_basrate> 1.465401& j_basrate<3.366332)
save(data, file = "jjdata.Rda")


# linear regression --------

OLS1<-lm(j_basrate ~ female, data)
summary(OLS1)

# run OLS regression with all
OLS2<-lm(j_basrate ~ female + ., data)
summary(OLS2)

#conventional controls
OLS3<-lm(j_basrate ~ female*(j_nchild_dv1 + j_ukborn + j_dvage + j_dvage2 + 
                               j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                               j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                               j_jbnssec8_dvrout), data)
summary(OLS3)

#unconv controls 
ols4<-lm(j_basrate ~ female*(j_nchild_dv1 + j_ukborn + j_dvage + j_dvage2 + 
                               j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                               j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                               j_jbnssec8_dvrout + j_urban_dv+j_gor_dv+j_scghqh+j_ivlitrans+j_mhealthtypn1+j_hconda37+j_hconda372+j_ftquals+j_feend+
                             j_feend2+j_hedlik+j_qfhigh_dvd+j_qfhigh_dva+j_qfhigh_dvn+j_ladopt+j_ladopt2+j_jspl+j_jsttwtb+j_jsttwtb2+j_fivealcdr +
                           j_ypnetcht+j_scsf7+j_yr2uk4+j_yr2uk42+j_eumem+j_ncrr8+j_smoker+j_nxtendreas3+j_sasian+j_carr+j_afr+j_upset+j_bensta2+j_nchild_dv2+j_scsf1) + 
           (j_nchild_dv1 + j_ukborn + j_dvage + j_dvage2 + 
              j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
              j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
              j_jbnssec8_dvrout + j_urban_dv+j_gor_dv+j_scghqh+j_ivlitrans+j_mhealthtypn1+j_hconda37+j_hconda372+j_ftquals+j_feend+
              j_feend2+j_hedlik+j_qfhigh_dvd+j_qfhigh_dva+j_qfhigh_dvn+j_ladopt+j_ladopt2+j_jspl+j_jsttwtb+j_jsttwtb2+j_fivealcdr +
              j_ypnetcht+j_scsf7+j_yr2uk4+j_yr2uk42+j_eumem+j_ncrr8+j_smoker+j_nxtendreas3+j_sasian+j_carr+j_afr+j_upset+j_bensta2+j_nchild_dv2+j_scsf1)^2, data)
summary(ols4)

#hdm lasso ----- 
#conventional controls
con = model.matrix(~-1 + female*(j_nchild_dv1 + j_ukborn + j_dvage + j_dvage2 + 
                                   j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                                   j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                                   j_jbnssec8_dvrout), data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE))
dim(con)
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$j_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso11 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso11)

#unconventional controls 
uncon = model.matrix(~-1 + female*(j_nchild_dv1 + j_ukborn + j_dvage + j_dvage2 + 
                                      j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                                      j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                                      j_jbnssec8_dvrout + j_urban_dv+j_gor_dv+j_scghqh+j_ivlitrans+j_mhealthtypn1+j_hconda37+j_hconda372+j_ftquals+j_feend+
                                      j_feend2+j_hedlik+j_qfhigh_dvd+j_qfhigh_dva+j_qfhigh_dvn+j_ladopt+j_ladopt2+j_jspl+j_jsttwtb+j_jsttwtb2+j_fivealcdr +
                                      j_ypnetcht+j_scsf7+j_yr2uk4+j_yr2uk42+j_eumem+j_ncrr8+j_smoker+j_nxtendreas3+j_sasian+j_carr+j_afr+j_upset+j_bensta2+j_nchild_dv2+j_scsf1) + 
                       (j_nchild_dv1 + j_ukborn + j_dvage + j_dvage2 + 
                          j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                          j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                          j_jbnssec8_dvrout + j_urban_dv+j_gor_dv+j_scghqh+j_ivlitrans+j_mhealthtypn1+j_hconda37+j_hconda372+j_ftquals+j_feend+
                          j_feend2+j_hedlik+j_qfhigh_dvd+j_qfhigh_dva+j_qfhigh_dvn+j_ladopt+j_ladopt2+j_jspl+j_jsttwtb+j_jsttwtb2+j_fivealcdr +
                          j_ypnetcht+j_scsf7+j_yr2uk4+j_yr2uk42+j_eumem+j_ncrr8+j_smoker+j_nxtendreas3+j_sasian+j_carr+j_afr+j_upset+j_bensta2+j_nchild_dv2+j_scsf1)^2, data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(uncon)
index.female <- grep("female", colnames(uncon))

x=uncon[,-1] # covariates

y <- data$j_basrate
d <- data$female

ylasso2 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso2)
ylasso22 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso22)

# PCA ------

n=nrow(data)
Index <- 1:n

set.seed(1)
dat <- data %>% slice_sample(n=500)
dat %<>% relocate(j_basrate,female)
dat=dat[ , which(apply(dat, 2, var) != 0)] #remove zv col
y=as.matrix(dat[,1]) #wage
d=as.matrix(dat[,2]) #female

dat %<>% mutate_all(as.numeric)
x=as.matrix(dat[,3:540])

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
nfact=498
fact_coef = matrix(0,nrow=nfact,ncol=1)
fact_se   = matrix(0,nrow=nfact,ncol=1)
fact_aic  = matrix(0,nrow=nfact,ncol=1)
fact_aicc = matrix(0,nrow=nfact,ncol=1)

tic(for (m in (1:nfact))
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
)
toc()
numfact=which.min(fact_aicc[1:496,1])
print(numfact)

factprint=cbind(fact_coef,fact_se,fact_aic,fact_aicc)
factprint
factprint[496,]

#### table -----------
rm(list=ls())
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/jjdata.Rda")
j <- data
j %<>% select(c("female", "j_nchild_dv1" ,"j_ukborn","j_dvage","j_dvage2",  
                     "j_hiqual_dv","j_jbstatp","j_jbstatf","j_jbsizes","j_jbsizel",  
                     "j_tujbpl","j_marstat_dv","j_jbnssec8_dvhiman","j_jbnssec8_dvhiprof","j_jbnssec8_dvlowma", 
                     "j_jbnssec8_dvrout","j_urban_dv", "j_gor_dv", "j_scghqh", "j_ivlitrans", "j_mhealthtypn1",
                     "j_hconda37", "j_hconda372", "j_ftquals", "j_feend",
                     "j_feend2", "j_hedlik", "j_qfhigh_dvd", "j_qfhigh_dva", "j_qfhigh_dvn", "j_ladopt", "j_ladopt2", 
                     "j_jspl" ,"j_jsttwtb", "j_jsttwtb2", "j_fivealcdr",
                     "j_ypnetcht", "j_scsf7", "j_yr2uk4", "j_yr2uk42" ,"j_eumem", "j_ncrr8", "j_smoker" ,"j_nxtendreas3", "j_sasian", "j_carr", "j_afr",
                     "j_upset", "j_bensta2", "j_nchild_dv2", "j_scsf1", "j_istrtdaty", "j_basrate"))
rm(data)
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/kkdata.Rda")
k <- data
k %<>% select(c("female", "k_nchild_dv1" ,"k_ukborn","k_dvage","k_dvage2",  
                "k_hiqual_dv","k_jbstatp","k_jbstatf","k_jbsizes","k_jbsizel",  
                "k_tujbpl","k_marstat_dv","k_jbnssec8_dvhiman","k_jbnssec8_dvhiprof","k_jbnssec8_dvlowma", 
                "k_jbnssec8_dvrout","k_urban_dv", "k_gor_dv", "k_scghqh", "k_ivlitrans", "k_mhealthtypn1",
                "k_hconda37", "k_hconda372", "k_ftquals", "k_feend",
                "k_feend2", "k_hedlik", "k_qfhigh_dvd", "k_qfhigh_dva", "k_qfhigh_dvn", "k_ladopt", "k_ladopt2", 
                "k_jspl" ,"k_jsttwtb", "k_jsttwtb2", "k_fivealcdr",
                "k_ypnetcht", "k_scsf7", "k_yr2uk4", "k_yr2uk42" ,"k_eumem", "k_ncrr8", "k_smoker" ,"k_nxtendreas3", "k_sasian", "k_carr", "k_afr",
                "k_upset", "k_bensta2", "k_nchild_dv2", "k_scsf1", "k_istrtdaty","k_basrate"))
rm(data)
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/lldata.Rda")
l<- data
l %<>% select(c("female", "l_nchild_dv1" ,"l_ukborn","l_dvage","l_dvage2",  
                "l_hiqual_dv","l_jbstatp","l_jbstatf","l_jbsizes","l_jbsizel",  
                "l_tujbpl","l_marstat_dv","l_jbnssec8_dvhiman","l_jbnssec8_dvhiprof","l_jbnssec8_dvlowma", 
                "l_jbnssec8_dvrout","l_urban_dv", "l_gor_dv", "l_scghqh", "l_ivlitrans", "l_mhealthtypn1",
                "l_hconda37", "l_hconda372", "l_ftquals", "l_feend",
                "l_feend2", "l_hedlik", "l_qfhigh_dvd", "l_qfhigh_dva", "l_qfhigh_dvn", "l_ladopt", "l_ladopt2", 
                "l_jspl" ,"l_jsttwtb", "l_jsttwtb2", "l_fivealcdr",
                "l_ypnetcht", "l_scsf7", "l_yr2uk4", "l_yr2uk42" ,"l_eumem", "l_ncrr8", "l_smoker" ,"l_nxtendreas3", "l_sasian", "l_carr", "l_afr",
                "l_upset", "l_bensta2", "l_nchild_dv2", "l_scsf1", "l_istrtdaty","l_basrate"))
rm(data)
sumtable(j, group = 'female', out = 'latex')
sumtable(k, group = 'female', out = 'latex')
sumtable(l, group = 'female', out = 'latex')
names(j) <- sub("^j_", "", names(j))
names(k) <- sub("^k_", "", names(k))
names(l) <- sub("^l_", "", names(l))

data <- merge(j,l, all = T)
data <- merge(data, k, all = T)

cpi <- tibble(
  istrtdaty = 2018:2022,
  cpi = c(105.4, 107.5, 108.5, 112.6, 127.9) # CPI values from ONS base year 2015
)
data <- left_join(data, cpi, by = "istrtdaty")
data %<>%  mutate(real_wage = basrate / cpi * 100) %>% filter(istrtdaty<2022)

library(RColorBrewer) # load the package

ggplot(data, aes(x = istrtdaty, y = real_wage, group = as.factor(female), colour = as.factor(female))) +
  stat_summary(
    geom = "pointrange", # use pointrange geom
    fun.min = function(z) {quantile(z, 0.25)}, # lower quartile
    fun.max = function(z) {quantile(z, 0.75)}, # upper quartile
    fun = median, # median
    position = position_dodge(width = 0.1), # dodge the points by 0.1 units
    linewidth = 1, size = 1.2 # make the lines twice as thick
  ) +
  theme_bw() + # use black and white theme
  scale_color_brewer(palette = "Set1") # use color palette from RColorBrewer
ggplot(data, aes(x=istrtdaty)) +stat_ecdf() + theme_bw()

ggplot(data, aes(x = istrtdaty, y = jbstatf)) + geom_bar(stat = 'summary',fun=sum)
    