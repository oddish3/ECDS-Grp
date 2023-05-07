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
#data <- read_dta("l_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "jdata.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/jdata.Rda")
#load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/mean_by_pidp.rda")
#data %<>% select(-l_basrate) %>% left_join(mean_by_pidp, by = "pidp")
#####
data %<>% filter(l_ivfio == 1 & l_ioutcome == 11 & l_basrate > 0 & l_dvage > 0) 
data %<>% select(-starts_with(c("l_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("l_ind")))
data <- data[,-(2:12)]

#conventional dummies
data$l_nchild_dv1 <-ifelse(data$l_nchild_dv>0,1,0) #have children
data$l_nchild_dv2 <-ifelse(data$l_nchild_dv>0,data$l_nchild_dv^2,0) #childern sqr
data$l_ukborn <- ifelse(data$l_ukborn==5,1,0)#not born in uk
data$l_dvage2 <- ifelse(data$l_dvage, data$l_dvage^2,0) #age quad
data$l_hiqual_dv = ifelse(data$l_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$l_jbstatp = ifelse(data$l_jbstat==2, 1,0) #paid - ft/pt emp
data$l_jbstatf = ifelse(data$l_jbstat==12&13, 1,0) #furlough/temp laid off
data$l_jbsizes = ifelse(data$l_jbsize==1&2&3&4, 1,0) #small
data$l_jbsizel = ifelse(data$l_jbsize==8&9, 1,0) #large
data$l_tujbpl = ifelse(data$l_tujbpl==1, 1,0) #union
data$l_marstat_dv = ifelse(data$l_marstat_dv==2, 1,0) #married
data <- mutate(data,
               l_jbnssec8_dvhiman = case_when( #large employers/higher man
                 l_jbnssec8_dv == 1 ~ 1,
                 TRUE ~ 0
               ),
               l_jbnssec8_dvhiprof = case_when( #higher prof
                 l_jbnssec8_dv == 2 ~ 1,
                 TRUE ~ 0
               ),
               l_jbnssec8_dvlowma = case_when( #lower man
                 l_jbnssec8_dv == 3 ~ 1,
                 TRUE ~ 0
               ),
               l_jbnssec8_dvrout = case_when( #low sup/semi rout/rout
                 l_jbnssec8_dv == 6&7&8 ~ 1,
                 TRUE ~ 0
               ) #industry dummies
)
data$l_scsf1 <- ifelse(data$l_scsf1>3, 1,0) #health is fair or poor
data$female <- ifelse(data$l_sex == 2, 1, 0) #female
data$l_basrate <- log(data$l_basrate) #log pay var
data = select(data, -1,-17:-23, ) #remove id var
data %<>% select(-(c("l_jbnssec8_dv", "l_sex","l_sex_dv"))) #remove highly related vars
data %<>% select(-contains(c("l_jbnssec3","l_jbnssec5", "jbsoc00_cc"))) #ditto

#unconventional dummies 
data$l_urban_dv <- ifelse(data$l_urban_dv==2, 1, 0) #rural
data$l_gor_dv <- ifelse(data$l_gor_dv==7&8, 1, 0) #ldn + SE
data$l_scghqh <- ifelse(data$l_scghqh==3&4,1,0) #ability to face problems less than usual
data$l_ivlitrans <- ifelse(data$l_ivlitrans != 0 & data$l_ivlitrans != 9, 1, 0) #interview language wasnt english or welsh
data$l_mhealthtypn1 <- ifelse(data$l_mhealthtypn1==1,1,0) #anxiety 
data$l_hconda37 <- ifelse(data$l_hconda37>0,data$l_hconda37,0) #know age had anxiety at
data$l_hconda372 <- ifelse(data$l_hconda37>0, data$l_hconda37^2,0) #anxiety sqr
data$l_ftquals <- ifelse(data$l_ftquals==1,1,0) #recent educ qual
data$l_feend <- ifelse(data$l_feend>0, data$l_hconda37,0)
data$l_feend2 <- ifelse(data$l_feend>0, data$l_hconda37^2,0) #left educ sqr
data$l_hedlik <- ifelse(data$l_hedlik==3&4&5,1,0) #not likely to purs FE
data$l_qfhigh_dvd <- ifelse(data$l_qfhigh_dv == 1&2&3&4&5&6, 1,0) #degree
data$l_qfhigh_dva <- ifelse(data$l_qfhigh_dv == 7&8&9&10&11, 1,0) #a levels
data$l_qfhigh_dvn <- ifelse(data$l_qfhigh_dv == 96, 1,0) #no quals
data$l_lnprnt <- ifelse(data$l_lprnt>0, data$l_lprnt,0) #nat child
data$l_lnprnt2 <- ifelse(data$l_lprnt>0, data$l_lprnt^2,0) #sqr
data$l_lprnt <- ifelse(data$l_lprnt == 2, 1,0) #not natural parent
data$l_jspl <-  ifelse(data$l_lprnt>2, 1,0) #dont work at home 
data$l_jsttwtb <- ifelse(data$l_jsttwtb>0, data$l_jsttwtb,0) #time to work
data$l_jsttwtb2 <- ifelse(data$l_jsttwtb>0, data$l_jsttwtb^2,0) #time to work sq
data$l_fivealcdr <-  ifelse(data$l_fivealcdr>3, 1,0) #2 or more 5 alc drink in last 4 wk
data$l_ypnetcht <-  ifelse(data$l_ypnetcht>=3, 1,0) #more than 1 hour social media
data$l_scsf7 <-  ifelse(data$l_scsf7==1&2&3, 1,0) #p/m health hurt social life
data$l_yr2uk4 <-  ifelse(data$l_yr2uk4>0, data$l_yr2uk4,0)
data$l_yr2uk42 <-  ifelse(data$l_yr2uk4>0, data$l_yr2uk4^2,0) #year came to uk sqr 
data$l_eumem <-  ifelse(data$l_eumem==2, 1,0) #leave eu
data$l_ncrr8 <-  ifelse(data$l_ncrr8>1, 1,0) #partner live far away
data$l_smoker <- ifelse(data$l_smoker==1,1,0) #smokes
data$l_nxtendreas3 <-ifelse(data$l_nxtendreas3==1 & data$l_nxtendreas4 ==1 & 
                              data$l_nxtendreas5 == 1& data$l_nxtendreas7==1, 1,0) # made redundant,sacked, temp job loss or health reason left job
data$l_sasian <- ifelse(data$l_racel ==9 & 10 &11,1,0)
data$l_carr <- ifelse(data$l_racel ==14,1,0)
data$l_afr <- ifelse(data$l_racel ==15,1,0)
data$l_upset <-ifelse(data$l_upset>1,1,0) #dont turn to mum when upset = not close with mum 
data$l_bensta2 <- ifelse(data$l_bensta2==1,1,0) #educ grant

ggplot(data, aes(x=l_basrate, colour=as.factor(female)))+geom_density() +theme_classic()




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

quantile(data$l_basrate, 0.01)
quantile(data$l_basrate, 0.99)
data %<>% filter(l_basrate> 1.465401& l_basrate<3.366332)
save(data, file = "jjdata.Rda")


# linear regression --------

OLS1<-lm(l_basrate ~ female, data)
summary(OLS1)

# run OLS regression with all
OLS2<-lm(l_basrate ~ female + ., data)
summary(OLS2)

#conventional controls
OLS3<-lm(l_basrate ~ female*(l_nchild_dv1 + l_nchild_dv2 + l_ukborn + l_dvage + l_dvage2 + 
                               l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                               l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                               l_jbnssec8_dvrout + l_scsf1), data)
summary(OLS3)

#unconv controls 
ols4<-lm(l_basrate ~ female*(l_nchild_dv1 + l_nchild_dv2 + l_ukborn + l_dvage + l_dvage2 + 
                                 l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                                 l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                                 l_jbnssec8_dvrout + l_scsf1 +
                                 l_urban_dv + l_gor_dv + l_scghqh + l_ivlitrans + l_mhealthtypn1 + l_hconda37 + l_hconda372 + 
                                 l_ftquals + l_feend + l_feend2 + l_hedlik + l_qfhigh_dvd + l_qfhigh_dva + l_qfhigh_dvn + l_lnprnt + l_lnprnt2 + l_lprnt +
                                 l_jspl + l_jsttwtb  + l_jsttwtb2 + l_fivealcdr + l_ypnetcht + l_scsf7 + l_yr2uk4 + l_yr2uk42 + 
                                 l_eumem + l_ncrr8 + l_smoker + l_nxtendreas3 + l_sasian + l_carr + l_afr + l_upset + l_bensta2) + 
           (l_nchild_dv1 + l_nchild_dv2 + l_ukborn + l_dvage + l_dvage2 + 
              l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
              l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
              l_jbnssec8_dvrout + l_scsf1 +
              l_urban_dv + l_gor_dv + l_scghqh + l_ivlitrans + l_mhealthtypn1 + l_hconda37 + l_hconda372 + 
              l_ftquals + l_feend + l_feend2 + l_hedlik + l_qfhigh_dvd + l_qfhigh_dva + l_qfhigh_dvn+ l_lnprnt + l_lnprnt2 + l_lprnt +
              l_jspl + l_jsttwtb  + l_jsttwtb2 + l_fivealcdr + l_ypnetcht + l_scsf7 + l_yr2uk4 + l_yr2uk42 + 
              l_eumem + l_ncrr8 + l_smoker + l_nxtendreas3 + l_sasian + l_carr + l_afr + l_upset + l_bensta2)^2, data)
summary(ols4)

#hdm lasso ----- 
#conventional controls
con = model.matrix(~-1 + female*(l_nchild_dv1 + l_nchild_dv2 + l_ukborn + l_dvage + l_dvage2 + 
                                   l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                                   l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                                   l_jbnssec8_dvrout + l_scsf1), data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE))
dim(con)
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$l_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso11 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso11)

#unconventional controls 
uncon = model.matrix(~-1 + female*(l_nchild_dv1 + l_nchild_dv2 + l_ukborn + l_dvage + l_dvage2 + 
                                     l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                                     l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                                     l_jbnssec8_dvrout + l_scsf1 +
                                     l_urban_dv + l_gor_dv + l_scghqh + l_ivlitrans + l_mhealthtypn1 + l_hconda37 + l_hconda372 + 
                                     l_ftquals + l_feend + l_feend2 + l_hedlik + l_qfhigh_dvd + l_qfhigh_dva + l_qfhigh_dvn + l_lnprnt + l_lnprnt2 + l_lprnt +
                                     l_jspl + l_jsttwtb  + l_jsttwtb2 + l_fivealcdr + l_ypnetcht + l_scsf7 + l_yr2uk4 + l_yr2uk42 + 
                                     l_eumem + l_ncrr8 + l_smoker + l_nxtendreas3 + l_sasian + l_carr + l_afr + l_upset + l_bensta2) + 
                       (l_nchild_dv1 + l_nchild_dv2 + l_ukborn + l_dvage + l_dvage2 + 
                          l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                          l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                          l_jbnssec8_dvrout + l_scsf1 +
                          l_urban_dv + l_gor_dv + l_scghqh + l_ivlitrans + l_mhealthtypn1 + l_hconda37 + l_hconda372 + 
                          l_ftquals + l_feend + l_feend2 + l_hedlik + l_qfhigh_dvd + l_qfhigh_dva + l_qfhigh_dvn+ l_lnprnt + l_lnprnt2 + l_lprnt +
                          l_jspl + l_jsttwtb  + l_jsttwtb2 + l_fivealcdr + l_ypnetcht + l_scsf7 + l_yr2uk4 + l_yr2uk42 + 
                          l_eumem + l_ncrr8 + l_smoker + l_nxtendreas3 + l_sasian + l_carr + l_afr + l_upset + l_bensta2)^2, data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(uncon)
index.female <- grep("female", colnames(uncon))

x=uncon[,-1] # covariates

y <- data$l_basrate
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
dat %<>% relocate(l_basrate,female)
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
rm(data)
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/kkdata.Rda")
k <- data
rm(data)
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/lldata.Rda")
l<- data
rm(data)


#table ----
#conventional contrls
Zj <- j %>% select(c("female","j_nchild_dv1", "j_nchild_dv2", "j_ukborn", "j_dvage", "j_dvage2",
                       "j_hiquaj_dv", "j_jbstatp", "j_jbstatf", "j_jbsizes", "j_jbsizel", 
                       "j_tujbpl", "j_marstat_dv", "j_jbnssec8_dvhiman", "j_jbnssec8_dvhiprof", "j_jbnssec8_dvlowma",
                       "j_jbnssec8_dvrout", "j_scsf1"))
sumtable(Zj, group = 'female', out = 'latex')
Zk <- k %>% select(c("female","k_nchild_dv1", "k_nchild_dv2", "k_ukborn", "k_dvage", "k_dvage2",
                    "k_hiqual_dv", "k_jbstatp", "k_jbstatf", "k_jbsizes", "k_jbsizel", 
                    "k_tujbpl", "k_marstat_dv", "k_jbnssec8_dvhiman", "k_jbnssec8_dvhiprof", "k_jbnssec8_dvlowma",
                    "k_jbnssec8_dvrout", "k_scsf1"))
sumtable(Zk, group = 'female', out = 'latex')

Zl <- l %>% select(c("female","l_nchild_dv1", "l_nchild_dv2", "l_ukborn", "l_dvage", "l_dvage2",
                    "l_hiqual_dv", "l_jbstatp", "l_jbstatf", "l_jbsizes", "l_jbsizel", 
                    "l_tujbpl", "l_marstat_dv", "l_jbnssec8_dvhiman", "l_jbnssec8_dvhiprof", "l_jbnssec8_dvlowma",
                    "l_jbnssec8_dvrout", "l_scsf1"))
Zl<-sumtable(Zl, group = 'female', out = 'latex')


#uncon

Zj <- j %>% select(c("female","j_urban_dv", "j_gor_dv", "j_scghqh", "j_ivlitrans", "j_mhealthtypn1", "j_hconda37", "j_hconda372",
                       "j_ftquals", "j_feend", "j_feend2", "j_hedlik", "j_qfhigh_dvd", "j_qfhigh_dva", "j_qfhigh_dvn", "j_lnprnt", "j_lnprnt2", "j_lprnt",
                       "j_jspl", "j_jsttwtb", "j_jsttwtb2", "j_fivealcdr", "j_ypnetcht", "j_scsf7", "j_yr2uk4", "j_yr2uk42", 
                       "j_eumem", "j_ncrr8", "j_smoker", "j_nxtendreas3", "j_sasian", "j_carr", "j_afr", "j_upset", "j_bensta2"))
sumtable(Zj, group = 'female', out = 'latex')
Zk <- k %>% select(c("female","k_urban_dv", "k_gor_dv", "k_scghqh", "k_ivlitrans", "k_mhealthtypn1", "k_hconda37", "k_hconda372",
                     "k_ftquals", "k_feend", "k_feend2", "k_hedlik", "k_qfhigh_dvd", "k_qfhigh_dva", "k_qfhigh_dvn", "k_lnprnt", "k_lnprnt2", "k_lprnt",
                     "k_jspl", "k_jsttwtb", "k_jsttwtb2", "k_fivealcdr", "k_ypnetcht", "k_scsf7", "k_yr2uk4", "k_yr2uk42", 
                     "k_eumem", "k_ncrr8", "k_smoker", "k_nxtendreas3", "k_sasian", "k_carr", "k_afr", "k_upset", "k_bensta2"))
sumtable(Zk, group = 'female', out = 'latex')

Zl <- l %>% select(c("female","l_urban_dv", "l_gor_dv", "l_scghqh", "l_ivlitrans", "l_mhealthtypn1", "l_hconda37", "l_hconda372",
                     "l_ftquals", "l_feend", "l_feend2", "l_hedlik", "l_qfhigh_dvd", "l_qfhigh_dva", "l_qfhigh_dvn", "l_lnprnt", "l_lnprnt2", "l_lprnt",
                     "l_jspl", "l_jsttwtb", "l_jsttwtb2", "l_fivealcdr", "l_ypnetcht", "l_scsf7", "l_yr2uk4", "l_yr2uk42", 
                     "l_eumem", "l_ncrr8", "l_smoker", "l_nxtendreas3", "l_sasian", "l_carr", "l_afr", "l_upset", "l_bensta2"))

sumtable(Zl, group = 'female', out = 'latex')



