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
#data <- read_dta("j_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "jdata.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/jdata.Rda")
#load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/mean_by_pidp.rda")
#data %<>% select(-j_basrate) %>% left_join(mean_by_pidp, by = "pidp")
#####
data %<>% filter(j_ivfio == 1 & j_ioutcome == 11 & j_basrate > 0 & j_dvage > 0) 
data %<>% select(-starts_with(c("j_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("j_ind")))
data <- data[,-(2:12)]

#conventional dummies
data$j_nchild_dv1 <-ifelse(data$j_nchild_dv>0,1,0) #have children
data$j_nchild_dv2 <-ifelse(data$j_nchild_dv>0,data$j_nchild_dv^2,0) #childern sqr
data$j_ukborn <- ifelse(data$j_ukborn==5,1,0)#not born in uk
data$j_dvage2 <- ifelse(data$j_dvage, data$j_dvage^2,0) #age quad
data$j_hiquaj_dv = ifelse(data$j_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$j_jbstatp = ifelse(data$j_jbstat==2, 1,0) #paid - ft/pt emp
data$j_jbstatf = ifelse(data$j_jbstat==12&13, 1,0) #furlough/temp laid off
data$j_jbsizes = ifelse(data$j_jbsize==1&2&3&4, 1,0) #small
data$j_jbsizel = ifelse(data$j_jbsize==8&9, 1,0) #large
data$j_tujbpl = ifelse(data$j_tujbpl==1, 1,0) #union
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
data$j_scsf1 <- ifelse(data$j_scsf1>3, 1,0) #health is fair or poor
data$female <- ifelse(data$j_sex == 2, 1, 0) #female
data$j_basrate <- log(data$j_basrate) #log pay var
data = select(data, -1,-17:-23, ) #remove id var
data %<>% select(-(c("j_jbnssec8_dv", "j_sex","j_sex_dv"))) #remove highly related vars
data %<>% select(-contains(c("j_jbnssec3","j_jbnssec5", "jbsoc00_cc"))) #ditto

#unconventional dummies 
data$j_urban_dv <- ifelse(data$j_urban_dv==2, 1, 0) #rural
data$j_gor_dv <- ifelse(data$j_gor_dv==7&8, 1, 0) #ldn + SE
data$j_scghqh <- ifelse(data$j_scghqh==3&4,1,0) #ability to face problems less than usual
data$j_ivlitrans <- ifelse(data$j_ivlitrans != 0 & data$j_ivlitrans != 9, 1, 0) #interview language wasnt english or welsh
data$j_mhealthtypn1 <- ifelse(data$j_mhealthtypn1==1,1,0) #anxiety 
data$j_hconda37 <- ifelse(data$j_hconda37>0,data$j_hconda37,0) #know age had anxiety at
data$j_hconda372 <- ifelse(data$j_hconda37>0, data$j_hconda37^2,0) #anxiety sqr
data$j_ftquals <- ifelse(data$j_ftquals==1,1,0) #recent educ qual
data$j_feend <- ifelse(data$j_feend>0, data$j_hconda37,0)
data$j_feend2 <- ifelse(data$j_feend>0, data$j_hconda37^2,0) #left educ sqr
data$j_hedlik <- ifelse(data$j_hedlik==3&4&5,1,0) #not likely to purs FE
data$j_qfhigh_dvd <- ifelse(data$j_qfhigh_dv == 1&2&3&4&5&6, 1,0) #degree
data$j_qfhigh_dva <- ifelse(data$j_qfhigh_dv == 7&8&9&10&11, 1,0) #a levels
data$j_qfhigh_dvn <- ifelse(data$j_qfhigh_dv == 96, 1,0) #no quals
data$j_lnprnt <- ifelse(data$j_lprnt>0, data$j_lprnt,0) #nat child
data$j_lnprnt2 <- ifelse(data$j_lprnt>0, data$j_lprnt^2,0) #sqr
data$j_lprnt <- ifelse(data$j_lprnt == 2, 1,0) #not natural parent
data$j_jspl <-  ifelse(data$j_lprnt>2, 1,0) #dont work at home 
data$j_jsttwtb <- ifelse(data$j_jsttwtb>0, data$j_jsttwtb,0) #time to work
data$j_jsttwtb2 <- ifelse(data$j_jsttwtb>0, data$j_jsttwtb^2,0) #time to work sq
data$j_fivealcdr <-  ifelse(data$j_fivealcdr>3, 1,0) #2 or more 5 alc drink in last 4 wk
data$j_ypnetcht <-  ifelse(data$j_ypnetcht>=3, 1,0) #more than 1 hour social media
data$j_scsf7 <-  ifelse(data$j_scsf7==1&2&3, 1,0) #p/m health hurt social life
data$j_yr2uk4 <-  ifelse(data$j_yr2uk4>0, data$j_yr2uk4,0)
data$j_yr2uk42 <-  ifelse(data$j_yr2uk4>0, data$j_yr2uk4^2,0) #year came to uk sqr 
data$j_eumem <-  ifelse(data$j_eumem==2, 1,0) #leave eu
data$j_ncrr8 <-  ifelse(data$j_ncrr8>1, 1,0) #partner live far away
data$j_smoker <- ifelse(data$j_smoker==1,1,0) #smokes
data$j_nxtendreas3 <-ifelse(data$j_nxtendreas3==1 & data$j_nxtendreas4 ==1 & 
                              data$j_nxtendreas5 == 1& data$j_nxtendreas7==1, 1,0) # made redundant,sacked, temp job loss or health reason left job
data$j_sasian <- ifelse(data$j_racel ==9 & 10 &11,1,0)
data$j_carr <- ifelse(data$j_racel ==14,1,0)
data$j_afr <- ifelse(data$j_racel ==15,1,0)
data$j_upset <-ifelse(data$j_upset>1,1,0) #dont turn to mum when upset = not close with mum 
data$j_bensta2 <- ifelse(data$j_bensta2==1,1,0) #educ grant

ggplot(data, aes(x=j_basrate, colour=as.factor(female)))+geom_density() +theme_classic()

con = model.matrix(~-1 + female*(j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                                   j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                                   j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                                   j_jbnssec8_dvrout + j_scsf1) + 
                     (j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                        j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                        j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                        j_jbnssec8_dvrout + j_scsf1)^2, data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587

uncon = model.matrix(~-1 + female*(j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                                     j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                                     j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                                     j_jbnssec8_dvrout + j_scsf1 +
                                     j_urban_dv + j_gor_dv + j_scghqh + j_ivlitrans + j_mhealthtypn1 + j_hconda37 + j_hconda372 + 
                                     j_ftquals + j_feend + j_feend2 + j_hedlik + j_qfhigh_dvd + j_qfhigh_dva + j_qfhigh_dvn + j_lnprnt + j_lnprnt2 + j_lprnt +
                                     j_jspl + j_jsttwtb  + j_jsttwtb2 + j_fivealcdr + j_ypnetcht + j_scsf7 + j_yr2uk4 + j_yr2uk42 + 
                                     j_eumem + j_ncrr8 + j_smoker + j_nxtendreas3 + j_sasian + j_carr + j_afr + j_upset + j_bensta2) + 
                       (j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                          j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                          j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                          j_jbnssec8_dvrout + j_scsf1 +
                          j_urban_dv + j_gor_dv + j_scghqh + j_ivlitrans + j_mhealthtypn1 + j_hconda37 + j_hconda372 + 
                          j_ftquals + j_feend + j_feend2 + j_hedlik + j_qfhigh_dvd + j_qfhigh_dva + j_qfhigh_dvn+ j_lnprnt + j_lnprnt2 + j_lprnt +
                          j_jspl + j_jsttwtb  + j_jsttwtb2 + j_fivealcdr + j_ypnetcht + j_scsf7 + j_yr2uk4 + j_yr2uk42 + 
                          j_eumem + j_ncrr8 + j_smoker + j_nxtendreas3 + j_sasian + j_carr + j_afr + j_upset + j_bensta2)^2, data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587


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

#table ----
#conventional contrls
Z <- data %>% select(c("female","j_nchild_dv1", "j_nchild_dv2", "j_ukborn", "j_dvage", "j_dvage2",
                       "j_hiqual_dv", "j_jbstatp", "j_jbstatf", "j_jbsizes", "j_jbsizel", 
                       "j_tujbpl", "j_marstat_dv", "j_jbnssec8_dvhiman", "j_jbnssec8_dvhiprof", "j_jbnssec8_dvlowma",
                       "j_jbnssec8_dvrout", "j_scsf1"))


sumtable(Z, group = 'female')
#uncon
Z <- data %>% select(c("female","j_urban_dv", "j_gor_dv", "j_scghqh", "j_ivlitrans", "j_mhealthtypn1", "j_hconda37", "j_hconda372",
  "j_ftquals", "j_feend", "j_feend2", "j_hedlik", "j_qfhigh_dvd", "j_qfhigh_dva", "j_qfhigh_dvn", "j_lnprnt", "j_lnprnt2", "j_lprnt",
  "j_jspl", "j_jsttwtb", "j_jsttwtb2", "j_fivealcdr", "j_ypnetcht", "j_scsf7", "j_yr2uk4", "j_yr2uk42", 
  "j_eumem", "j_ncrr8", "j_smoker", "j_nxtendreas3", "j_sasian", "j_carr", "j_afr", "j_upset", "j_bensta2"))
sumtable(Z, group = 'female')
# linear regression --------

OLS1<-lm(j_basrate ~ female, data)
summary(OLS1)

# run OLS regression with all
OLS2<-lm(j_basrate ~ female + ., data)
summary(OLS2)

#conventional controls
OLS3<-lm(j_basrate ~ female*(j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                                       j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                                       j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                                       j_jbnssec8_dvrout + j_scsf1), data)
summary(OLS3)

#unconv controls 
ols4<-lm(j_basrate ~ female*(j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                         j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                         j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                         j_jbnssec8_dvrout + j_scsf1 +
                         j_urban_dv + j_gor_dv + j_scghqh + j_ivlitrans + j_mhealthtypn1 + j_hconda37 + j_hconda372 + 
                         j_ftquals + j_feend + j_feend2 + j_hedlik + j_qfhigh_dvd + j_qfhigh_dva + j_qfhigh_dvn + j_lnprnt + j_lnprnt2 + j_lprnt +
                         j_jspl + j_jsttwtb  + j_jsttwtb2 + j_fivealcdr + j_ypnetcht + j_scsf7 + j_yr2uk4 + j_yr2uk42 + 
                         j_eumem + j_ncrr8 + j_smoker + j_nxtendreas3 + j_sasian + j_carr + j_afr + j_upset + j_bensta2), data)
summary(ols4)

#hdm lasso ----- 

# no controls 
nocon = model.matrix(~-1 + female+(.) , data)
nocon <- nocon[, which(apply(nocon, 2, var) != 0)]
nocon = apply(nocon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
index.female <- grep("female", colnames(nocon))
x=nocon[,-1] 
y <- data$j_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,index = index.female, method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,index = index.female,method="double selection")
summary(ylasso)
## all controls 

allcon = model.matrix(~-1 + female+(.) , data)
allcon <- con[, which(apply(allcon, 2, var) != 0)]
allcon = apply(allcon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
index.female <- grep("female", colnames(allcon))
x=allcon[,-1] 
y <- data$j_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,index = index.female, method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,index = index.female,method="double selection")
summary(ylasso)


#conventional controls
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$j_basrate
d <- data$female

one<- lm(y ~ con, data)
summary(one)

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)

#unconventional controls 
uncon = model.matrix(~-1 + female*(j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                                     j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                                     j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                                     j_jbnssec8_dvrout + j_scsf1 +
                                     j_urban_dv + j_gor_dv + j_scghqh + j_ivlitrans + j_mhealthtypn1 + j_hconda37 + j_hconda372 + 
                                     j_ftquals + j_feend + j_feend2 + j_hedlik + j_qfhigh_dvd + j_qfhigh_dva + j_qfhigh_dvn + j_lnprnt + j_lnprnt2 + j_lprnt +
                                     j_jspl + j_jsttwtb  + j_jsttwtb2 + j_fivealcdr + j_ypnetcht + j_scsf7 + j_yr2uk4 + j_yr2uk42 + 
                                     j_eumem + j_ncrr8 + j_smoker + j_nxtendreas3 + j_sasian + j_carr + j_afr + j_upset + j_bensta2) + 
                       (j_nchild_dv1 + j_nchild_dv2 + j_ukborn + j_dvage + j_dvage2 + 
                          j_hiqual_dv + j_jbstatp + j_jbstatf + j_jbsizes  + j_jbsizel + 
                          j_tujbpl + j_marstat_dv + j_jbnssec8_dvhiman + j_jbnssec8_dvhiprof + j_jbnssec8_dvlowma +
                          j_jbnssec8_dvrout + j_scsf1 +
                          j_urban_dv + j_gor_dv + j_scghqh + j_ivlitrans + j_mhealthtypn1 + j_hconda37 + j_hconda372 + 
                          j_ftquals + j_feend + j_feend2 + j_hedlik + j_qfhigh_dvd + j_qfhigh_dva + j_qfhigh_dvn+ j_lnprnt + j_lnprnt2 + j_lprnt +
                          j_jspl + j_jsttwtb  + j_jsttwtb2 + j_fivealcdr + j_ypnetcht + j_scsf7 + j_yr2uk4 + j_yr2uk42 + 
                          j_eumem + j_ncrr8 + j_smoker + j_nxtendreas3 + j_sasian + j_carr + j_afr + j_upset + j_bensta2)^2, data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(uncon)
index.gender <- grep("female", colnames(uncon))
y <- data$j_basrate
effects.female <- rlassoEffects(x = uncon, y = y, index = index.gender)
summary(effects.female)
one<- lm(j_basrate ~ uncon, data)
summary(one)
x=uncon[,-1] # covariates

y <- data$j_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)


##########

plot(effects.female, oldNames = TRUE, joint=TRUE, level=0.95)
joint.CI <- confint(effects.female, level = 0.95, joint = TRUE, las = 2)
joint.CI

#logit regression of furlough ----
con = model.matrix(~-1 + female + female:(j_dvage + j_dvage2 + j_nchild_dv + j_hiquaj_dv  + 
                                            j_jbsizes + j_jbsizel + j_tujbpl + j_marstat_dv + 
                                            j_jbnssec8_dvman + j_jbnssec8_dvint + j_jbnssec8_dvsma + j_jbnssec8_dvlow)+ 
                     (j_dvage + j_dvage2 + j_nchild_dv + j_hiquaj_dv + 
                        j_jbsizes + j_jbsizel + j_tujbpl + j_marstat_dv + 
                        j_jbnssec8_dvman + j_jbnssec8_dvint + j_jbnssec8_dvsma + j_jbnssec8_dvlow)^2,
                   data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
mylogit <- glm(j_jbstatf ~ con, data = data, family = "binomial")
summary(mylogit)

#lasso regression of furlough ----
con = model.matrix(~-1 + female + female:(j_dvage + j_dvage2 + j_nchild_dv + j_hiquaj_dv  + 
                                            j_jbsizes + j_jbsizel + j_tujbpl + j_marstat_dv + 
                                            j_jbnssec8_dvman + j_jbnssec8_dvint + j_jbnssec8_dvsma + j_jbnssec8_dvlow)+ 
                     (j_dvage + j_dvage2 + j_nchild_dv + j_hiquaj_dv + 
                        j_jbsizes + j_jbsizel + j_tujbpl + j_marstat_dv + 
                        j_jbnssec8_dvman + j_jbnssec8_dvint + j_jbnssec8_dvsma + j_jbnssec8_dvlow)^2,
                   data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
mylogit <- rlasso(j_jbstatf ~ con, data = data)
summary(mylogit)



# PCA ------

n=nrow(data)
Index <- 1:n

set.seed(1)
dat <- data %>% slice_sample(n=1000)
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
nfact=537
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
numfact=which.min(fact_aicc[,1])
print(numfact)

factprint=cbind(fact_coef,fact_se,fact_aic,fact_aicc)
factprint
 
