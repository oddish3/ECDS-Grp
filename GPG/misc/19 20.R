rm(list=ls())
##### read data --------
#uncomment if first time
#data <- read_dta("k_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "kdata.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/kdata.Rda")
#load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/mean_by_pidp.rda")
#data %<>% select(-k_basrate) %>% left_join(mean_by_pidp, by = "pidp")
#####
data %<>% filter(k_ivfio == 1 & k_ioutcome == 11 & k_basrate > 0 & k_dvage > 0) 
data %<>% select(-starts_with(c("k_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("k_ind")))
data <- data[,-(2:12)]

#conventional dummies
data$k_nchild_dv1 <-ifelse(data$k_nchild_dv>0,1,0) #have children
data$k_nchild_dv2 <-ifelse(data$k_nchild_dv>0,data$k_nchild_dv^2,0) #childern sqr
data$k_ukborn <- ifelse(data$k_ukborn==5,1,0)#not born in uk
data$k_dvage2 <- ifelse(data$k_dvage, data$k_dvage^2,0) #age quad
data$k_hiqual_dv = ifelse(data$k_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$k_jbstatp = ifelse(data$k_jbstat==2, 1,0) #paid - ft/pt emp
data$k_jbstatf = ifelse(data$k_jbstat==12&13, 1,0) #furlough/temp laid off
data$k_jbsizes = ifelse(data$k_jbsize==1&2&3&4, 1,0) #small
data$k_jbsizel = ifelse(data$k_jbsize==8&9, 1,0) #large
data$k_tujbpl = ifelse(data$k_bensta3==1, 1,0) #changed to mentioned union
data$k_marstat_dv = ifelse(data$k_marstat_dv==2, 1,0) #married
data <- mutate(data,
               k_jbnssec8_dvhiman = case_when( #large employers/higher man
                 k_jbnssec8_dv == 1 ~ 1,
                 TRUE ~ 0
               ),
               k_jbnssec8_dvhiprof = case_when( #higher prof
                 k_jbnssec8_dv == 2 ~ 1,
                 TRUE ~ 0
               ),
               k_jbnssec8_dvlowma = case_when( #lower man
                 k_jbnssec8_dv == 3 ~ 1,
                 TRUE ~ 0
               ),
               k_jbnssec8_dvrout = case_when( #low sup/semi rout/rout
                 k_jbnssec8_dv == 6&7&8 ~ 1,
                 TRUE ~ 0
               ) #industry dummies
)
data$k_scsf1 <- ifelse(data$k_scsf1>3, 1,0) #health is fair or poor
data$female <- ifelse(data$k_sex == 2, 1, 0) #female
data$k_basrate <- log(data$k_basrate) #log pay var
data = select(data, -1,-17:-23, ) #remove id var
data %<>% select(-(c("k_jbnssec8_dv", "k_sex","k_sex_dv"))) #remove highly related vars
data %<>% select(-contains(c("k_jbnssec3","k_jbnssec5", "jbsoc00_cc"))) #ditto

#unconventional dummies 
data$k_urban_dv <- ifelse(data$k_urban_dv==2, 1, 0) #rural
data$k_gor_dv <- ifelse(data$k_gor_dv==7&8, 1, 0) #ldn + SE
data$k_scghqh <- ifelse(data$k_scghqh==3&4,1,0) #ability to face problems less than usual
data$k_ivlitrans <- ifelse(data$k_ivlitrans != 0 & data$k_ivlitrans != 9, 1, 0) #interview language wasnt english or welsh
data$k_mhealthtypn1 <- ifelse(data$k_mhealthtypn1==1,1,0) #anxiety 
data$k_hconda37 <- ifelse(data$k_hconda37>0,data$k_hconda37,0) #know age had anxiety at
data$k_hconda372 <- ifelse(data$k_hconda37>0, data$k_hconda37^2,0) #anxiety sqr
data$k_ftquals <- ifelse(data$k_ftquals==1,1,0) #recent educ qual
data$k_feend <- ifelse(data$k_feend>0, data$k_hconda37,0) #left educ pos
data$k_feend2 <- ifelse(data$k_feend>0, data$k_hconda37^2,0) #left educ sqr
data$k_hedlik <- ifelse(data$k_hedlik==3&4&5,1,0) #not likely to purs FE
data$k_qfhigh_dvd <- ifelse(data$k_qfhigh_dv == 1&2&3&4&5&6, 1,0) #degree
data$k_qfhigh_dva <- ifelse(data$k_qfhigh_dv == 7&8&9&10&11, 1,0) #a levels
data$k_qfhigh_dvn <- ifelse(data$k_qfhigh_dv == 96, 1,0) #no quals
data$k_lnprnt <- ifelse(data$k_lprnt>0, data$k_lprnt,0) #nat child
data$k_lnprnt2 <- ifelse(data$k_lprnt>0, data$k_lprnt^2,0) #sqr
data$k_lprnt <- ifelse(data$k_lprnt == 2, 1,0) #not natural parent
data$k_jspl <-  ifelse(data$k_lprnt>2, 1,0) #dont work at home 
data$k_jsttwtb <- ifelse(data$k_jsttwtb>0, data$k_jsttwtb,0) #time to work
data$k_jsttwtb2 <- ifelse(data$k_jsttwtb>0, data$k_jsttwtb^2,0) #time to work sq
data$k_fivealcdr <-  ifelse(data$k_auditc5==4&5, 1,0) #changed to weekly/daily drinking 
data$k_ypnetcht <-  ifelse(data$k_ypnetcht>=3, 1,0) #more than 1 hour social media
data$k_scsf7 <-  ifelse(data$k_scsf7==1&2&3, 1,0) #p/m health hurt social life
data$k_yr2uk4 <-  ifelse(data$k_yr2uk4>0, data$k_yr2uk4,0)
data$k_yr2uk42 <-  ifelse(data$k_yr2uk4>0, data$k_yr2uk4^2,0) #year came to uk sqr 
data$k_eumem <-  ifelse(data$k_eumem==2, 1,0) #leave eu
data$k_ncrr8 <-  ifelse(data$k_ncrr8>1, 1,0) #partner live far away
data$k_smoker <- ifelse(data$k_smoker==1,1,0) #smokes
data$k_nxtendreas3 <-ifelse(data$k_nxtendreas3==1 & data$k_nxtendreas4 ==1 & 
                              data$k_nxtendreas5 == 1& data$k_nxtendreas7==1, 1,0) # made redundant,sacked, temp job loss or health reason left job
data$k_sasian <- ifelse(data$k_racel ==9 & 10 &11,1,0)
data$k_carr <- ifelse(data$k_racel ==14,1,0)
data$k_afr <- ifelse(data$k_racel ==15,1,0)
data$k_upset <-ifelse(data$k_upset>1,1,0) #dont turn to mum when upset = not close with mum 
data$k_bensta2 <- ifelse(data$k_bensta2==1,1,0) #educ grant

ggplot(data, aes(x=k_basrate, colour=as.factor(female)))+geom_density() +theme_classic()

con = model.matrix(~-1 + female*(k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                                   k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                   k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                   k_jbnssec8_dvrout + k_scsf1) + 
                     (k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                        k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                        k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                        k_jbnssec8_dvrout + k_scsf1)^2, data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587

uncon = model.matrix(~-1 + female*(k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                                     k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                     k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                     k_jbnssec8_dvrout + k_scsf1 +
                                     k_urban_dv + k_gor_dv + k_scghqh + k_ivlitrans + k_mhealthtypn1 + k_hconda37 + k_hconda372 + 
                                     k_ftquals + k_feend + k_feend2 + k_hedlik + k_qfhigh_dvd + k_qfhigh_dva + k_qfhigh_dvn + k_lnprnt + k_lnprnt2 + k_lprnt +
                                     k_jspl + k_jsttwtb  + k_jsttwtb2 + k_fivealcdr + k_ypnetcht + k_scsf7 + k_yr2uk4 + k_yr2uk42 + 
                                     k_eumem + k_ncrr8 + k_smoker + k_nxtendreas3 + k_sasian + k_carr + k_afr + k_upset + k_bensta2) + 
                       (k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                          k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                          k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                          k_jbnssec8_dvrout + k_scsf1 +
                          k_urban_dv + k_gor_dv + k_scghqh + k_ivlitrans + k_mhealthtypn1 + k_hconda37 + k_hconda372 + 
                          k_ftquals + k_feend + k_feend2 + k_hedlik + k_qfhigh_dvd + k_qfhigh_dva + k_qfhigh_dvn+ k_lnprnt + k_lnprnt2 + k_lprnt +
                          k_jspl + k_jsttwtb  + k_jsttwtb2 + k_fivealcdr + k_ypnetcht + k_scsf7 + k_yr2uk4 + k_yr2uk42 + 
                          k_eumem + k_ncrr8 + k_smoker + k_nxtendreas3 + k_sasian + k_carr + k_afr + k_upset + k_bensta2)^2, data = data) #controls 
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

quantile(data$k_basrate, 0.01)
quantile(data$k_basrate, 0.99)
data %<>% filter(k_basrate> 1.470176& k_basrate<3.401197)

#table ----
#conventional contrls
Z <- data %>% select(c("female","k_nchild_dv1", "k_nchild_dv2", "k_ukborn", "k_dvage", "k_dvage2",
                       "k_hiqual_dv", "k_jbstatp", "k_jbstatf", "k_jbsizes", "k_jbsizel", 
                       "k_tujbpl", "k_marstat_dv", "k_jbnssec8_dvhiman", "k_jbnssec8_dvhiprof", "k_jbnssec8_dvlowma",
                       "k_jbnssec8_dvrout", "k_scsf1"))


sumtable(Z, group = 'female')
#uncon
Z <- data %>% select(c("female","k_urban_dv", "k_gor_dv", "k_scghqh", "k_ivlitrans", "k_mhealthtypn1", "k_hconda37", "k_hconda372",
                       "k_ftquals", "k_feend", "k_feend2", "k_hedlik", "k_qfhigh_dvd", "k_qfhigh_dva", "k_qfhigh_dvn", "k_lnprnt", "k_lnprnt2", "k_lprnt",
                       "k_jspl", "k_jsttwtb", "k_jsttwtb2", "k_fivealcdr", "k_ypnetcht", "k_scsf7", "k_yr2uk4", "k_yr2uk42", 
                       "k_eumem", "k_ncrr8", "k_smoker", "k_nxtendreas3", "k_sasian", "k_carr", "k_afr", "k_upset", "k_bensta2"))
sumtable(Z, group = 'female')
# linear regression --------

OLS1<-lm(k_basrate ~ female, data)
summary(OLS1)

# run OLS regression with all
OLS2<-lm(k_basrate ~ female + ., data)
summary(OLS2)

#conventional controls
OLS3<-lm(k_basrate ~ female*(k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                               k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                               k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                               k_jbnssec8_dvrout + k_scsf1), data)
summary(OLS3)

#unconv controls 
ols4<-lm(k_basrate ~ female*(k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                               k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                               k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                               k_jbnssec8_dvrout + k_scsf1) +
           (k_urban_dv + k_gor_dv + k_scghqh + k_ivlitrans + k_mhealthtypn1 + k_hconda37 + k_hconda372 + 
              k_ftquals + k_feend + k_feend2 + k_hedlik + k_qfhigh_dvd + k_qfhigh_dva + k_qfhigh_dvn + k_lnprnt + k_lnprnt2 + k_lprnt +
              k_jspl + k_jsttwtb  + k_jsttwtb2 + k_fivealcdr + k_ypnetcht + k_scsf7 + k_yr2uk4 + k_yr2uk42 + 
              k_eumem + k_ncrr8 + k_smoker + k_nxtendreas3 + k_sasian + k_carr + k_afr + k_upset + k_bensta2)^2, data)
summary(ols4)

#hdm lasso ----- 
#conventional controls
con = model.matrix(~-1 + female*(k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                                   k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                   k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                   k_jbnssec8_dvrout + k_scsf1), data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE))
dim(con)
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$k_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso11 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso11)

#unconventional controls 
uncon = model.matrix(~-1 + female*(k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                                     k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                     k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                     k_jbnssec8_dvrout + k_scsf1 +
                                     k_urban_dv + k_gor_dv + k_scghqh + k_ivlitrans + k_mhealthtypn1 + k_hconda37 + k_hconda372 + 
                                     k_ftquals + k_feend + k_feend2 + k_hedlik + k_qfhigh_dvd + k_qfhigh_dva + k_qfhigh_dvn + k_lnprnt + k_lnprnt2 + k_lprnt +
                                     k_jspl + k_jsttwtb  + k_jsttwtb2 + k_fivealcdr + k_ypnetcht + k_scsf7 + k_yr2uk4 + k_yr2uk42 + 
                                     k_eumem + k_ncrr8 + k_smoker + k_nxtendreas3 + k_sasian + k_carr + k_afr + k_upset + k_bensta2) + 
                       (k_nchild_dv1 + k_nchild_dv2 + k_ukborn + k_dvage + k_dvage2 + 
                          k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                          k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                          k_jbnssec8_dvrout + k_scsf1 +
                          k_urban_dv + k_gor_dv + k_scghqh + k_ivlitrans + k_mhealthtypn1 + k_hconda37 + k_hconda372 + 
                          k_ftquals + k_feend + k_feend2 + k_hedlik + k_qfhigh_dvd + k_qfhigh_dva + k_qfhigh_dvn+ k_lnprnt + k_lnprnt2 + k_lprnt +
                          k_jspl + k_jsttwtb  + k_jsttwtb2 + k_fivealcdr + k_ypnetcht + k_scsf7 + k_yr2uk4 + k_yr2uk42 + 
                          k_eumem + k_ncrr8 + k_smoker + k_nxtendreas3 + k_sasian + k_carr + k_afr + k_upset + k_bensta2)^2, data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(uncon)
index.female <- grep("female", colnames(uncon))

x=uncon[,-1] # covariates

y <- data$k_basrate
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
dat %<>% relocate(k_basrate,female)
dat=dat[ , which(apply(dat, 2, var) != 0)] #remove zv col
y=as.matrix(dat[,1]) #wage
d=as.matrix(dat[,2]) #female

dat %<>% mutate_all(as.numeric)
x=as.matrix(dat[,3:644])

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




