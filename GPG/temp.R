#2019/20 k ----
####-----
rm(list=ls())
##### read data --------
#uncomment if first time
#data <- read_dta("k_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "kdata.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/kdata.Rda")
#####
data %<>% filter(k_ivfio == 1 & k_ioutcome == 11 & k_basrate > 0  & k_paynu_dv>0 & k_dvage>0&k_jbft_dv==1&k_jbstat %in% c(2,12,13))
data %<>% select(-starts_with(c("k_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("k_ind")))
data <- data[,-(2:12)]

#conventional dummies
data$k_nchild_dv1 <-ifelse(data$k_nchild_dv>0,1,0) #have children
data$k_ukborn <- ifelse(data$k_ukborn>0&data$k_ukborn==5,1,0)#not born in uk
data$k_dvage <- ifelse(data$k_dvage>0,data$k_dvage,0) #age 
data$k_dvage2 <- ifelse(data$k_dvage, data$k_dvage^2,0) #age quad
data$k_hiqual_dv = ifelse(data$k_hiqual_dv>0&data$k_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$k_jbstatp = ifelse(data$k_jbterm1==1, 1,0) #in ft or pt emp
data$k_jbstatf = ifelse(data$k_jbstat>0&data$k_jbendreas5==1, 1,0) #temp job ended, no furlough var here so nearest
data$k_jbsizes = ifelse(data$k_jbsize>0&data$k_jbsize==1&2&3&4, 1,0) #small firm 
data$k_jbsizel = ifelse(data$k_jbsize>0&data$k_jbsize==8&9, 1,0) #large firm
data$k_tujbpl = ifelse(data$k_bensta3==1, 1,0) #slight change to mention union
data$k_marstat_dv = ifelse(data$k_marstat_dv>0&data$k_marstat_dv==2, 1,0) #married
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
data$female <- ifelse(data$k_sex == 2, 1, 0) #female
data$k_basrate <- log(data$k_basrate) #log pay var
data$k_paynu_dv <- log(data$k_paynu_dv) #log alt pay var
data$date <- paste(data$k_istrtdatd, data$k_istrtdatm, data$k_istrtdaty, sep = "/")
data$date <- dmy(data$date)
data$covid<- ifelse(data$date>"2020-03-23", 1,0)
data = select(data, -1,-17:-23, ) #remove id var
data %<>% select(-(c("k_jbnssec8_dv", "k_sex","k_sex_dv"))) #remove highly related vars
data %<>% select(-contains(c("k_jbnssec3","k_jbnssec5", "jbsoc00_cc"))) #ditto

#unconventional dummies  
data$k_urban_dv <- ifelse(data$k_urban_dv>0&data$k_urban_dv==1, 1, 0) #urban
data$k_gor_dv <- ifelse(data$k_gor_dv>0&data$k_gor_dv==7&8, 1, 0) #ldn + SE
data$k_scghqh <- ifelse(data$k_scghqh>0&data$k_scghqh==3&4,1,0) #ability to face problems less than usual
data$k_ivlitrans <- ifelse(data$k_ivlitrans>0&data$k_ivlitrans != 0 & data$k_ivlitrans != 9, 1, 0) #interview language wasnt english or welsh
data$k_mhealthtypn1 <- ifelse(data$k_mhealthtypn1>0&data$k_mhealthtypn1==1,1,0) #has anxiety 
data$k_hconda37 <- ifelse(data$k_hconda37>0,data$k_hconda37,0) #know age had anxiety at
data$k_hconda372 <- ifelse(data$k_hconda37>0, data$k_hconda37^2,0) #^ sqrd
data$k_ftquals <- ifelse(data$k_ftquals>0&data$k_ftquals==1,1,0) #recent educ qual
data$k_feend <- ifelse(data$k_feend>0, data$k_feend,0) #age left educ
data$k_feend2 <- ifelse(data$k_feend>0, data$k_feend^2,0) #left educ sqr
data$k_hedlik <- ifelse(data$k_hedlik>0&data$k_hedlik==3&4&5,1,0) #feels they were not likely to purs FE
data$k_qfhigh_dvd <- ifelse(data$k_qfhigh_dv>0&data$k_qfhigh_dv == 1&2&3&4&5&6, 1,0) #degree educated
data$k_qfhigh_dva <- ifelse(data$k_qfhigh_dv>0&data$k_qfhigh_dv == 7&8&9&10&11, 1,0) #a level educatrd
data$k_qfhigh_dvn <- ifelse(data$k_qfhigh_dv>0&data$k_qfhigh_dv == 96, 1,0) #no formal quals
data$k_ladopt <- ifelse(data$k_ladopt>0, data$k_lprnt,0) #has adopted
data$k_ladopt2 <- ifelse(data$k_ladopt>0, data$k_lprnt^2,0) #has adopted sqr
data$k_jspl <-  ifelse(data$k_lprnt>2, 1,0) #dont work at home 
data$k_jsttwtb <- ifelse(data$k_jsttwtb>0, data$k_jsttwtb,0) #time to work
data$k_jsttwtb2 <- ifelse(data$k_jsttwtb>0, data$k_jsttwtb^2,0) #time to work sq
data$k_fivealcdr <-  ifelse(data$k_auditc5==4&5, 1,0) #changed to weekly/daily drinking
data$k_ypnetcht <-  ifelse(data$k_ypnetcht>=3, 1,0) #more than 1 hour social media
data$k_scsf7 <-  ifelse(data$k_scsf7>0&data$k_scsf7==1&2&3, 1,0) #p/m health hurt social life
data$k_yr2uk4 <-  ifelse(data$k_yr2uk4>0, data$k_yr2uk4,0) #year came to uk
data$k_yr2uk42 <-  ifelse(data$k_yr2uk4>0, data$k_yr2uk4^2,0) #year came to uk sqr 
data$k_eumem <-  ifelse(data$k_eumem>0&data$k_eumem==2, 1,0) #leave eu
data$k_ncrr8 <-  ifelse(data$k_ncrr8>1, 1,0) #partner live far away
data$k_smoker <- ifelse(data$k_smoker>0&data$k_smoker==1,1,0) #smokes
data$k_nxtendreas3 <-ifelse(data$k_nxtendreas3>0&data$k_nxtendreas3==1 & data$k_nxtendreas4 ==1 & 
                              data$k_nxtendreas5 == 1& data$k_nxtendreas7==1, 1,0) # made redundant,sacked, temp job loss or for health reason left job
data$k_sasian <- ifelse(data$k_racel>0&data$k_racel ==9 & 10 &11,1,0) #south asian
data$k_carr <- ifelse(data$k_racel>0&data$k_racel ==14,1,0) #carribean
data$k_afr <- ifelse(data$k_racel>0&data$k_racel ==15,1,0) #african
data$k_upset <-ifelse(data$k_upset>0&data$k_upset>1,1,0) #dont turn to mum when upset = not close with mum 
data$k_bensta2 <- ifelse(data$k_bensta2>0&data$k_bensta2==1,1,0) #educ grant
data$k_nchild_dv2 <- ifelse(data$k_nchild_dv>0, data$k_nchild_dv^2,0) #number of children sqr
data$k_scsf1 <- ifelse(data$k_scsf1>3, 1,0) #health is fair or poor

# Identify variables with more than 50% missing values
data[data < 0] <- NA
missing_prop <- colMeans(is.na(data))
vars_to_remove <- names(missing_prop[missing_prop > 0.5])
# Remove variables with more than 50% missing values
data <- data[, !names(data) %in% vars_to_remove]

#code everything as categorical
missing_prop <- colMeans(is.na(data))
vars_to_remove <- names(missing_prop[missing_prop >0.5])
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
data %<>% filter(k_basrate>1.816452& k_basrate<3.239934)

save(data, file = "kkdata.Rda")


# linear regression --------

OLS1<-lm(k_basrate ~ female, data)
summary(OLS1)

# run OLS regression with all
OLS2<-lm(k_basrate ~ female + ., data)
summary(OLS2)

#conventional controls
OLS3<-lm(k_basrate ~ female*(k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                               k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                               k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                               k_jbnssec8_dvrout+covid), data)
summary(OLS3)

#unconv controls 
ols4<-lm(k_basrate ~ female*(k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                               k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                               k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                               k_jbnssec8_dvrout + k_urban_dv+k_gor_dv+k_scghqh+k_ivlitrans+k_mhealthtypn1+k_hconda37+k_hconda372+k_ftquals+k_feend+
                               k_feend2+k_hedlik+k_qfhigh_dvd+k_qfhigh_dva+k_qfhigh_dvn+k_ladopt+k_ladopt2+k_jspl+k_jsttwtb+k_jsttwtb2+k_fivealcdr +
                               k_ypnetcht+k_scsf7+k_yr2uk4+k_yr2uk42+k_eumem+k_ncrr8+k_smoker+k_nxtendreas3+k_sasian+k_carr+k_afr+k_upset+k_bensta2+k_nchild_dv2+k_scsf1+covid) + 
           (k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
              k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
              k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
              k_jbnssec8_dvrout + k_urban_dv+k_gor_dv+k_scghqh+k_ivlitrans+k_mhealthtypn1+k_hconda37+k_hconda372+k_ftquals+k_feend+
              k_feend2+k_hedlik+k_qfhigh_dvd+k_qfhigh_dva+k_qfhigh_dvn+k_ladopt+k_ladopt2+k_jspl+k_jsttwtb+k_jsttwtb2+k_fivealcdr +
              k_ypnetcht+k_scsf7+k_yr2uk4+k_yr2uk42+k_eumem+k_ncrr8+k_smoker+k_nxtendreas3+k_sasian+k_carr+k_afr+k_upset+k_bensta2+k_nchild_dv2+k_scsf1+covid)^2, data)
summary(ols4)

#hdm lasso ----- 
#conventional controls
con = model.matrix(~-1 + female*(k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                                   k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                   k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                   k_jbnssec8_dvrout+covid), data = data)
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
uncon = model.matrix(~-1 + female*(k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                                     k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                     k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                     k_jbnssec8_dvrout + k_urban_dv+k_gor_dv+k_scghqh+k_ivlitrans+k_mhealthtypn1+k_hconda37+k_hconda372+k_ftquals+k_feend+
                                     k_feend2+k_hedlik+k_qfhigh_dvd+k_qfhigh_dva+k_qfhigh_dvn+k_ladopt+k_ladopt2+k_jspl+k_jsttwtb+k_jsttwtb2+k_fivealcdr +
                                     k_ypnetcht+k_scsf7+k_yr2uk4+k_yr2uk42+k_eumem+k_ncrr8+k_smoker+k_nxtendreas3+k_sasian+k_carr+k_afr+k_upset+k_bensta2+k_nchild_dv2+k_scsf1+covid) + 
                       (k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                          k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                          k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                          k_jbnssec8_dvrout + k_urban_dv+k_gor_dv+k_scghqh+k_ivlitrans+k_mhealthtypn1+k_hconda37+k_hconda372+k_ftquals+k_feend+
                          k_feend2+k_hedlik+k_qfhigh_dvd+k_qfhigh_dva+k_qfhigh_dvn+k_ladopt+k_ladopt2+k_jspl+k_jsttwtb+k_jsttwtb2+k_fivealcdr +
                          k_ypnetcht+k_scsf7+k_yr2uk4+k_yr2uk42+k_eumem+k_ncrr8+k_smoker+k_nxtendreas3+k_sasian+k_carr+k_afr+k_upset+k_bensta2+k_nchild_dv2+k_scsf1+covid)^2, data = data) #controls 
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
uncon = model.matrix(~-1 + female+(k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                                     k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                     k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                     k_jbnssec8_dvrout + k_urban_dv+k_gor_dv+k_scghqh+k_ivlitrans+k_mhealthtypn1+k_hconda37+k_hconda372+k_ftquals+k_feend+
                                     k_feend2+k_hedlik+k_qfhigh_dvd+k_qfhigh_dva+k_qfhigh_dvn+k_ladopt+k_ladopt2+k_jspl+k_jsttwtb+k_jsttwtb2+k_fivealcdr +
                                     k_ypnetcht+k_scsf7+k_yr2uk4+k_yr2uk42+k_eumem+k_ncrr8+k_smoker+k_nxtendreas3+k_sasian+k_carr+k_afr+k_upset+k_bensta2+k_nchild_dv2+k_scsf1+covid),
                     data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587

set.seed(1)
#dat <- data #%>% slice_sample(n=500)
data %<>% relocate(k_basrate,female)
#data=data[ , which(apply(data, 2, var) != 0)] #remove zv col
y=as.matrix(data[,1]) #wage
d=as.matrix(data[,2]) #female

#data %<>% mutate_all(as.numeric)
x=uncon[,2:44]

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
#dev.off()
par(mar = c(1, 1, 1, 1))
biplot(xpc_conf,scale=0)

par(mfrow=c(1,1))
screeplot(xpc_conf,npcs=length(xpc_conf$sdev),type="l")

plot(xpc_varexpl,xlab="Principal Component",ylab="Proportion of Variance Explained", 
     type="b")

summary(lm(y~d+x))
nfact=43
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
numfact=which.min(fact_aicc)
print(numfact)

factprint=cbind(fact_coef,fact_se,fact_aic,fact_aicc)
factprint
factprint[numfact,1:2]

#sensitivity analysis -----
OLS1<-lm(k_paynu_dv ~ female, data)
summary(OLS1)
#conventional 
con = model.matrix(~-1 + female*(k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                                   k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                   k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                   k_jbnssec8_dvrout+covid), data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE))
dim(con)
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$k_paynu_dv
d <- data$female

ylasso22 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso22)

#unconventional controls 
uncon = model.matrix(~-1 + female*(k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                                     k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                                     k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                                     k_jbnssec8_dvrout + k_urban_dv+k_gor_dv+k_scghqh+k_ivlitrans+k_mhealthtypn1+k_hconda37+k_hconda372+k_ftquals+k_feend+
                                     k_feend2+k_hedlik+k_qfhigh_dvd+k_qfhigh_dva+k_qfhigh_dvn+k_ladopt+k_ladopt2+k_jspl+k_jsttwtb+k_jsttwtb2+k_fivealcdr +
                                     k_ypnetcht+k_scsf7+k_yr2uk4+k_yr2uk42+k_eumem+k_ncrr8+k_smoker+k_nxtendreas3+k_sasian+k_carr+k_afr+k_upset+k_bensta2+k_nchild_dv2+k_scsf1+covid) + 
                       (k_nchild_dv1 + k_ukborn + k_dvage + k_dvage2 + 
                          k_hiqual_dv + k_jbstatp + k_jbstatf + k_jbsizes  + k_jbsizel + 
                          k_tujbpl + k_marstat_dv + k_jbnssec8_dvhiman + k_jbnssec8_dvhiprof + k_jbnssec8_dvlowma +
                          k_jbnssec8_dvrout + k_urban_dv+k_gor_dv+k_scghqh+k_ivlitrans+k_mhealthtypn1+k_hconda37+k_hconda372+k_ftquals+k_feend+
                          k_feend2+k_hedlik+k_qfhigh_dvd+k_qfhigh_dva+k_qfhigh_dvn+k_ladopt+k_ladopt2+k_jspl+k_jsttwtb+k_jsttwtb2+k_fivealcdr +
                          k_ypnetcht+k_scsf7+k_yr2uk4+k_yr2uk42+k_eumem+k_ncrr8+k_smoker+k_nxtendreas3+k_sasian+k_carr+k_afr+k_upset+k_bensta2+k_nchild_dv2+k_scsf1+covid)^2, data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(uncon)
index.female <- grep("female", colnames(uncon))

x=uncon[,-1] # covariates
y <- data$k_paynu_dv
d <- data$female

ylasso22 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso22)


#2020/21 l survey ----
####-----
rm(list=ls())
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")


##### read data --------
#uncomment if first time
#data <- read_dta("l_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "ldata.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/ldata.Rda")
#####
data %<>% filter(l_ivfio == 1 & l_ioutcome == 11 & l_basrate > 0  & l_paynu_dv>0 & l_dvage>0) 
data %<>% select(-starts_with(c("l_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("l_ind")))
data <- data[,-(2:12)]

#conventional dummies
data$l_nchild_dv1 <-ifelse(data$l_nchild_dv>0,1,0) #have children
data$l_ukborn <- ifelse(data$l_ukborn>0&data$l_ukborn==5,1,0)#not born in uk
data$l_dvage <- ifelse(data$l_dvage>0,data$l_dvage,0) #age 
data$l_dvage2 <- ifelse(data$l_dvage, data$l_dvage^2,0) #age quad
data$l_hiqual_dv = ifelse(data$l_hiqual_dv>0&data$l_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$l_jbstatp = ifelse(data$l_jbstat>0&data$l_jbstat==2, 1,0) #in ft or pt emp
data$l_jbstatf = ifelse(data$l_jbstat>0&data$l_jbendreas5==1, 1,0) #temp job ended,, no furlough var here so nearest
data$l_jbsizes = ifelse(data$l_jbsize>0&data$l_jbsize==1&2&3&4, 1,0) #small firm 
data$l_jbsizel = ifelse(data$l_jbsize>0&data$l_jbsize==8&9, 1,0) #large firm
data$l_tujbpl = ifelse(data$l_tujbpl>0&data$l_tujbpl==1, 1,0) #in a union
data$l_marstat_dv = ifelse(data$l_marstat_dv>0&data$l_marstat_dv==2, 1,0) #married
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
data$female <- ifelse(data$l_sex == 2, 1, 0) #female
data$l_basrate <- log(data$l_basrate) #log pay var
data$l_paynu_dv <- log(data$l_paynu_dv) #log alt pay var
data$date <- paste(data$l_istrtdatd, data$l_istrtdatm, data$l_istrtdaty, sep = "/")
data$date <- dmy(data$date)
data$covid<- ifelse(data$date>"2020-03-23", 1,0)
data = select(data, -1,-17:-23, ) #remove id var
data %<>% select(-(c("l_jbnssec8_dv", "l_sex","l_sex_dv"))) #remove highly related vars
data %<>% select(-contains(c("l_jbnssec3","l_jbnssec5", "jbsoc00_cc"))) #ditto

#unconventional dummies  
data$l_urban_dv <- ifelse(data$l_urban_dv>0&data$l_urban_dv==1, 1, 0) #urban
data$l_gor_dv <- ifelse(data$l_gor_dv>0&data$l_gor_dv==7&8, 1, 0) #ldn + SE
data$l_scghqh <- ifelse(data$l_scghqh>0&data$l_scghqh==3&4,1,0) #ability to face problems less than usual
data$l_ivlitrans <- ifelse(data$l_ivlitrans>0&data$l_ivlitrans != 0 & data$l_ivlitrans != 9, 1, 0) #interview language wasnt english or welsh
data$l_mhealthtypn1 <- ifelse(data$l_mhealthtypn1>0&data$l_mhealthtypn1==1,1,0) #has anxiety 
data$l_hconda37 <- ifelse(data$l_hconda37>0,data$l_hconda37,0) #know age had anxiety at
data$l_hconda372 <- ifelse(data$l_hconda37>0, data$l_hconda37^2,0) #^ sqrd
data$l_ftquals <- ifelse(data$l_ftquals>0&data$l_ftquals==1,1,0) #recent educ qual
data$l_feend <- ifelse(data$l_feend>0, data$l_feend,0) #age left educ
data$l_feend2 <- ifelse(data$l_feend>0, data$l_feend^2,0) #left educ sqr
data$l_hedlik <- ifelse(data$l_hedlik>0&data$l_hedlik==3&4&5,1,0) #feels they were not likely to purs FE
data$l_qfhigh_dvd <- ifelse(data$l_qfhigh_dv>0&data$l_qfhigh_dv == 1&2&3&4&5&6, 1,0) #degree educated
data$l_qfhigh_dva <- ifelse(data$l_qfhigh_dv>0&data$l_qfhigh_dv == 7&8&9&10&11, 1,0) #a level educatrd
data$l_qfhigh_dvn <- ifelse(data$l_qfhigh_dv>0&data$l_qfhigh_dv == 96, 1,0) #no formal quals
data$l_ladopt <- ifelse(data$l_ladopt>0, data$l_lprnt,0) #has adopted
data$l_ladopt2 <- ifelse(data$l_ladopt>0, data$l_lprnt^2,0) #has adopted sqr
data$l_jspl <-  ifelse(data$l_lprnt>2, 1,0) #dont work at home 
data$l_jsttwtb <- ifelse(data$l_jsttwtb>0, data$l_jsttwtb,0) #time to work
data$l_jsttwtb2 <- ifelse(data$l_jsttwtb>0, data$l_jsttwtb^2,0) #time to work sq
data$l_fivealcdr <-  ifelse(data$l_fivealcdr>3, 1,0) #2 or more 5 alc drink in last 4 wk
data$l_ypnetcht <-  ifelse(data$l_ypnetcht>=3, 1,0) #more than 1 hour social media
data$l_scsf7 <-  ifelse(data$l_scsf7>0&data$l_scsf7==1&2&3, 1,0) #p/m health hurt social life
data$l_yr2uk4 <-  ifelse(data$l_yr2uk4>0, data$l_yr2uk4,0) #year came to uk
data$l_yr2uk42 <-  ifelse(data$l_yr2uk4>0, data$l_yr2uk4^2,0) #year came to uk sqr 
data$l_eumem <-  ifelse(data$l_eumem>0&data$l_eumem==2, 1,0) #leave eu
data$l_ncrr8 <-  ifelse(data$l_ncrr8>1, 1,0) #partner live far away
data$l_smoker <- ifelse(data$l_smoker>0&data$l_smoker==1,1,0) #smokes
data$l_nxtendreas3 <-ifelse(data$l_nxtendreas3>0&data$l_nxtendreas3==1 & data$l_nxtendreas4 ==1 & 
                              data$l_nxtendreas5 == 1& data$l_nxtendreas7==1, 1,0) # made redundant,sacked, temp job loss or for health reason left job
data$l_sasian <- ifelse(data$l_racel>0&data$l_racel ==9 & 10 &11,1,0) #south asian
data$l_carr <- ifelse(data$l_racel>0&data$l_racel ==14,1,0) #carribean
data$l_afr <- ifelse(data$l_racel>0&data$l_racel ==15,1,0) #african
data$l_upset <-ifelse(data$l_upset>0&data$l_upset>1,1,0) #dont turn to mum when upset = not close with mum 
data$l_bensta2 <- ifelse(data$l_bensta2>0&data$l_bensta2==1,1,0) #educ grant
data$l_nchild_dv2 <- ifelse(data$l_nchild_dv>0, data$l_nchild_dv^2,0) #number of children sqr
data$l_scsf1 <- ifelse(data$l_scsf1>3, 1,0) #health is fair or poor

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

save(data, file = "lldata.Rda")


# linear regression --------

OLS1<-lm(l_basrate ~ female, data)
summary(OLS1)

# run OLS regression with all
OLS2<-lm(l_basrate ~ female + ., data)
summary(OLS2)

#conventional controls
OLS3<-lm(l_basrate ~ female*(l_nchild_dv1 + l_ukborn + l_dvage + l_dvage2 + 
                               l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                               l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                               l_jbnssec8_dvrout), data)
summary(OLS3)

#unconv controls 
ols4<-lm(l_basrate ~ female*
           (l_nchild_dv1 + l_ukborn + l_dvage + l_dvage2 + 
              l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
              l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
              l_jbnssec8_dvrout + l_urban_dv+l_gor_dv+l_scghqh+l_ivlitrans+l_mhealthtypn1+l_hconda37+l_hconda372+l_ftquals+l_feend+
              l_feend2+l_hedlik+l_qfhigh_dvd+l_qfhigh_dva+l_qfhigh_dvn+l_ladopt+l_ladopt2+l_jspl+l_jsttwtb+l_jsttwtb2+l_fivealcdr +
              l_ypnetcht+l_scsf7+l_yr2uk4+l_yr2uk42+l_eumem+l_ncrr8+l_smoker+l_nxtendreas3+l_sasian+l_carr+l_afr+l_upset+l_bensta2+l_nchild_dv2+l_scsf1)^2, data)
summary(ols4)

#hdm lasso ----- 
#conventional controls
con = model.matrix(~-1 + female*(l_nchild_dv1 + l_ukborn + l_dvage + l_dvage2 + 
                                   l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                                   l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                                   l_jbnssec8_dvrout), data = data)
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
uncon = model.matrix(~-1 + female*(l_nchild_dv1 + l_ukborn + l_dvage + l_dvage2 + 
                                     l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                                     l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                                     l_jbnssec8_dvrout + l_urban_dv+l_gor_dv+l_scghqh+l_ivlitrans+l_mhealthtypn1+l_hconda37+l_hconda372+l_ftquals+l_feend+
                                     l_feend2+l_hedlik+l_qfhigh_dvd+l_qfhigh_dva+l_qfhigh_dvn+l_ladopt+l_ladopt2+l_jspl+l_jsttwtb+l_jsttwtb2+l_fivealcdr +
                                     l_ypnetcht+l_scsf7+l_yr2uk4+l_yr2uk42+l_eumem+l_ncrr8+l_smoker+l_nxtendreas3+l_sasian+l_carr+l_afr+l_upset+l_bensta2+l_nchild_dv2+l_scsf1) + 
                       (l_nchild_dv1 + l_ukborn + l_dvage + l_dvage2 + 
                          l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                          l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                          l_jbnssec8_dvrout + l_urban_dv+l_gor_dv+l_scghqh+l_ivlitrans+l_mhealthtypn1+l_hconda37+l_hconda372+l_ftquals+l_feend+
                          l_feend2+l_hedlik+l_qfhigh_dvd+l_qfhigh_dva+l_qfhigh_dvn+l_ladopt+l_ladopt2+l_jspl+l_jsttwtb+l_jsttwtb2+l_fivealcdr +
                          l_ypnetcht+l_scsf7+l_yr2uk4+l_yr2uk42+l_eumem+l_ncrr8+l_smoker+l_nxtendreas3+l_sasian+l_carr+l_afr+l_upset+l_bensta2+l_nchild_dv2+l_scsf1)^2, data = data) #controls 
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
uncon = model.matrix(~-1 + female+(l_nchild_dv1 + l_ukborn + l_dvage + l_dvage2 + 
                                     l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                                     l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                                     l_jbnssec8_dvrout + l_urban_dv+l_gor_dv+l_scghqh+l_ivlitrans+l_mhealthtypn1+l_hconda37+l_hconda372+l_ftquals+l_feend+
                                     l_feend2+l_hedlik+l_qfhigh_dvd+l_qfhigh_dva+l_qfhigh_dvn+l_ladopt+l_ladopt2+l_jspl+l_jsttwtb+l_jsttwtb2+l_fivealcdr +
                                     l_ypnetcht+l_scsf7+l_yr2uk4+l_yr2uk42+l_eumem+l_ncrr8+l_smoker+l_nxtendreas3+l_sasian+l_carr+l_afr+l_upset+l_bensta2+l_nchild_dv2+l_scsf1) ,
                     data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587

set.seed(1)
#dat <- data #%>% slice_sample(n=500)
data %<>% relocate(l_basrate,female)
#data=data[ , which(apply(data, 2, var) != 0)] #remove zv col
y=as.matrix(data[,1]) #wage
d=as.matrix(data[,2]) #female

#data %<>% mutate_all(as.numeric)
x=uncon[,2:46]

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
#dev.off()
par(mar = c(1, 1, 1, 1))
biplot(xpc_conf,scale=0)

par(mfrow=c(1,1))
screeplot(xpc_conf,npcs=length(xpc_conf$sdev),type="l")

plot(xpc_varexpl,xlab="Principal Component",ylab="Proportion of Variance Explained", 
     type="b")

summary(lm(y~d+x))
nfact=45
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
numfact=which.min(fact_aicc[1:44,1])
print(numfact)

factprint=cbind(fact_coef,fact_se,fact_aic,fact_aicc)
factprint
factprint[44,1:2]

#sensitivity analysis -----
OLS1<-lm(l_paynu_dv ~ female, data)
summary(OLS1)
#conventional + 
con = model.matrix(~-1 + female+(l_nchild_dv1 + l_ukborn + l_dvage + l_dvage2 + 
                                   l_hiqual_dv + l_jbstatp + l_jbstatf + l_jbsizes  + l_jbsizel + 
                                   l_tujbpl + l_marstat_dv + l_jbnssec8_dvhiman + l_jbnssec8_dvhiprof + l_jbnssec8_dvlowma +
                                   l_jbnssec8_dvrout), data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE))
x=con[,-1] 
y <- data$l_paynu_dv
d <- data$female

ylasso11 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso11)

#unconventional controls only +
uncon = model.matrix(~-1 + female+(l_urban_dv+l_gor_dv+l_scghqh+l_ivlitrans+l_mhealthtypn1+l_hconda37+l_hconda372+l_ftquals+l_feend+
                                     l_feend2+l_hedlik+l_qfhigh_dvd+l_qfhigh_dva+l_qfhigh_dvn+l_ladopt+l_ladopt2+l_jspl+l_jsttwtb+l_jsttwtb2+l_fivealcdr +
                                     l_ypnetcht+l_scsf7+l_yr2uk4+l_yr2uk42+l_eumem+l_ncrr8+l_smoker+l_nxtendreas3+l_sasian+l_carr+l_afr+l_upset+l_bensta2+l_nchild_dv2+l_scsf1)^2, data = data) #controls 
uncon <- uncon[, which(apply(uncon, 2, var) != 0)]
uncon = apply(uncon, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
x=uncon[,-1] # covariates

y <- data$l_paynu_dv
d <- data$female

ylasso22 <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso22)