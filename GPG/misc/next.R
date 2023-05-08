####-----
#2020/21
rm(list=ls())

##### read data --------
#uncomment if first time
#data <- read_dta("l_indresp.dta")
#data <- zap_labels(data)
#data <- as.data.frame(data)
#save(data, file = "ldata.Rda")
load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/ldata.Rda")
#load("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls/mean_by_pidp.rda")
#data %<>% select(-l_basrate) %>% left_join(mean_by_pidp, by = "pidp")
#####
data %<>% filter(l_ivfio == 1 & l_ioutcome == 11 & l_basrate > 0 & l_dvage > 0 & j_employ ==1) #3000/30k # &l_hiqual_dv>0& l_jbstat>0& l_jbnssec8_dv>0 & l_scsf1>0 & l_jbsize>0 &l_tujbpl>0&l_marstat_dv>0
data %<>% select(-starts_with(c("l_ovtr")))
names(data)[names(data) == "pidp"] <- "individual"
data %<>% select(-contains(c("pid")))
data %<>% select(-starts_with(c("l_ind")))
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

quantile(data$l_basrate, 0.01)
quantile(data$l_basrate, 0.99)
data %<>% filter(l_basrate>4.6064 & l_basrate<33)

#exclude_cols <- c("l_dvage", "l_basrate", "l_birthy", "l_ioutcome")
# Convert all numeric columns to factors, except for excluded columns
#data[, sapply(data, is.numeric) & !(names(data) %in% exclude_cols)] <- 
#lapply(data[, sapply(data, is.numeric) & !(names(data) %in% exclude_cols)], factor)

#dummies
data$l_dvage2 <- data$l_dvage^2
data$l_hiqual_dv = ifelse(data$l_hiqual_dv==4&5&9, 1,0) #highest qual was gcse 
data$l_jbstatp = ifelse(data$l_jbstat==2, 1,0) #paid - ft/pt emp
data$l_jbstatf = ifelse(data$l_jbstat==12&13, 1,0)
#data$jbterm1 = ifelse(data$jbterm1==1, 1,0) #perm
data$l_jbsizes = ifelse(data$l_jbsize==1&2&3&4, 1,0) #small
data$l_jbsizem = ifelse(data$l_jbsize==5&6&7, 1,0) #med
data$l_jbsizel = ifelse(data$l_jbsize==8&9, 1,0) #large
data$l_tujbpl = ifelse(data$l_tujbpl==1, 1,0) #union
data$l_marstat_dv = ifelse(data$l_marstat_dv==2, 1,0) #married
data <- mutate(data,
               l_jbnssec8_dvman = case_when( 
                 l_jbnssec8_dv == 1 ~ 1,
                 TRUE ~ 0
               ),
               l_jbnssec8_dvint = case_when(
                 l_jbnssec8_dv == 2 ~ 1,
                 TRUE ~ 0
               ),
               l_jbnssec8_dvsma = case_when(
                 l_jbnssec8_dv == 3 ~ 1,
                 TRUE ~ 0
               ),
               l_jbnssec8_dvlow = case_when(
                 l_jbnssec8_dv == 4 ~ 1,
                 TRUE ~ 0
               ),
               l_jbnssec8_dvrou = case_when(
                 l_jbnssec8_dv == 5 ~ 1,
                 TRUE ~ 0
               )
)
data$female <- ifelse(data$l_sex == 2, 1, 0)
data$l_basrate <- log(data$l_basrate)
data = select(data, -1,-17:-23, )
data %<>% select(-(c("l_jbnssec8_dv", "l_sex","l_sex_dv")))
data %<>% select(-contains(c("l_jbnssec3","l_jbnssec5", "jbsoc00_cc")))
ggplot(data, aes(x=l_basrate, colour=as.factor(female)))+geom_density() +theme_classic()
set.seed(12345)
con = model.matrix(~-1 + female*(l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                                   l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                                   l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow) + 
                     (l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                        l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                        l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)^2, data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587

#unconventional dummies 


#table ----
Z <- data %>% select(c("l_basrate","female", "l_dvage","l_dvage2","l_nchild_dv","l_hiqual_dv","l_jbstat",
                       "l_jbsizes","l_jbsizel","l_tujbpl","l_marstat_dv","l_jbnssec8_dvman",
                       "l_jbnssec8_dvint", "l_jbnssec8_dvsma", "l_jbnssec8_dvlow"))
ZMale <- Z %>% filter(female == 0)
ZFemale<- Z %>% filter(female == 1)

Z %>% split(. $female) %>% walk(~ stargazer(., type = "latex"))
# linear regression --------

OLS1<-lm(l_basrate ~female, data)
summary(OLS1)

# run OLS regression with somecontrols
OLS3<-lm(l_basrate ~ con, data)
summary(OLS3)

#conventional controls
OLS2<-lm(l_basrate ~ female*(l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                               l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                               l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow), data)
summary(OLS2)


#hdm lasso ----- 
### unrestricted

con = model.matrix(~-1 + female+(.) , data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$l_basrate
d <- data$female

one<- lm(y ~ con, data)
summary(one)

#effects.female <- rlassoEffects(x = con, y = y, index = index.female)
#summary(effects.female)

ylasso1 <- rlassoEffect(x=x,y=y,d=d,index = index.female, method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,index = index.female,method="double selection")
summary(ylasso)



#conventional controls

con = model.matrix(~-1 + female + female : (l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                                              l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                                              l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow),
                   data = data)
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
index.female <- grep("female", colnames(con))
x=con[,-1] 
y <- data$l_basrate
d <- data$female

one<- lm(y ~ con, data)
summary(one)

#effects.female <- rlassoEffects(x = con, y = y, index = index.female)
#summary(effects.female)


ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)


#unconventional controls 2 way
con = model.matrix(~-1 + female + female:(l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                                   l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                                   l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)+ 
                     (l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                        l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                        l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)^2,
                   data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(con)
index.gender <- grep("female", colnames(con))
y <- data$l_basrate
effects.female <- rlassoEffects(x = con, y = y, index = index.gender)
summary(effects.female)
one<- lm(l_basrate ~ con, data)
summary(one)
x=con[,-1] # covariates

y <- data$l_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)

#conventional controls 2 way
con = model.matrix(~-1 + female + female:(l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                                            l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                                            l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)+ 
                     (l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + l_jbstat + 
                        l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                        l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)^2,
                   data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
dim(con)
index.gender <- grep("female", colnames(con))
y <- data$l_basrate
effects.female <- rlassoEffects(x = con, y = y, index = index.gender)
summary(effects.female)
one<- lm(l_basrate ~ con, data)
summary(one)
x=con[,-1] # covariates

y <- data$l_basrate
d <- data$female

ylasso1 <- rlassoEffect(x=x,y=y,d=d,method="partialling out")
summary(ylasso1)
ylasso <- rlassoEffect(x=x,y=y,d=d,method="double selection")
summary(ylasso)




##########

plot(effects.female, oldNames = TRUE, joint=TRUE, level=0.95)
joint.CI <- confint(effects.female, level = 0.95, joint = TRUE, las = 2)
joint.CI

#logit regression ----
con = model.matrix(~-1 + female + female:(l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv  + 
                                            l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                                            l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)+ 
                     (l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + 
                        l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                        l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)^2,
                   data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
mylogit <- glm(l_jbstatf ~ con, data = data, family = "binomial")
summary(mylogit)

#lasso regression of furlough ----
con = model.matrix(~-1 + female + female:(l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv  + 
                                            l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                                            l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)+ 
                     (l_dvage + l_dvage2 + l_nchild_dv + l_hiqual_dv + 
                        l_jbsizes + l_jbsizel + l_tujbpl + l_marstat_dv + 
                        l_jbnssec8_dvman + l_jbnssec8_dvint + l_jbnssec8_dvsma + l_jbnssec8_dvlow)^2,
                   data = data) #controls 
con <- con[, which(apply(con, 2, var) != 0)]
con = apply(con, 2, function(x) scale(x, center = TRUE, scale = FALSE)) #587
mylogit <- rlasso(l_jbstatf ~ con, data = data)
summary(mylogit)



# PCA ------

n=nrow(data)
Index <- 1:n

set.seed(1)
dat <- data %>% slice_sample(n=1000)
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

