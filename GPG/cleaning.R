rm(list=ls())

library(haven)
library(dplyr)
library(labelled)
library(ggplot2)

#data wrangling
###############
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")


file_names <- list.files('C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls',
                         pattern = 'indresp.dta')
# loop through the file names and read them in with read_dta, create a new variable for each data frame
for (i in seq_along(file_names)) {
  assign(paste0("data", i), read_dta(file_names[i]))}


#look_for(data1, "survey")
#sum(data1$a_ioutcome %in% c("11","13"))

data1<-data1 %>% filter(a_ivfio %in% c(1))  # difff
data1<-data1 %>%select(pidp,a_sex,a_dvage,a_basrate, a_ovtrate, a_fimnlabgrs_dv, a_istrtdaty,a_racel,a_jbstat,a_oprlg,a_jbft_dv)

data2<-data2 %>% filter(b_ivfio %in% c(1))  #diiffff
data2<-data2 %>%select(pidp,b_sex,b_dvage,b_basrate, b_ovtrate, b_fimnlabgrs_dv, b_istrtdaty,b_racel,b_jbstat,b_oprlg,b_jbft_dv)

data3<-data3 %>% filter(c_ivfio %in% c(1)) %>% 
  select(pidp,c_sex,c_dvage,c_basrate, c_ovtrate, c_fimnlabgrs_dv,c_istrtdaty,c_racel,c_jbstat,c_oprlg,c_jbft_dv)

data4<-data4 %>% filter(d_ivfio %in% c(1)) %>% 
  select(pidp,d_sex,d_dvage,d_basrate, d_ovtrate, d_fimnlabgrs_dv,d_istrtdaty,d_racel,d_jbstat,d_oprlg,d_jbft_dv)

data5<-data5 %>% filter(e_ivfio %in% c(1)) %>% 
  select(pidp,e_sex,e_dvage,e_basrate, e_ovtrate, e_fimnlabgrs_dv,e_istrtdaty,e_racel,e_jbstat,e_oprlg,e_jbft_dv)

data6<-data6 %>% filter(f_ivfio %in% c(1)) %>% 
  select(pidp,f_sex,f_dvage,f_basrate, f_ovtrate, f_fimnlabgrs_dv,f_istrtdaty,f_racel,f_jbstat,f_oprlg,f_jbft_dv)

data7<-data7 %>% filter(g_ivfio %in% c(1)) %>% 
  select(pidp,g_sex,g_dvage,g_basrate, g_ovtrate, g_fimnlabgrs_dv,g_istrtdaty,g_racel,g_jbstat,g_oprlg,g_jbft_dv)

data8<-data8 %>% filter(h_ivfio %in% c(1)) %>% 
  select(pidp,h_sex,h_dvage,h_basrate, h_ovtrate, h_fimnlabgrs_dv,h_istrtdaty,h_racel,h_jbstat,h_oprlg,h_jbft_dv)

data9<-data9 %>% filter(i_ivfio %in% c(1)) %>% 
  select(pidp,i_sex,i_dvage,i_basrate, i_ovtrate, i_fimnlabgrs_dv,i_istrtdaty,i_racel,i_jbstat,i_oprlg,i_jbft_dv)

data10<-data10 %>% filter(j_ivfio %in% c(1)) %>% 
  select(pidp,j_sex,j_dvage,j_basrate, j_ovtrate, j_fimnlabgrs_dv,j_istrtdaty,j_racel,j_jbstat,j_oprlg,j_jbft_dv)

data11<-data11 %>% filter(k_ivfio %in% c(1)) %>% 
  select(pidp,k_sex,k_dvage,k_basrate, k_ovtrate, k_fimnlabgrs_dv,k_istrtdaty,k_racel,k_jbstat,k_oprlg,k_jbft_dv)

data12<-data12 %>% filter(l_ivfio %in% c(1)) %>% 
  select(pidp,l_sex,l_dvage,l_basrate, l_ovtrate, l_fimnlabgrs_dv,l_istrtdaty,l_racel,l_jbstat,l_oprlg,l_jbft_dv)


# Create a list of data frames
data_list <- list(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12)
# Define the new column names you want to use
new_names <- c("pidp", "sex", "dvage", "basrate", "ovtrate", "fimnlabgrs_dv", "istrtdaty", "racel", "jbstat", "oprlg", "jbft_dv")
# Loop through each data frame in the list
for (i in seq_along(data_list)) {
  # Rename the columns using the new names
  colnames(data_list[[i]]) <- new_names
}
combined_data <- bind_rows(data_list)
combined_data <- combined_data %>% filter(basrate >0)
#&ovtrate>0 & fimnlabgrs_dv >0)

#save(combined_data, file = "combined_data.RData")

#data1$pay1 <- data1$a_basrate #basic pay hourly rate
#data1$pay2 <- data1$a_ovtrate #overtime pay hourly rate 
#data1$pay3 <- data1$a_fimnlabgrs_dv #total monthly personal income gross 
#data1 <- data1 %>% filter(pay1>=0 & pay2>=0 & pay3>=0)
#data1$pay1 <- as.integer(data1$pay1[data1$pay1 >= 0])
#data1$pay2 <- as.integer(data1$pay2[data1$pay2 >= 0])

istrtdaty <- years
sc<- c(91.3, 92.6, 94.6, 96,98,99.3,100,101.9,103.7,105.5,107.7,114.1)
deflator<- data.frame(istrtdaty,sc)
save(deflator, file = "deflator.RData")
#################


rm(list=ls())
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")
load("combined_data.RData")
#load('reftable.RData')
load('Deflator.RData')
library(janitor)
library(labelled)
library(dplyr)
library(haven)
library(ggplot2)
library(generics)
#############
combined_data$sex <- as.numeric(unclass(combined_data$sex))
combined_data$dvage <- as.numeric(unclass(combined_data$dvage))
combined_data$basrate <- as.numeric(unclass(combined_data$basrate))
combined_data$ovtrate <- as.numeric(unclass(combined_data$ovtrate))
combined_data$fimnlabgrs_dv <- as.numeric(unclass(combined_data$fimnlabgrs_dv))
combined_data$istrtdaty <- as.numeric(unclass(combined_data$istrtdaty))
combined_data$racel <- as.numeric(unclass(combined_data$racel))
combined_data$jbstat <- as.numeric(unclass(combined_data$jbstat))
combined_data$oprlg <- as.numeric(unclass(combined_data$oprlg))
combined_data$jbft_dv <- as.numeric(unclass(combined_data$jbft_dv))
#########

#dv age unavial)  & racel >=0&oprlg >=0
combined_data1 <-combined_data %>%  filter(dvage >=0 & istrtdaty >=0 &jbstat>=0 & sex %in% c("1","2") & basrate>0)
#                                           & jbstat == 2 & jbft_dv %in% c("1","2"))
#summary(combined_data1[-1])
#class(combined_data1$sex)
#attributes(combined_data1)
combined_data.1 <- combined_data1 %>% filter(istrtdaty<2022)

mean_income_sex1 <- aggregate(basrate ~ istrtdaty + sex, data = combined_data.1 %>% filter(jbft_dv == 1), median) #PT
mean_income_sex11 <- aggregate(basrate ~ istrtdaty + sex, data = combined_data.1 %>% filter(jbft_dv == 2), median) #FT
#mean_income_sex2 <- aggregate(pay2 ~ istrtdaty + sex, data = combined_data.1, mean)
#mean_income_sex3 <- aggregate(pay3 ~ istrtdaty + sex, data = combined_data.1, mean)
#mean_income_sex1$lnpay1<-log(mean_income_sex1$pay1)
mean_income_sex1$ratio <- mean_income_sex1$basrate[mean_income_sex1$sex==1]/mean_income_sex1$basrate[mean_income_sex1$sex==2]
mean_income_sex11$ratio <- mean_income_sex11$basrate[mean_income_sex11$sex==1]/mean_income_sex11$basrate[mean_income_sex11$sex==2]
scale = 14
ggplot(mean_income_sex1, aes(x = istrtdaty, y = basrate, group = sex, color = factor(sex))) + 
  geom_line() + 
  geom_line(aes(y = ratio*scale, color = "M/F Ratio")) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="M/F Ratio"))

  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 2017, linetype = 'dashed')+
  scale_color_manual(values = c("black", "blue", "red"), labels = c("Male", "Female", "Ratio")) +
  labs(title = "Median Gross Hourly Income (FT) by Year and Sex", x = "Year", y = "Mean Income") +
  theme_bw()

ggplot(mean_income_sex11, aes(x = istrtdaty, y = basrate, group = sex, color = factor(sex))) + 
  geom_line() + 
  geom_line(aes(y = ratio*scale, color = "M/F Ratio")) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="M/F Ratio"))

+ scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 2017, linetype = 'dashed')+
  scale_color_manual(values = c("black", "blue"), labels = c("Male", "Female")) +
  labs(title = "Median Gross Hourly Income (PT) by Year and Sex", x = "Year", y = "Mean Income") +
  theme_bw()


combined_data.1 <- combined_data1 %>% filter(jbft_dv == 1)
years = unique(combined_data.1$istrtdaty)
meandiffs <- numeric(length(years))
for (i in years) {
  meandiffs[i-2008] <- median(combined_data.1$basrate[combined_data.1$sex == 1 & combined_data.1$istrtdaty == i]) - median(combined_data.1$basrate[combined_data.1$sex == 2  & combined_data.1$istrtdaty == i])
}
#mean(combined_data.1$basrate[combined_data.1$sex == 2 & combined_data.1$istrtdaty == 2009]) - mean(combined_data.1$basrate[combined_data.1$sex == 1  & combined_data.1$istrtdaty == 2009])
meandiffs1<- data.frame(istrtdaty = years, meandiffs = meandiffs)

#creating data 
mean_income_sex1 <- left_join(mean_income_sex1,meandiffs1, by="istrtdaty")  
mean_income_sex1<-left_join(mean_income_sex1, deflator, by="istrtdaty")
mean_income_sex1 <- mean_income_sex1 %>%  mutate(Real=meandiffs/(sc/100))


ggplot(mean_income_sex1, aes(x = istrtdaty, y =abs(Real))) + 
  geom_line() + geom_smooth() + scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 2017, linetype = 'dashed')+
  scale_color_manual(values = c("black", "blue"), labels = c("Male", "Female")) +
  labs(title = "Median Gender Pay Differential", x = "Year", y = "Difference in Mean Income") +
  theme_minimal()

## Estimate a model on a training sample of the data
ols1_train = lm(Real ~ istrtdaty, data = mean_income_sex1 %>% filter(istrtdaty<=2016))

## Note that I'm including a 95% prediction interval. See ?predict.lm for other
## intervals and options.
mean_income_sex1 = augment(ols1_train, newdata = mean_income_sex1, interval = "prediction")

library(broom)

mean_income_sex1 %>%
  ggplot(aes(x = istrtdaty, y = Real, col = (istrtdaty<=2016), fill = (istrtdaty<=2016))) +
  geom_point(alpha = 0.7) +
  geom_line(aes(y = .fitted)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.3, col = NA) +
  scale_color_discrete(name = "Training sample?", aesthetics = c("colour", "fill")) +
  labs(
    title = "Real median Gender Differential (naive)",
    caption = "Line of best fit, with shaded regions denoting 95% prediction interval."
  )

#do prediction, then look at residuals for which women 
#have to log wages
#what factors do mena dn women differ most in? PCA
############
################### is female
mean_income_sex1 <- mean_income_sex1 %>% mutate(Real = ifelse(sex==2, -abs(Real),0))
mean_income_sex1<-mean_income_sex1 %>% 
  mutate(treat = (sex == 2),
         post = (istrtdaty >= 2017),
         treatpost = treat*post)
dd<-lm(Real~treat+post+treatpost, data =mean_income_sex1)
library(stargazer)
stargazer(dd, type = 'text')
#######################
# so 

newdata1<-left_join(combined_data.1, mean_income_sex1, c("istrtdaty","sex"))

ols1_train = lm(basrate.x ~ istrtdaty, data = newdata1 %>% filter(istrtdaty<=2017 & sex == 2 & Real < 0 & basrate.x < 100))
newdata1 = augment(ols1_train, newdata = newdata1, interval = "prediction")
library(broom)















#model selection etc

library(stargazer)
##oprlg need fix
reg_pay_all_control <- newdata1$basrate.x ~ newdata1$sex + newdata1$dvage + newdata1$ovtrate +
  newdata1$Real+ newdata1$fimnlabgrs_dv + newdata1$istrtdaty +newdata1$racel+newdata1$jbstat+newdata1$jbft_dv
ols <- lm(reg_pay_all_control)
stargazer(ols, type = 'text')

OLS_bss = regsubsets(reg_pay_all_control, data = NULL, nvmax = 14)
summary(OLS_bss)

ols_bss_sum = summary(OLS_bss)

names(ols_bss_sum)
while (!is.null(dev.list()))  dev.off()
par(mfrow=c(4,1), oma=c(0, 0, 0, 0))
par(mar=c(5, 5, 2, 2))
plot(ols_bss_sum$rss,xlab="Number of variables",ylab="RSS",type="l", main="RSS vs Number of variables")
plot(ols_bss_sum$adjr2,xlab="Number of variables",ylab="Adjusted R-squared",type="l", main="Adjusted R-squared vs Number of variables")
plot(ols_bss_sum$bic,xlab="Number of variables",ylab="BIC",type="l", main="BIC vs Number of variables")
plot(ols_bss_sum$cp,xlab="Number of variables",ylab="CP",type="l", main="CP vs Number of variables")
par(mfrow=c(1,1))

which.min(ols_bss_sum$bic)
one<-coef(OLS_bss,which.min(ols_bss_sum$bic))
names(one)
which.max(ols_bss_sum$adjr2)
two<-coef(OLS_bss,which.max(ols_bss_sum$adjr2))
names(two)

#creating a predict function 
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  xmat=model.matrix(form,newdata)
  coefi=coef(object, id=id)
  xvars = names(coefi)
  xmat[,xvars]%*%coefi }

#validation set approach 
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(newdata1),rep=TRUE)
test=(!train)

OLS_train=regsubsets(basrate.x ~ sex + dvage +ovtrate +Real +fimnlabgrs_dv +istrtdaty+racel+jbstat+jbft_dv,
                     data=newdata1[train,],nvmax=10)

# Use the test data to select the best number of regressors
mse_validation=rep(NA,9)
library(leaps)
for (i in 1:9)
{
  pred = predict.regsubsets(OLS_train,newdata1[test,],id=i)
  mse_validation[i] = mean( (newdata1$basrate.x[test]-pred)^2)
}
mse_validation

# discover how many variables validation selects
which.min(mse_validation)
ii=which.min(mse_validation)

ols_validation = regsubsets(basrate.x ~ sex + dvage +ovtrate +Real +fimnlabgrs_dv +istrtdaty+racel+jbstat+jbft_dv,
                          data=newdata1[train,],nvmax=10)

coef(ols_validation,ii)
sqrt(diag(vcov(ols_validation,ii)))
# Rerun using all data (efficiency) to report model chosen based on best validation 
pred = predict.regsubsets(ols_validation,newdata1,id=ii)
mse_OLS_validation = mean((newdata1$basrate.x-pred)^2) 
mse_OLS_validation




#look_for(reftable, "occupation")
subset<- combined_data.1 %>% filter(istrtdaty == 2017)
subset$meandiffs <-as.numeric(rownames(subset))
mean_income_sex111<-mean_income_sex1 %>% select("sex", "istrtdaty","meandiffs")
subset1<-left_join(subset,mean_income_sex111, by c("sex","istrtdaty"))










#not best pay variable

g2<-ggplot(mean_income_sex2, aes(x = istrtdaty, y = pay2, group = sex, color = factor(sex))) +
  geom_line() + scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  geom_vline(xintercept = 2017, linetype = 'dashed') + 
  scale_color_manual(values = c("black", "blue"), labels = c("Male", "Female")) +
  labs(title = "Mean Income by Year and Sex", x = "Year", y = "Mean Income") +
  theme_minimal()
g2
g3<-ggplot(mean_income_sex3, aes(x = istrtdaty, y = pay3, group = sex, color = factor(sex))) +
  geom_line() + scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  geom_vline(xintercept = 2017, linetype = 'dashed')+
  scale_color_manual(values = c("black", "blue"), labels = c("Male", "Female")) +
  labs(title = "Mean Income by Year and Sex", x = "Year", y = "Mean Income") +
  theme_minimal()
g3

library(ggpubr)
#ggarrange(g1,g2,g3)


combined_data..1<-combined_data %>% filter(istrtdaty>=2009 & istrtdaty<2025)
e1 <-ggplot(combined_data..1, aes(x=istrtdaty)) +
  stat_ecdf() + scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) #therefore best to exclude 2021 and after
e1
combined_data..2<-combined_data %>% filter(istrtdaty>=2010 & istrtdaty<2021)
e2 <-ggplot(combined_data..2, aes(x=istrtdaty)) +
  stat_ecdf() + scale_x_continuous(breaks = scales::pretty_breaks(n = 13))
e2
ggarrange(e1,e2)



