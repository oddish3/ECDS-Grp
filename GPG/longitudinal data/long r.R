# Longitudinal Dataset, October 2021 - December 2022
setwd("~/GitHub/ECDS-Grp/GPG/longitudinal data")
rm(list=ls())
 library(haven)
 long22 <- read_dta("lgwt22_5q_od21_od22_eul.dta")
 View(long22l)
library(labelled) 
look_for(long22, 'hour') #hourpay
look_for(long22, 'date')
         
