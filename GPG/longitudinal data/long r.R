# Longitudinal Dataset, October 2021 - December 2022
setwd("~/GitHub/ECDS-Grp/GPG/longitudinal data")
rm(list=ls())
 library(haven)
library(dplyr)
 long22 <- read_dta("lgwt22_5q_od21_od22_eul.dta")
 long22 <- data.frame(lapply(long22, as.numeric))
 long22 <- na.omit(long22)
