rm(list=ls())
setwd("C:/R folder/EC DS/Misc/UKDS 2020/6614stata_54BAB6F89E00B73D09078E3AA069E59E09CABADF507F71FB415420400843987A_V1/UKDA-6614-stata/stata/stata13_se/ukhls")

set.seed(12345)
# read data1 --------
data1 <- read_dta("l_indresp.dta") 
#data <- pdata.frame(data,index=c("pidp","h_istrtdaty"),drop.index=FALSE)
#options(max.print=10000)
data1 %<>% filter(l_basrate>0.01)
quantile(data1$l_basrate, 0.01)
quantile(data1$l_basrate, 0.99)
data1 %<>% filter(l_basrate>4.616 & l_basrate<33 & !l_sex ==0) #5115/5221
data1 = select(data1, -2,-17:-23)
#data1$h_indinub_lw
data1[data1 < 0] <- 0
ggplot(data1, aes(x=l_basrate, colour=as.factor(l_sex)))+geom_density() +theme_classic()
