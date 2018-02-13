rm(list=ls())
library("ICC")
library(boot)
setwd("..")

mydata <- read.csv("FCTprime_allHCtime2_spss.csv")

dat <- mydata[,c(1:4,9:12)]


### study 1
dat1 <- subset(dat, affectsess == '#NULL!')

id <- dat1$subnum

iterations <- 100
boot_size <-1000

Diff <- NULL
count_IND <- rep(0,boot_size)

set.seed(123)

for (k in 1:boot_size) {
  for (i in 1:iterations) {
    dat_boot <- dat1[sample(length(id), replace=T),]
    dat_ses1 <- subset(dat_boot, select = c(subnum,dx,daysbtw,cho.lo.1,cho.hi.1))
    dat_ses2 <- subset(dat_boot, select = c(subnum,dx,daysbtw,cho.lo.2,cho.hi.2))
  
    colnames(dat_ses1) <- c("subnum ","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    colnames(dat_ses2) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
  
    dat_new <- rbind(dat_ses1,dat_ses2)
    ICC_Choice_Low <- ICCest(factor(subnum), Choice_Rating_Low, data = dat_new, CI.type = "S")
    ICC_Choice_High <- ICCest(factor(subnum), Choice_Rating_High, data = dat_new, CI.type = "S")
  
    ICC_Diff <- ICC_Choice_High$ICC - ICC_Choice_Low$ICC
    Diff[i] <- ICC_Diff
  
  }
  Diff_CI <- quantile(Diff,probs=c(0.025,0.975)) 
  if (Diff_CI[1] < 0 && Diff_CI[2] >0) {
    count_IND[k] = 1
  }
}

1 - length(count_IND[count_IND == 0])/ boot_size ##0.84



### study 2
dat2 <- subset(dat, affectsess != '#NULL!')

id <- dat2$subnum

iterations <- 100
boot_size <-1000

Diff <- NULL
count_IND <- rep(0,boot_size)

set.seed(123)

for (k in 1:boot_size) {
  for (i in 1:iterations) {
    dat_boot <- dat2[sample(length(id), replace=T),]
    dat_ses1 <- subset(dat_boot, select = c(subnum,dx,daysbtw,cho.lo.1,cho.hi.1))
    dat_ses2 <- subset(dat_boot, select = c(subnum,dx,daysbtw,cho.lo.2,cho.hi.2))
    
    colnames(dat_ses1) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    colnames(dat_ses2) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    
    dat_new <- rbind(dat_ses1,dat_ses2)
    ICC_Choice_Low <- ICCest(factor(subnum), Choice_Rating_Low, data = dat_new, CI.type = "S")
    ICC_Choice_High <- ICCest(factor(subnum), Choice_Rating_High, data = dat_new, CI.type = "S")
    
    ICC_Diff <- ICC_Choice_High$ICC - ICC_Choice_Low$ICC
    Diff[i] <- ICC_Diff
    
  }
  Diff_CI <- quantile(Diff,probs=c(0.025,0.975)) 
  if (Diff_CI[1] < 0 && Diff_CI[2] >0) {
    count_IND[k] = 1
  }
}

1 - length(count_IND[count_IND == 0])/ boot_size ## <0.01


### study 1 & 2

id <- dat$subnum

iterations <- 100
boot_size <-1000

Diff <- NULL
count_IND <- rep(0,boot_size)

set.seed(123)

for (k in 1:boot_size) {
  for (i in 1:iterations) {
    dat_boot <- dat[sample(length(id), replace=T),]
    dat_ses1 <- subset(dat_boot, select = c(subnum,dx,daysbtw,cho.lo.1,cho.hi.1))
    dat_ses2 <- subset(dat_boot, select = c(subnum,dx,daysbtw,cho.lo.2,cho.hi.2))
    
    colnames(dat_ses1) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    colnames(dat_ses2) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    
    dat_new <- rbind(dat_ses1,dat_ses2)
    ICC_Choice_Low <- ICCest(factor(subnum), Choice_Rating_Low, data = dat_new, CI.type = "S")
    ICC_Choice_High <- ICCest(factor(subnum), Choice_Rating_High, data = dat_new, CI.type = "S")
    
    ICC_Diff <- ICC_Choice_High$ICC - ICC_Choice_Low$ICC
    Diff[i] <- ICC_Diff
    
  }
  Diff_CI <- quantile(Diff,probs=c(0.025,0.975)) 
  if (Diff_CI[1] < 0 && Diff_CI[2] >0) {
    count_IND[k] = 1
  }
}

1 - length(count_IND[count_IND == 0])/ boot_size ## <0.01


