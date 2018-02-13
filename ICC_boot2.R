rm(list=ls())
library("ICC")
library(boot)
setwd("..")

mydata <- read.csv("FCTprime_allHCtime2_spss.csv")

dat <- mydata[,c(1:4,9:12)]


### Low Fat
dat1 <- subset(dat, affectsess == '#NULL!')
dat2 <- subset(dat, affectsess != '#NULL!')

id1 <- dat1$subnum
id2 <- dat2$subnum

iterations <- 100
boot_size <-1000

Diff_Low <- NULL
Diff_High <- NULL

count_IND_Low <- rep(0,boot_size)
count_IND_High <- rep(0,boot_size)

set.seed(123)

for (k in 1:boot_size) {
  for (i in 1:iterations) {
    dat1_boot <- dat1[sample(length(id1), replace=T),]
    dat1_ses1 <- subset(dat1_boot, select = c(subnum,dx,daysbtw,cho.lo.1,cho.hi.1))
    dat1_ses2 <- subset(dat1_boot, select = c(subnum,dx,daysbtw,cho.lo.2,cho.hi.2))
  
    
    dat2_boot <- dat2[sample(length(id2), replace=T),]
    dat2_ses1 <- subset(dat2_boot, select = c(subnum,dx,daysbtw,cho.lo.1,cho.hi.1))
    dat2_ses2 <- subset(dat2_boot, select = c(subnum,dx,daysbtw,cho.lo.2,cho.hi.2))
    
    
    colnames(dat1_ses1) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    colnames(dat1_ses2) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
  
    colnames(dat2_ses1) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    colnames(dat2_ses2) <- c("subnum","dx","daysbtw","Choice_Rating_Low","Choice_Rating_High")
    
    
    dat1_new <- rbind(dat1_ses1,dat1_ses2)
    dat2_new <- rbind(dat2_ses1,dat2_ses2)
    
    
    ICC_Choice_Low1 <- ICCest(factor(subnum), Choice_Rating_Low, data = dat1_new, CI.type = "S")
    ICC_Choice_High1 <- ICCest(factor(subnum), Choice_Rating_High, data = dat1_new, CI.type = "S")
    
    ICC_Choice_Low2 <- ICCest(factor(subnum), Choice_Rating_Low, data = dat2_new, CI.type = "S")
    ICC_Choice_High2 <- ICCest(factor(subnum), Choice_Rating_High, data = dat2_new, CI.type = "S")
  
    ICC_Diff_Low <- ICC_Choice_Low1$ICC - ICC_Choice_Low2$ICC
    ICC_Diff_High <- ICC_Choice_High1$ICC - ICC_Choice_High2$ICC
    
    Diff_Low[i] <- ICC_Diff_Low
    Diff_High[i] <- ICC_Diff_High
  }
  
  
  Diff_CI_Low <- quantile(Diff_Low,probs=c(0.025,0.975)) 
  Diff_CI_High <- quantile(Diff_Low,probs=c(0.025,0.975))
  
  if (Diff_CI_Low[1] < 0 && Diff_CI_Low[2] >0) {
    count_IND_Low[k] = 1
  }
  
  if (Diff_CI_High[1] < 0 && Diff_CI_High[2] >0) {
    count_IND_High[k] = 1
  }
}

1 - length(count_IND_Low[count_IND_Low == 0])/ boot_size ##0.84
1 - length(count_IND_High[count_IND_High == 0])/ boot_size

