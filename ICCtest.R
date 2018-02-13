rm(list=ls())
library("ICC")
setwd("..")

mydata <- read.csv("newdata2.csv")

FCT_low <- mydata[which(mydata$Food == 'low'), ]
ICC_Choice_low <- ICCest(factor(subnum), Choice_Rating, data = FCT_low, CI.type = "S")
ICC_Health_low <- ICCest(factor(subnum), Health_Rating, data = FCT_low, CI.type = "S")
ICC_Taste_low <- ICCest(factor(subnum), Taste_Rating, data = FCT_low, CI.type = "S")

FCT_high <- mydata[which(mydata$Food == 'high'), ]
ICC_Choice_high <- ICCest(factor(subnum), Choice_Rating, data = FCT_high, CI.type = "S")
ICC_Health_high <- ICCest(factor(subnum), Health_Rating, data = FCT_high, CI.type = "S")
ICC_Taste_high <- ICCest(factor(subnum), Taste_Rating, data = FCT_high, CI.type = "S")


ICC <- round(c(ICC_Choice_low$ICC,ICC_Choice_high$ICC,ICC_Health_low$ICC,ICC_Health_high$ICC,
         ICC_Taste_low$ICC, ICC_Taste_high$ICC),3)
LowerCI <- round(c(ICC_Choice_low$LowerCI,ICC_Choice_high$LowerCI,ICC_Health_low$LowerCI,ICC_Health_high$LowerCI,
             ICC_Taste_low$LowerCI, ICC_Taste_high$LowerCI),3)
UpperCI <- round(c(ICC_Choice_low$UpperCI,ICC_Choice_high$UpperCI,ICC_Health_low$UpperCI,ICC_Health_high$UpperCI,
             ICC_Taste_low$UpperCI, ICC_Taste_high$UpperCI),3)

Food <- c(rep(c('Low Fat','High Fat'),3))
Type <- c(rep('Choice',2),rep('Health',2),rep('Taste',2))

DF <- data.frame(Type,Food,ICC,LowerCI,UpperCI)

write.csv(DF, file = "ICC_Study2.csv", na = "NA",  row.names = F)