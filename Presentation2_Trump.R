#############################
## Presentation Trump var
#############################

setwd("~/Econometrix2018/Presentation2")
Trump_df<-read.csv("Trump_wiz_Polarity.csv")

View(Trump_df)
attach(Trump_df)
plot(Polarity,Retweets)
hist(Polarity)
summary(lm(Retweets~Polarity+I(Polarity^2)))

#VIF
summary(lm(Polarity~I(Polarity^2)))
1/(1-0.3442)

#Bruch Pegan Test
Trump.lm<-lm(Retweets~Polarity+I(Polarity^2))
summary(Trump.lm)
resi<-Trump.lm$residuals
summary(lm(resi^2~Polarity+I(Polarity^2)))
#robst se
sqrt(diag(hccm(Trump.lm,type="hc0")))
-151.191/25.914264
10.627/6.426094

summary(lm(Retweets~Polarity+I(Polarity^2),weights = 1/Polarity))
#Local min (7.114, 3829.21)

#FGLS
resilog<-lm(log(resi^2)~Polarity+I(Polarity^2))
FGLS<-resilog$fitted.values
FGLS
h<-exp(FGLS)
h
summary(lm(Retweets~Polarity+I(Polarity^2), weights = 1/h))
