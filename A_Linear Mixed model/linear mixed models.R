##lme4 linear mixed models
rm(list = ls())



### linear mixed models

dat1=read.csv("lentil_blocked.csv")
head(dat1)

tapply(dat1$YIELD, dat1$VARIETY, mean)
dat2=dat1[-1,]

head(dat2)
tapply(dat2$YIELD, dat2$VARIETY, mean)
dat3=dat1[-c(2,5,8,9,11),]

head(dat3)
tapply(dat3$YIELD, dat3$VARIETY, mean)
tapply(dat1$YIELD, dat1$VARIETY, mean)

library(emmeans)


attach(dat3) # or place dat3$ before all variables hereafter
out3=lm(YIELD~VARIETY+BLOCK)
emmeans(out3, ~VARIETY)


library(lme4)
out4=lmer(YIELD~VARIETY+(1|BLOCK), dat3)
fe=fixef(out4)
fe
BLUEs=fe+c(0,rep(fe[1],length(fe)-1))
BLUEs


BLUEs_SE=sqrt(diag(vcov(out4)))
BLUEs_SE # Note totally wrong standard errors not reflecting n
emmeans(out4, ~VARIETY) # easier code, but still wrong SEs


out5=lmer(YIELD~BLOCK+(1|VARIETY), dat3)
fe=fixef(out5)
fe
summary(out5)

(BLUEs=fe+c(0,rep(fe[1],length(fe)-1)))
(mean_BLUEs=mean(BLUEs))
mean_BLUEs

re=ranef(out5)
re
BLUPs=re[[1]]+mean_BLUEs
BLUPs

### if two or more random effects 
# Assuming two random effects
re1 = re[[1]]
re2 = re[[2]]

# Calculate BLUPs by adding both sets of random effects to the mean of the BLUEs
BLUPs = re1 + re2 + mean_BLUEs

# Assuming k random effects
re1 = re[[1]]
re2 = re[[2]]
# ...
re_k = re[[k]]

# Calculate BLUPs by adding all sets of random effects to the mean of the BLUEs
BLUPs = re1 + re2 + ... + re_k + mean_BLUEs


## Summary of the model
summary(out5)
summary(out4)

##eatimate the fixed co-efficients from the model
coef(out5)
coef(out4)


##AIC ad BIC VALUES
AIC(out4)
BIC(out4)

## anova for models
anova(out4, out5)


## predictions from the model

predict(out4, newdata= newdata.csv)



