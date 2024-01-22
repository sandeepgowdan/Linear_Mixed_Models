##rcbd in R By mixed model

data<- data_ge
str(data)
data$REP<- as.factor(data$REP)
data$GEN<- as.factor(data$GEN)
data$ENV<- as.factor(data$ENV)
str(data)
##fIT fIXED MODEL

model = lm(GY~ GEN + ENV + GEN:ENV, data=data) ## all are fixed effects
anova(model)

##mixed model
model1 = lmer(GY~ (1|GEN) + ENV + (1|GEN:ENV), data=data)
anova(model1, type1 = 1)
summary(model1)

###mean separtion
library(agricolae)
LSD<-LSD.test(model,"GEN") 
LSD
