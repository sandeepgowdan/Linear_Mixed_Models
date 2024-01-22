## Heritablity BLUPs Using Linear mixed model

##Import data
data <- data_ge
str(data)
head(data)
attach(data)
hist(GY)

data$REP<- as.factor(data$REP)
data$GEN<- as.factor(data$GEN)
data$ENV<- as.factor(data$ENV)

library(lme4)
##compute Variance compaonent
##we use random effects, we feel thata data is representative
#of population and is drwan randomly

Yieldvarcomp<- lmer(GY ~ (1|GEN) + (1|ENV) + (1|REP%in%ENV)+(1|(GEN:ENV)))
## Model is over parametrized: because there is one obd=servation for ecvery replicatin in
##(1|REP%in%ENV) causes spurious errir var in (1|REP%in%ENV)
##give good estimate but not reliable
## to solve this bypass the warnings or use other model

Yieldvarcomp<- lmer(GY ~ (1|GEN) + (1|ENV) +(1|(GEN:ENV)))
summary(Yieldvarcomp)

##BLUPs account for environmental effects and missing data
## Blups shrinks the value towards mean
My_blup<-ranef(Yieldvarcomp)
str(My_blup)

##Extart BLUPs

Varietyblups<- My_blup$GEN
Varietyblups

write.csv(Varietyblups, file = "varietyblups.csv")




##ANOVA in R By mixed model

data<- data_ge
str(data)
data$REP<- as.factor(data$REP)
data$GEN<- as.factor(data$GEN)
data$ENV<- as.factor(data$ENV)
str(data)

library(lme4)
library(lmerTest)
m =aov(GY~ GEN + ENV + GEN:ENV, data=data) ## all are fixed effects
summary(m)

m1 =aov(GY~  GEN*ENV, data=data)
summary(m1)

###linear Mixed models  use random and fixed effects
Results1<- lmer(GY~ (GEN) + (1|ENV), data=data)
anova(Results1)
rand(Results1)  ##to see random effects
