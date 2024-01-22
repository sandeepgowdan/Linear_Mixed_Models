##Mixed model Analydsis
library(lme4)
library(lmerTest)
library(agricolae)
library(lsmeans)
library(car)
library(mltcomp)
library(emmeans)

data<- data_ge
data$ENV<- as.factor(data$ENV) 
leveneTest(GY~ENV, data=data)
## simliary can be done for years

##suubset the location E1 for fixed model
LOC<- data[data$ENV == "E1",]
dim(LOC)
str(LOC)

LOC$REP<- as.factor(LOC$REP)
LOC$GEN<- as.factor(LOC$GEN)
LOC$ENV<- as.factor(LOC$ENV)
str(LOC)

#linear model
##simple linear model all effects are fixed
T1<- lm(GY~1+GEN+REP, data = LOC)
summary(T1)
anova(T1)


## Linear Mixed model

T2<- lmer(GY~1+GEN+(1|REP), data = LOC, REML =FALSE) ##or
T22<- lmer(GY~1+GEN+(1|REP), data = LOC)

summary(T2) ## or T22 
anova(T2)  ##or T22

##Multple comparison of treatments
##emmeans(model, list(pairwise ~Group), adjust ="turkey")
emmeans(T2, ist(pairwise ~ GEN), adjust = "turkey")  ##check

T3 <- lmer(GY~1+(1|GEN)+(1|REP), data = LOC, REML =FALSE)
summary(T3)

#model comparison using AIC
anova(T2, T3) # the model with the lower AIC is the one which is better

## Linera mixed model for all the location
Mod1 <- lmer(GY ~ 1 +GEN +(1|REP) + (1|ENV), data=data, REML = FALSE)
summary(Mod1)

##model with interaction wwith location and year
Mod2<- lmer(GY~ 1+GEN + (1|REP) + (1|ENV) + (1|YEAR)
            + (1|ENV:YEAR) + (1| ENV:GEN) + (1| YEAR:GEN)
            + (1| GEN: ENV:YEAR), data=data, REML =FALSE)
summary(Mod2)

anova(Mod1, Mode2)


##Subset each yaer and can do mixed liner model
print(VarCorr(Mod1), comp=("Variance"))
