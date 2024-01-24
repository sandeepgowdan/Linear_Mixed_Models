
### ANOVA, MIXED MODELS for BALANCED AND UNBAALANCED DATA ####

## clear the environment and set woking directory
setwd()
library(dplyr)
# Download the data
maize = read.csv("MaizeRILs.csv")
maize$rep = as.factor(maize$rep)
maize$block = as.factor(maize$block)

# Exploratory analysis
str(maize)
table(maize$RIL,maize$location, maize$rep) # is the data balanced

par(mfrow=c(1,2))
boxplot(maize$height ~maize$location) # effect of location
boxplot(maize$height ~ maize$rep +maize$location) # effect of block within location

# Fitting the model
##rep is nested within location and location is crossed (interaction) with RILS
m0 = lm(height ~ rep:location + location + RIL + location*RIL, data=maize)
anova(m0) # fit the model

plot(m0, which = c(1,2)) # check model assumptions


# What is the most representative value of the genetic potential of RILs?
# i) Maybe, the mean per RIL
RIL_mean = maize %>%
  dplyr::group_by(RIL) %>%
  dplyr::summarise(mean = mean(height)) %>%
  as.data.frame() 
head(RIL_mean)

# ii) Least Square estimated from the model !
# RIL_i = mu + t_i + mean(t_i:loc) + mean(t_i:block)
library(emmeans)
lsmeans1 = summary(lsmeans(m0, "RIL"))
blues = data.frame(id = lsmeans1$RIL,
                   lsmeans = lsmeans1$lsmean,
                   blues = lsmeans1$lsmean - mean(maize$height),
                   RIL_mean = RIL_mean$mean) 
head(blues)
plot(blues$lsmeans, blues$RIL_mean)

########## MIXED MODELS ###########

library(lme4)
library(lmerTest)

mm = lmer(height ~ rep:location + location + (1|RIL)  + (1|location:RIL),data = maize)
summary(mm)
plot(mm)
anova(mm) # very similar results from the fixed model
summary(mm)$varcor # Random effects



# Comparing the importance of GxE interaction
mm_full = lmer(height ~ rep:location + location + (1|RIL)  + (1|location:RIL),data = maize)
rand(mm_full) # from the lmerTest

####### ANOVA vs MIXED EFFECTS OF BALANCED DATA #######

# lsmeans (or BLUES) from a fixed model
lsmean_obj = lsmeans(m0, "RIL")
blues = summary(lsmean_obj) 

# ---- BLUPs ---- 
# Predicted random effects are called BLUP (best linear unbiased predictor)
randeffect = ranef(mm)
blups = randeffect$RIL$`(Intercept)`

# ---- Comparing ---
# Plots
par(mfrow=c(1,3))
hist(blues$lsmean, main="lsmeans")
hist(blups, main="BLUPs")
plot(blues$lsmean, blups, main="blups vs. lsmeans")


# We can present the BLUPs in the same scale of the trait
# adj.means = BLUP + overal.mean
final = data.frame(RIL = blues$RIL,
                   lsmeans = blues$lsmean,
                   blups = blups,
                   adj.blups = blups + mean(maize$height))
head(final,n=10)



### UNBALANCED DATA #######


# Download the data
maize_miss = read.csv("MaizeRILs_miss.csv")
maize_miss$rep = as.factor(maize_miss$rep)

# Exploratory analysis
str(maize_miss)
table(maize_miss$RIL,maize_miss$location) # RIL-5 was evaluated in only two location, for example

# ANOVA and Mixed Model
m1 = lm(height ~ rep:location + location + RIL + location*RIL,data=maize_miss) # fixed
mm1 = lmer(height ~ rep:location + location + (1|RIL)  + (1|location:RIL), data = maize_miss) # random

# #Results
anova(m1) #df for location:RIL changed
summary(mm1) # Some differences
# 
# # lsmean package
lsmean_obj = summary(lsmeans(m1, "RIL"))
head(lsmean_obj) 

# # Predict the random term
randeffect = ranef(mm1)
blups = randeffect$RIL$`(Intercept)`
hist(blups)
blups










