## claer the workig directory
rm(list=ls())
data_ge
data<- data_ge
# Install and load the necessary package
install.packages("lme4")
library(lme4)


# Fit the linear mixed-effects model REP as fixed and GEN REP ENV as random and REP nested in ENV
model <- lmer(HM ~ REP + (1 | GEN) + (1 | ENV/REP), data = data)

# Print the summary of the model
summary(model)



# Extract variance components
var_comp <- VarCorr(model)

# Calculate heritability
total_var <- sigma(model)^2 + sum(unlist(var_comp))^2  # Total phenotypic variance
genetic_var <- sum(var_comp$vcov[,"GEN"]) + sum(var_comp$vcov[,"GEN:ENV"])  # Genetic variance

heritability <- genetic_var / total_var

total_var


# Extract fixed effects
fixed_effects <- fixef(model)
fe <- fixed_effects


## BLUEs for fixed effects
(BLUEs=fe+c(0,rep(fe[1],length(fe)-1)))
(mean_BLUEs=mean(BLUEs))
mean_BLUEs


## random effects
re=ranef(model)
re
BLUPs=re[[3]]+mean_BLUEs  ## if more than 1 random effects cahnge rge number in the bracket
BLUPs





