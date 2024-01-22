
## Generalized Linear mixed model using lme4 from the penguind data from  palmerpenguins package
rm(list = ls())


install.packages("palmerpenguins")
library(palmerpenguins)
library(lme4)
library(tidyverse)
data("penguins")

penguins_data <- penguins
str(penguins_data)
head(penguins_data)

##convert into factotrs
penguins_data$ species <- as.factor(penguins_data$ species)
penguins_data$island <- as.factor(penguins_data$island)
penguins_data$sex <- as.factor(penguins_data$sex)
penguins_data$year <- as.factor(penguins_data$year)


## remova NA or misssing values
penguins_data <- na.omit(penguins_data)

## fit the glm model from the glm function of lme4 Package
glm.mod <- glm(
  sex ~ body_mass_g + bill_length_mm + species, 
  family = binomial, 
  data = penguins_data
)

##fitted.values contains the probabilities the GLM predicted
glm.mod$fitted.values[1:10]
length(glm.mod$fitted.values)

##manually turn the predicted probabilities into sex predictions. 
##That requires us to know whether the predicted 

threshold <- 0.5
preds <- penguins_data %>% 
  mutate(
    prob.fit = glm.mod$fitted.values,
    prediction = if_else(prob.fit > threshold, 'male', 'female'),
    correct = if_else(sex == prediction, 'correct', 'incorrect')
  )
preds


##Predict other probabilities for observations that have not been present in the original dataset.

new_observations <- tibble(
  body_mass_g = c(4532, 5392),
  bill_length_mm = c(40, 49),
  species = c('Adelie', 'Gentoo')
)
predict(
  glm.mod, 
  newdata = new_observations
)
##This  will gave us the value of the linear predictors. But we don't need this 
##  So we Will predict() that we care about the response variable.

predict(
  glm.mod, 
  newdata = new_observations,
  type = 'response'
)
## these are probabilities.These are our predicted probabilities. 
## we will save the probabilities into our tibble.

new_observations <- new_observations %>%
  mutate(
    pred_prob = predict(
      glm.mod,
      newdata = new_observations,
      type = 'response'
    ),
    prediction = if_else(pred_prob > 0.5, 'male', 'female')
  )
new_observations



