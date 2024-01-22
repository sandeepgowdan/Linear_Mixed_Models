##generalized linear models
# Simulate example data
set.seed(123)
n <- 50  # Number of observations
treatments <- c("A", "B")
treatment_data <- sample(treatments, n, replace = TRUE)
treatment_data
yield <- rnorm(n, mean = ifelse(treatment_data == "A", 50, 60), sd = 5)
yield

# Create a data frame
data <- data.frame(treatment = as.factor(treatment_data),
                   yield = yield)
data
# Fit a Generalized Linear Model (GLM)
glm_model <- glm(yield ~ treatment, data = data, family = gaussian)

# Summarize the model
summary(glm_model)


####Gaussian (Normal) Family (family = gaussian):
#Suitable for continuous response variables with normally distributed errors.
#Example: Height, weight, yield, etc.

####Poisson Family (family = poisson):

#Suitable for count data where the response variable represents 
#the number of events occurring in a fixed interval.
#Example: Number of flowers, number of pests, etc.


####Binomial Family (family = binomial):

#Suitable for binary response variables (success/failure) or proportions.
#Example: Germination success (yes/no), proportion of seeds germinated.

##Gamma Family (family = Gamma):
#Suitable for continuous, positive response variables that often have a right-skewed distribution.
#Example: Time to maturity, growth rate, etc.

