library(lme4)
library(emmeans)

data <- data_ge
str(data)
head(data)
data$REP <- as.factor(data$REP)
data$GEN <- as.factor(data$GEN)
data$ENV <- as.factor(data$ENV)

Fitted <- lmer(GY ~ GEN + (1|ENV))
Fitted

# Extracting design matrix of fixed effects (Intercept and Genotype)
X <- model.matrix(formula(Fitted))
X <- X[!duplicated(X), ]
X

# Extracting the beta coefficients
Beta <- fixef(Fitted)
Beta

# Obtaining the BLUEs of genotypes
BLUEs_Gen <- X %*% Beta
BLUEs_Gen

# Estimating least square means by GEN
Lsmeans_Gen <- lsmeans(Fitted, ~ GEN)
str(Lsmeans_Gen)

# Creating the data frame of GEN and BLUEs
BLUEs_Gen1 <- data.frame(GID = levels(data$GEN),
                         BLUEs = summary(Lsmeans_Gen)$lsmean)
BLUEs_Gen1

# BLUP of genotypes
Fitted2 <- lmer(GY ~ (1|GEN) + (1|ENV) + (1|(GEN:ENV)))
Fitted2

# Fixed effect=Intercept
Intercept <- fixef(Fitted2)
str(Intercept)
U_ref <- c(ranef(Fitted2)$GEN)
U_ref

# BLUP of Genotypes
BLUP_Gen2 <- Intercept + U_ref$'(Intercept)'
BLUP_Gen2
cor(BLUEs_Gen, BLUP_Gen2)

# Creating a data frame of GEN and BLUPs
BLUPs_Gen1 <- data.frame(GID = levels(data$GEN),
                         BLUPs = BLUP_Gen2)
BLUPs_Gen1

# Creating a data frame of GEN, BLUEs, and BLUPs
Combined_Gen <- data.frame(
  GID = levels(data$GEN),
  BLUEs = BLUEs_Gen1$BLUEs,
  BLUPs = BLUP_Gen2
)

# Writing combined data to CSV file
write.csv(Combined_Gen, file = "BLUEs_BLUPs_Gen.csv", row.names = FALSE)
