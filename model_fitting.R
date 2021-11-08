# Model fitting and Generalized Linear Model

# This file contains a tutorial material for fitting models and constructing Generalized Linear Model

# Edited by David Du
# Nov-08-2021

# Note: model fit refers to investigating the R-squard adj value
# Note: AIC -> Akaike Information Criterion -> refers to the likelihood of the model

# Import data ----
soils <- read.csv("Peru_Soil_Data.csv", row.names=1, stringsAsFactors=T)

## Exercise 1: Exploring Model Fit ----

# Creating different statistical models
lm_Habitat <- lm(Soil_pH~Habitat,data=soils)
lm_TBS <- lm(Soil_pH~Total_Base_Saturation,data=soils)
lm_Habitat_TBS <- lm(Soil_pH~Habitat + Total_Base_Saturation,data=soils)
lm_Habitat_TBS_Interaction <-lm(Soil_pH~Habitat*Total_Base_Saturation, data=soils)

# Compare their AIC values -> lower AIC, the better.  AIC is the only way to compare models of non-continuous variables
AIC(lm_Habitat, lm_TBS, lm_Habitat_TBS, lm_Habitat_TBS_Interaction)  # df -> # of variables estimated

# Calculating AIC by hand
logLik(lm_Habitat_TBS) #gives a value of 3.59134
2*4 - 2*3.59134

# Constructing a null model
lm_null <- lm(Soil_pH ~ 1 , data=soils)
AIC(lm_null, lm_Habitat, lm_TBS, lm_Habitat_TBS)  # Consider if the simple models and even worth accessing


## Exercise 2: Generalised Linear Models (GLMs) with a Poisson Response ----

# Another dataset
inga <- read.csv("Inga_abundances.csv",row.names=1,stringsAsFactors=T)

# Combine the soil and tree database
combo <- cbind(soils,inga)

# Assess how the abundance of different species varies with soil characteristics and habitat type
mod1 <- glm(thibaudiana~Habitat, data=combo, family=poisson)  # family = poisson since we are dealing with count variable
mod1

# Since we are logging the value for poisson, to obtain true value need to exponentiate
exp(-1.946)  # this is the true value of the negative value of the intercept
mean(combo$thibaudiana[combo$Habitat=="floodplain"])  

# We try to do the same for upland habitat
exp(-1.946+4.518)
mean(combo$thibaudiana[combo$Habitat=="upland"])

summary(mod1)

# Compare using AIC
mod_null <- glm(thibaudiana~1,data=combo,family=poisson)
AIC(mod_null,mod1)

# Try a second model predicting abundance with pH
mod2 <- glm(thibaudiana~Soil_pH,data=combo,family=poisson)
summary(mod2)
AIC(mod_null,mod1,mod2)  # not as good as model 1

# Try with combo model and model with interaction elements 
mod3 <- glm(thibaudiana~Habitat + Soil_pH,data=combo,family=poisson)
mod4 <- glm(thibaudiana~Habitat * Soil_pH,data=combo,family=poisson)
AIC(mod_null,mod1,mod2,mod3,mod4)  # mod3 is the best

# Mod 3 is the best, explore different aspect of the model
summary(mod3)

# Plot predicted versus observed
plot(combo$thibaudiana,fitted(mod3),xlab="Observed",ylab="predicted")
abline(0,1)  # see what we underpredict in the middle abundance 

# Residual versus our explanatory variable
par(mfrow=c(1,2))
plot(resid(mod3)~Habitat,data=combo)  # variability around the predicted mean in uplands
plot(resid(mod3)~Soil_pH,data=combo)  # a lot of variability at lower pH levels

# See how predicted variables compare to our observed variables
combo_fp <- combo[combo$Habitat=="floodplain",]
combo_up <- combo[combo$Habitat=="upland",]
plot(combo$Soil_pH,combo$thibaudiana,type="n")
points(combo_fp$Soil_pH,combo_fp$thibaudiana,pch=21,bg="blue")  # floodplain
points(combo_up$Soil_pH,combo_up$thibaudiana,pch=24,bg="red")  # uplands
points(combo$Soil_pH,fitted(mod3), col="grey")  # predicted values


## Exercise 3: Generalised Linear Models (GLMs) with a Binomial Response ----
## ***Key information -> binomial regression values are actually the proportion of time we see a given data (aka value of 1) per unit of explainatory variable

# Look at presence-absence of Inga auristellae (normally overdispersed if you investigate with poisson models)
combo$auristellae_PA <- combo$auristellae
combo$auristellae_PA[combo$auristellae_PA>0] <- 1  # create new "binary variable" signifying presence-absence of the tree
mod1 <- glm(auristellae_PA~Soil_pH,data=combo,family=binomial)  # explain presence absence by pH
summary(mod1)  # Pay attention to (- or +) only.  Est = -1.3961, this means as soil pH increase, our tree is less likely to be present

# Compare model to null to see if soil pH actually significantly affects presence and absence of our tree
mod_null <- glm(auristellae_PA~1,data=combo,family=binomial)
AIC(mod_null, mod1)  # our model has lower AIC, so significant

# Compare with other models with habitat (habitat, habitat combo, habitat interaction)
mod2 <- glm(auristellae_PA~ Habitat,data=combo,family=binomial)
mod3 <- glm(auristellae_PA~Soil_pH + Habitat,data=combo,family=binomial)
mod4 <- glm(auristellae_PA~Soil_pH * Habitat,data=combo,family=binomial)
AIC(mod_null, mod1, mod2, mod3, mod4)  # our previous model is still the best, also model 1 and 2 have similar explainitory power

# AIC for soil is lower than our previous model, let's see why
plot(auristellae_PA~Soil_pH,data=combo,pch=16)
points(combo$Soil_pH,fitted(mod1), col="grey")




