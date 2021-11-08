# Practical-1 Script

# This file contains some general statistics computed using R programming language.

# Edited by David Du
# Nov-08-2021


## Exercise 2: More on Analyses with Single Categorical Explanatory Variable and Continuous Response----

# Import Peru Soils data
soils <- read.csv("Peru_Soil_Data.csv",row.names=1)
str(soils)
head(soils)

# Distribution of different variables
hist(soils$Soil_pH,breaks=10, col="grey", xlab="Soil pH", main="")

# Add line specifying mean and median of the distribution
abline(v=median(soils$Soil_pH))
abline(v=mean(soils$Soil_pH))

# Variation of continuous variable across discrete categorical variables
boxplot(Soil_pH~Habitat, data = soils)
boxplot(Potassium~Habitat, data = soils)

# Conduct ANOVA to see if pH and Potassium differ by Habitat types

# pH
lm_pH <- lm(Soil_pH~Habitat,data=soils)
anova(lm_pH)

# K
lm_K <- lm(Potassium~Habitat,data=soils)
anova(lm_K)

# Check for assumptions

lm_pH_resids <- resid(lm_pH)

shapiro.test(lm_pH_resids)

bartlett.test(Soil_pH ~Habitat,data=soils)

lm_K_resids <- resid(lm_K)

shapiro.test(lm_K_resids)

bartlett.test(Potassium ~Habitat,data=soils)  # p-value < 0.04, potassium is not homogenous

# Visualize to see if assumptions has been met
plot(lm_pH)
plot(lm_K)

# Histogram of distribution
hist(soils$Soil_pH, breaks=10)
hist(soils$Potassium, breaks=10)  # distribution of potassium is right skewed

# Assessing distribution of data in boxplot
boxplot(Soil_pH~Habitat,data=soils)
boxplot(Potassium~Habitat,data=soils)

# Log transforms data to ensure that normality assumptions are met
soils$log_K <- log(soils$Potassium)

# Redo analysis above
lm_log_K <- lm(log_K ~Habitat,data=soils)
anova(lm_log_K)

# Check for normality of assumption under log transformation
lm_log_K_resids <- resid(lm_K)
shapiro.test(lm_log_K_resids)  # now the p-value is no longer significant
bartlett.test(log_K ~Habitat,data=soils)
plot(lm_log_K)


## Exercise 3: Relationships between Continuous Variables ----

# Scatter plot of soil ph against base saturation 
plot(soils$Total_Base_Saturation, soils$Soil_pH)
plot(Soil_pH~Total_Base_Saturation, data=soils)

# Reshape appearance of the plot
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue")
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue",cex=2)
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue",cex=2,cex.lab=1.5)

# Adding a line of best fit
bestfit <- lm(Soil_pH~Total_Base_Saturation,data=soils)  # create a linear model predicting pH from base saturation
abline(bestfit)

# Alter the appearance of the line
plot(Soil_pH~Total_Base_Saturation,data=soils,pch=21,bg="blue",cex=2,cex.lab=1.5)
abline(bestfit,lty=2,lwd=2,col="red")

# Explore relationship between soil pH and clay
plot(Soil_pH~Clay, data=soils)

pH_vs_Clay <- lm(Soil_pH~Clay,data=soils)
anova(pH_vs_Clay)  # p-value = 0.05227, not significant 
summary(pH_vs_Clay)

# Evaluation of heteroscedasticity 
pH_vs_Clay_resids <- resid(pH_vs_Clay)
shapiro.test(pH_vs_Clay_resids)  # p-value = 0.2897
plot(pH_vs_Clay)  # Last plot shows Blanquillo_floodplain has high leverage

# Identify the outlier
plot(Soil_pH~Clay, data=soils)
text(Soil_pH~Clay, data=soils,labels=rownames(soils))  # realize this point is an outlier
abline(pH_vs_Clay,col="red")

# Remove that outlier and refit the model
rownames(soils)
pH_vs_Clay_new <- lm(Soil_pH~Clay,data=soils[-1,])
anova(pH_vs_Clay_new)
plot(pH_vs_Clay_new)  # apparently now it is significant with p-value < 0.05


## Exercise 4: Non-parametric Alternatives to Simple Linear Models ----

# Look at how sodium concentration vary across habitats
salt <- lm(Sodium~ Habitat,data=soils)
summary(salt)
plot(salt)

# Log transform variable to see if we can justify it
lm_log_salt <- lm(log(Sodium)~ Habitat,data=soils)
summary(lm_log_salt)
plot(lm_log_salt)  # still have an issue outliers

# Introduce.... Kruskal test
kruskal.test(Sodium~ Habitat,data=soils)  # p-value < 0.05

# notes: for non-parametric tests, don't use log transformed data

# Investigate the relationship between total base saturation and sand content in soil
lm_sand_TBS <- lm(Total_Base_Saturation~Sand, data=soils)
summary(lm_sand_TBS)
plot(lm_sand_TBS)

# Rank based non-parametric tests
cor.test(soils$Sand,soils$Total_Base_Saturation,data=soils, method="spearman")  # p-value < 0.05

# To remove that warning message
cor.test(soils$Sand,soils$Total_Base_Saturation,data=soils, method="spearman",exact=FALSE)


## Exercise 5: General Linear Models (basically linear models with greater than 1 variables) ----

# To see if soil_pH can be explained by habitat and total_base_saturation
lm_pH_Habitat_TBS <- lm(Soil_pH~Habitat + Total_Base_Saturation,data=soils)
summary(lm_pH_Habitat_TBS)

# Add in the interaction factor
lm_pH_Habitat_TBS_Interaction <-lm(Soil_pH ~ Habitat*Total_Base_Saturation, data=soils)

anova(lm_pH_Habitat_TBS_Interaction)
summary(lm_pH_Habitat_TBS_Interaction)  # can remove the interaction variable 
plot(lm_pH_Habitat_TBS_Interaction)

# Model comparison
lm_pH_Habitat <- lm(Soil_pH~Habitat,data=soils)
lm_pH_TBS <- lm(Soil_pH~Total_Base_Saturation,data=soils)

# Compare 4 models
summary(lm_pH_Habitat)
summary(lm_pH_TBS)  # Total base saturation explains more of the variation in soil pH values than habitat
summary(lm_pH_Habitat_TBS)
summary(lm_pH_Habitat_TBS_Interaction)

# Model with 2 categorical variables
summary(soils$River_Basin:soils$Habitat)
soils_trim <- soils[soils$River_Basin=="Manu"|soils$River_Basin=="Los_Amigos",]
soils_trim <- soils[soils$River_Basin %in% c("Manu","Los_Amigos"),]

# Explaining variation in phosphorus levels using various ther categorical variables
lm_P_habitat <- lm(Phosphorus~ Habitat,data= soils_trim)  
lm_P_basin <- lm(Phosphorus~ River_Basin,data= soils_trim) 
lm_P_habitat_basin <- lm(Phosphorus~ Habitat+River_Basin, data=soils_trim)
lm_P_habitat_basin_interaction <- lm(Phosphorus~ Habitat*River_Basin, data=soils_trim)

# Compare Models
summary(lm_P_habitat)  
summary(lm_P_basin)  
summary(lm_P_habitat_basin)  
summary(lm_P_habitat_basin_interaction)  # has the lowest overall p-value



