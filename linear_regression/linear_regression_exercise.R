## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

##   Set working directory
setwd("C:/Users/Noura/Documents/R/Data Science Foundation/linear_regression/linear_regression")

##   Load data set
states <- readRDS("dataSets/states.rds")

##   Plot the data
plot(states$metro, states$energy)

##   Linear model
model1 <- lm(energy ~ metro, data = states)

##   Model1 Summary  
summary(model1)
SSE <- sum(model1$residuals^2)
SSE
plot(model1)

##   Plotting other varaiables
plot(states$income, states$energy)
plot(states$green, states$energy)
plot(states$density, states$energy)
plot(states$waste, states$energy)
plot(states$area, states$energy)
##   green has a positve relation with energy
## model with new variables metro/green/area
model2 <- lm(energy ~ metro + green , data = states)

##   Model2 Summary
summary(model2)

##   Model2 is better than model1 because it has higher multiple R-Squared and green has a significant linear relation with energy
plot(model2)

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

##  Interaction term
model3 <- lm(energy ~ metro*green, data = states)
coef(summary(model3))

##   Adding region to the model

str(states$region)
states$region <- factor(states$region)

model4 <- lm(energy ~ region , data = states)
summary(model4)
coef(summary(model4))
plot(model4)

##   Region/Energy do not have a significant linear relation