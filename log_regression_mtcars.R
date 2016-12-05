#INFO: http://www.theanalysisfactor.com/r-glm-model-fit/
#https://www.youtube.com/watch?v=NmjT1_nClzg&index=3&list=PLIeGtxpvyG-JmBQ9XoFD4rs-b3hkcX7Uu

#train
model <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial) # Y=vs, X= wt and disp
summary(model)
# Deviance is a measure of goodness of fit of a generalized linear model.
# Or rather, it’s a measure of badness of fit–higher numbers indicate worse fit.
# or our example, we have a value of 43.9 on 31 degrees of freedom. 
# Including the independent variables (weight and displacement) decreased the deviance to 21.4 points on 29 degrees of freedom, 
# a significant reduction in deviance.

#should select the model that has the smallest AIC
#it’s useful for comparing models, but isn’t interpretable on its own.

#new data
newdata = data.frame(wt = 2.1, disp = 180)

#predict
predict(model, newdata, type="response")

#test the model
#Our model appears to fit well because we have no significant difference between the model and the observed data (i.e. the p-value is above 0.05).
# X-squared = chi-square
# df = 10-2 (default group for hoslem = 10 and we have 2 X)
library(ResourceSelection)
h1 = hoslem.test(mtcars$vs, fitted(model))
print(h1)

cbind(h1$observed,h1$expected)

# Plotting the model

model_weight <- glm(formula = vs ~ wt, family = binomial, data = mtcars)
summary(model_weight)
# odds ratio for wt is exp(-1.9105) = 0.1480064

x_val <- seq(0, 6, 0.01)
y_val <- predict(model_weight, list(wt = x_val),type="response")

#plot the weight and VS
plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS") #pch is just the point symbol
lines(x_val,y_val)

