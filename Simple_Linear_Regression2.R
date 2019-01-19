#Model 0
library(Flury)
data(computers)
minutesmean <- mean(computers$Minutes)
model0 <- data.frame(matrix(data=c(computers$Units,computers$Minutes,
minutesmean*(0*computers$Minutes+1), (minutesmean-computers$Minutes)), ncol=4))
colnames(model0) <- c("Units replaced", "Observed time taken", "Expected value", 
"Expected - Observed value")
print(model0)

print("#Sum of errors for Model 0")
SSE0= sum(minutesmean-computers$Minutes)
print(SSE0)

## -End of Model 0

# - Model 1 Begining

minutesmeanmodel1 <-(10+ (12*(computers$Units)))
print(minutesmeanmodel1)

model1 <- data.frame(matrix(data=c(computers$Units,computers$Minutes,
        minutesmeanmodel1*(0*computers$Minutes+1), (minutesmeanmodel1-computers$Minutes)),ncol=4))
colnames(model1) <- c("Units replaced", "Observed time taken", "Expected value", 
                      "Expected - Observed value")
print(model1)


SSE1 = sum((computers$Minutes-minutesmeanmodel1)^2)

print(SSE1)

## -End of Model 1

# - Model 2 Begining

minutesmeanmodel2 <-(6+ (18*(computers$Units)))
print(minutesmeanmodel1)

model2 <- data.frame(matrix(data=c(computers$Units,computers$Minutes,
                                   minutesmeanmodel2*(0*computers$Minutes+1), (minutesmeanmodel2-computers$Minutes)), ncol=4))
colnames(model2) <- c("Units replaced", "Observed time taken", "Expected value", 
                      "Expected - Observed value")
print(model2)


SSE2 = sum((computers$Minutes-minutesmeanmodel2)^2)

print(SSE2)
print(sqrt(SSE2))

#----- End Model 2


#-Best Fit Liner Regression Model
library("Flury")
data(computers)
# Build the simple linear regression model using lm function
?lm
simpleLRmodel <- lm(formula = Minutes ~ Units , data = computers)
print(simpleLRmodel)
#Calculating Sum of Squared of Errors
#Regression coefficients 
b0 <- simpleLRmodel$coefficients[1] 
b1 <- simpleLRmodel$coefficients[2]
print(simpleLRmodel$coefficients[2] )
SSE <- sum(((b0 + b1 * computers$Units) - computers$Minutes) ^ 2)
print(SSE)
# predict time for replacing 2 units using the lm model

predicted_time <- predict(simpleLRmodel, newdata = data.frame(Units = 2))
print(predicted_time)

#Plotting the best fit model
library(ggplot2)

ggplot() + geom_point(data = computers,aes(x = Units, y = Minutes),size = 2) + 
  geom_abline(intercept = b0,slope = b1) + ggtitle("Best fit model") + 
  theme(plot.title = element_text(hjust = 0.5))
#Summary of the model
summary(simpleLRmodel)

#Accessing R squared directly
#r square is called coefficient of determination

summary(simpleLRmodel)$r.squared
