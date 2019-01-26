data(faithful)
head(faithful)

library(ggplot2)

ggplot(data=faithful,aes())
  ggplot() + geom_point(data = faithful,aes(x = eruptions, y = waiting),size = 2) +  ggtitle("Plot of faithful") 
+ theme(plot.title = element_text(hjust = 0.5))
?geom_point()
  
  #--------
set.seed(123)
training_data_size <- floor(0.75*nrow(faithful))
?sample()
faithful_training_index <- sample(1:nrow(faithful),training_data_size)
#Training data
faithful_train <- faithful[faithful_training_index,]

#Testing data
faithful_test <- faithful[-faithful_training_index,]
faithful_model <- lm(waiting ~ eruptions,data=faithful)
pred <- predict(faithful_model,faithful_test)


percentage_error <- ((pred - faithful_test$waiting)*100 )/faithful_test$waiting
print("Error")
print(mean(abs(percentage_error)))
print("Prediction Accuracy")
print(100-mean(abs(percentage_error)))
  
b0 <- faithful_model$coefficients[1]
b1 <- faithful_model$coefficients[2]
ggplot() + geom_point(data=faithful_test,aes(x=eruptions,y=waiting),size=2) + 
  geom_abline(intercept = b0,slope=b1,col='red',lwd=2)+ggtitle("Plot of faithful") + 
  theme(plot.title = element_text(hjust=0.5))
summary(faithful_model)$r.squared

#data(mtcars)

# Use mtcars dataset in R to calculate and compare the sum of square errors of linear regression
# models plotted for "mpg" vs. "hp" and "mpg" vs. "qsec".
library(ggplot2)
ggplot(mtcars, aes(hp,mpg)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Miles per Gallon") +
  xlab("No. of Horsepower") +
  ggtitle("Impact of Number of Horsepower on MPG")
m1 <- lm(mtcars$mpg~mtcars$hp,data=mtcars)
summary(m1)

abline(30.09,-0.06,col='purple') 
b1 <- 30.09
b0 <- 1.63
SSE=sum(((b0+b1 * computers$Units)-computers$Minutes)^2)
print(SSE)

#----
Age<- c(19,35,26,27,19,27)
salary <- c(19000,20000,43000,57000,76000,58000)
Purchase <- c(0,0,0,0,0,0)
data_set=data.frame(Age,salary,Purchase)
print(1:(nrow(data_set)))
#print(data.frame(Age,salary,Purchase))
?c()
library(ggplot2)
ggplot(data=data_set,aes(x=Age,y=Purchase))+ geom_point() + ggtitle("Scatterplot - Age vs Purchase") 
+ theme(plot.title = element_text(hadjust=0.5))
