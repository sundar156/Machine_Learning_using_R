#Decision Tree
install.packages("installr")
data("iris")
# Step 1: Split the dataset into training and testing part 
set.seed(1436) 
training_index <- sample(1:150, 120) 
training_data <- iris[training_index,] 
testing_data <- iris[-training_index,]
# Sample of training dataset 
head(training_data)
# Step 2: Building the decision tree model
library(C50) 
model <- C5.0(training_data[,-5], training_data$Species) 
print(model)
# Step 3: Calculate the Accuracy of the model
prediction <- predict(object = model, newdata = testing_data) 
actual_class <- testing_data$Species 
#confusion matrix 
addmargins(table(actual_class,prediction))

version
update.packages()
Y
