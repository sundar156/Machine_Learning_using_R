
#Day1

var <- 10
var

class(var)
var1 <- T
var1

class(var1)

# Vector - homogeneous collection of scalar object

v1 = 1:9
v1

v2 <- c(1,2,8,5,7,9)
v2

class(v2)

v2[2]
v2[2:5]
v2>=7
v2[v2>=7]


# List – heterogeneous collection of data


l1 <- list(a = 1,b = 6,c = 'r',d = T)
class(l1)

l1$d

l1$a


# Data Frame - Table in R

v1 <- c('jack', 'tom', 'james', 'jill')
v2 <- c(98,95,89,79)
v3 <- c(90,85,69,98)
v4 <- c(79,85,90,97)


df <- data.frame(v1,v2,v3,v4)
df

View(df)
colnames(df) <-c('Names', 'sci', 'eng', 'maths')
View(df)


df[3,3]

df[4,2]
df[2,]


df[,3]
df['eng']
df[3]


df[-4,]



install.packages("Flury")
library(Flury)

data()
data("computers")


View(computers)

help("computers")
?vector
example("vector")


getwd()
setwd('D:/Data Science/EML_R')
getwd()
dir()
ad <- read.csv(file = "adult.csv", header = F )
View(ad)

?read.csv


data()

# Plotting

View()

# plot(x,y, col, xlab, ylab, pch)

plot(computers$Units, computers$Minutes,
     col = 'red', xlab = 'Units', ylab = 'Minutes',
     main = 'Ploting with R', pch = 16)
?pch


dir()

salary <- read.csv("Salary_Data.csv")
View(salary)

plot(salary$YearsExperience, salary$Salary,
     col = 'red', xlab = 'YearsExperience', ylab = 'Salary',
     main = 'Ploting with R', pch = 16)


SLR - one feature variable - pridictor
linear Relation
O/p variable - cont. numeric value


# Simple linear Model

#lm(dependent var ~ Independent var, data = dataset)

model <- lm(Salary ~ YearsExperience, data= salary ) 
summary(model)     


abline(intercept, slope, col)
abline(25792.2, 9450.0, col = 'blue')     

# predict
# pred = b0 +b1X1
pred = 25792.2 + 9450.0*20
pred



dir()
br <- read.csv( "breakfast_data.csv",sep ='\t')
View(br)

plot(br)

# mULTIPLE linear Model

#lm(dependent var ~ Independent var+IV+IV, data = dataset)

model <- lm(rating ~., data = br ) 
summary(model)     


# predict
# pred = b0 +b1X1 +b2X2 +b3X3 +b4X4+ b5X5



# ML process
# Historical Data
br <- read.csv( "breakfast_data.csv",sep ='\t')
View(br)
plot(br)

# Splitting of data - 80-20
sp <- sample(1:nrow(br), 0.8*nrow(br))
sp
train_df <- br[sp,]
test_df <- br[-sp,]

# model

# model <- lm(rating ~protein+fat+fiber+carbo+sugars, data = train_df ) 
model <- lm(rating ~., data = train_df ) 
summary(model) 

# Model validation
# predict(model, test data)
pred_values <- predict(model, test_df)

pred_values
test_df$rating

# Error
error <- test_df$rating - pred_values
error <- (error*100)/test_df$rating
error_mean <- mean(abs(error))
error_mean
# Accuracy

acc <-100- error_mean
acc

# Day 2
# Logistic Regression

getwd()
dir()
chd <- read.csv("chd.csv")
View(chd)

# Historical data
chd <- read.csv("chd.csv")
View(chd)
plot(chd)

# Splitting the data into train and test data (80-20)
#set.seed(12)
sp <- sample(1:nrow(chd), 0.8*nrow(chd))  #1- 100, 80% = 80
sp
train <- chd[sp,]
test <- chd[-sp,]

# model creation
model <- glm(chd ~ age , data = train, family = 'binomial')
summary(model)


# Predict
pred <- predict(model, test, type = 'response') 
# type = 'response'- as we want probability instead of y value
pred

# setting  threshold 0.5
def <- rep(0,nrow(test))
def
def[pred>0.5] <-1
def

# validation
cf <- table(test$chd, def)
cf
# accuracy
acc <- ((cf[1]+cf[4])/nrow(test))*100
acc




# Decision Tree
install.packages('C50')  # for model
library(C50)
install.packages('partykit')  # for visualization of tree
library(partykit)


# Historical data
data(iris)
View(iris)

# Splitting the data into train and test data (80-20)
#set.seed(12)
sp <- sample(1:nrow(iris), 0.8*nrow(iris))  #1- 150, 80% = 120
sp
train <- iris[sp,]
test <- iris[-sp,]

# model
# c5.0(Features, Target)

model <- C5.0(train[,-5], train$Species)
summary(model)
plot(model, type = "simple")

# Predict
pred <- predict(model, test)
pred


# validation

cf <- addmargins(table(test$Species, pred))
cf
# accuracy
acc <- ((cf[1]+cf[6]+cf[11])/cf[16])*100
acc



# clustering


new <- iris
# Actual data
plot(new[c("Petal.Length", "Petal.Width")], col = new$Species)

# clustering the data
kc <- kmeans(new[-5], 3)
kc
# Groups created by clustering
plot(new[c("Petal.Length", "Petal.Width")], col = kc$cluster)

# Visualizing the confusion matrix
table(new$Species, kc$cluster)