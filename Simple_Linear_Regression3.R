print(dir())
DeathRate <- read.csv("/Users/home/Desktop/UpX/git/Machine_Learning_using_R/DataSet_prob2.csv",header=TRUE,strip.white = TRUE,skipNul = TRUE,skip=1)
print(DeathRate)
#read.table("/Users/home/Desktop/UpX/git/Machine_Learning_using_R/DataSet_prob2.csv",header=TRUE,strip.white = TRUE,skipNul = TRUE)
model1 <- data.frame(DeathRate)
?lm
print(model1[4])
SimpleLRmodel2 = lm(formula = model1$Per.capita.income..in.Euros. ~ model1$Death.Rate.per.1000.residents,data = model1)  
print(SimpleLRmodel2)
summary(SimpleLRmodel2)

#
version

install.packages("FRB")
library("FRB")
data(delivery)
head(delivery)

