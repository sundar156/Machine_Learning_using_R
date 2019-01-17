library(Flury)
data()
data("computers")
View(computers)
help("computers")
plot(computers$Units,computers$Minutes,col='red',xlab=units,ylab='minutes',
     main='plotting in R',pch=16)
abline(6,18,col='blue')
abline(10,12,col='red')  

m1 <- lm(computers$Minutes~computers$Units,data=computers)
print(m1)
summary(m1)
# optimised model is created
abline(4.162,15.09,col='purple') 
b1 <- 4.162
b0 <- 15.09
SSE=sum(((b0+b1 * computers$Units)-computers$Minutes)^2)

b0 <- 6
b1 <- 18
SSE=sum(((b0+b1 * computers$Units)-computers$Minutes)^2)
SSE

pred = b0 + b1*2
pred

pred = b0 + b1*2

#pred = b0 + b1*2 + 
help(glm)
chd = read.csv('chd.csv')
?family
install.packages('C50')
install.packages('partykit')

data('iris')

model <- C50.0(train_df[-5],train_df$Species)

