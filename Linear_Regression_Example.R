
library(Flury)
data()
data("computers")
View(computers)
print("computers Dataset")
?sum
mean_ = sum(computers$Minutes) / sum(computers$Units)
print(sum(computers$Units))
print(computers,row.names = F)
print(sum(computers$Minutes)/14)

#---

# Create a vector as input.
data <- c("East","West","East","North","North","East","West","West","West","East","North")

print(data)
print(is.factor(data))

# Apply the factor function.
factor_data <- factor(data)

print(factor_data)
print(is.factor(factor_data))

#------------
?c
?factor
tst= c(1,2,4,6,8)
mrk=c(45,60,78,90,80)
subj = c ('Eng','Mat','Sci','Phy','che')
?factor
mons= cut(mrk,breaks=c(0,50,80,100),labels=c('cat1','cat2','cat3'),right=FALSE)
print(mons)
table(mons)
#Plot of Model 0, 1 and 2 
models <- factor(c("Model 0", "Model 1", "Model 2","Model 1"),ordered = FALSE)
print(models)
levels(models)[1]="Model 0"
?`levels<-.factor`
levels(models)
slopes <- c(0,12,18)
intercepts <- c(mean(computers$Minutes),10,6)
Models <- data.frame(models, slopes, intercepts)
library(ggplot2)
ggplot() + geom_point(data = computers,aes(x = Units, y = Minutes),size = 2) +
  geom_abline(data = Models, aes(slope = slopes, intercept = intercepts, col = models),lwd = 1.2) +
  ggtitle("Speculated models") + theme(plot.title = element_text(hjust = 0.5))
#-------------

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

