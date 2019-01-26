#Step 1. Creating dataframe from the data
age <- c(20,26,41,31,36,40,31,46,29,26,32,32,25,37,35,33,18,22,35,29,29,21,34,26,34,34,23,35,25,24,31,26,31,32,33,33,31,20,33,35,28,24,19,29,19,28,34,30,20,26,35,35,49,39,41,58,47,55,52,40,46,48,52,59,35,47,60,49,40,46,59,41,35,37,60,35,37,36,56,40,42,35,39,40,49,38,46,40,37,46,53,42,38,50,56,41,51,35,57,41,35,44,37,48,37,50,52,41,40,58,45,35,36,55,35,48,42,40,37,47,40,43,59,60,39,57,57,38,49,52,50,59,35,37,52,48,37,37,48,41,37,39,49,55,37,35,36,42,43,45,46)
salary <- c(74000,15000,45000,76000,50000,47000,15000,59000,75000,30000, 135000 ,100000,90000,33000,38000,69000,86000,55000,71000 ,148000,47000,88000 ,115000,118000,43000,72000,28000,47000,22000,23000,34000,16000,71000 ,117000,43000,60000,66000,82000,41000,72000,32000,84000,26000,43000,70000,89000,43000,79000,36000,80000,22000,39000,74000 ,134000,71000 ,101000,47000 ,130000, 114000, 142000,22000,96000, 150000,42000,58000,43000 ,108000,65000,78000,96000, 143000,80000,91000, 144000, 102000,60000,53000 ,126000, 133000,72000,80000, 147000,42000, 107000,86000 ,112000,79000,57000,80000,82000, 143000, 149000,59000,88000, 104000,72000,146000,50000 ,122000,52000,97000,39000,52000 ,134000,146000,44000,90000,72000,57000,95000 ,131000,77000,144000, 125000,72000,90000, 108000,75000,74000 ,144000,61000, 133000,76000,42000, 106000,26000,74000,71000,88000,38000,36000,88000,61000,70000,21000, 141000,93000,62000 ,138000,79000,78000, 134000,89000,39000,77000,57000,63000,73000, 112000,79000 ,117000)
purchase <- c("No","No","No","No","No","No","No","No","No","No","Yes","Yes","No","No","No","No","No","No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","Yes","No","Yes","No","Yes","No","Yes","No","Yes","Yes","No","No","No","Yes","No","No","No","Yes","No","Yes","Yes","Yes","No","No","Yes","Yes","No","Yes","Yes","No","Yes","Yes","No","Yes","No","No","No","Yes","Yes","No","Yes","Yes","No","Yes","No","Yes","No","Yes","No","No","Yes","Yes","No","Yes","No","No","Yes","Yes","No","Yes","Yes","No","Yes","Yes","No","No","Yes","No","No","Yes","Yes","Yes","Yes","Yes","No","Yes","Yes","Yes","Yes","No","Yes","Yes","No","Yes","No","Yes","No","Yes","Yes","Yes","Yes","No","No","No","Yes","Yes","No","Yes")
super_market <- data.frame(age,salary,purchase)
#Step 2. normalization
normalize <- function(x){return ((x - min(x))/(max(x) - min(x)) )}
super_market_normalized <- as.data.frame(lapply(super_market[,c(1,2)],normalize))
#Step 3. Splitting the data into training and testing part
set.seed(1235)
training_data_size <- floor(0.70 * nrow(super_market_normalized))
training_index <- sample(1:nrow(super_market_normalized),training_data_size)
#Training data
train_data <- super_market_normalized[training_index,]
head(train_data)
#Testing data
test_data <- super_market_normalized[-training_index,]
train_target <- super_market[training_index,c(3)]
test_target <- super_market[-training_index,c(3)]
#table(test_target)
#Step 4. Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred <- knn(train = train_data, test = test_data, cl = train_target, k = 1)
#Step 5.Making the Confusion Matrix
cm <- table(test_target, y_pred)
print(cm)