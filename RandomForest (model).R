# importing data

data <- read.csv("Cardiotocographic.csv", TRUE, ",")

# cheacking structure of the data

str(data)

# creating NSP variable a factor variable

data$NSP <- as.factor(data$NSP)

# Splitting the train and test data

library(caTools)
set.seed(123)

split <- sample.split(data$NSP, SplitRatio = .75)

train <- subset(data, split == T)
test <- subset(data, split == F)

# Creating the random forest model

library(randomForest)

model <- randomForest(NSP~., data= train,
                      ntree= 320,
                      mtry= 9,
                      importance= T,
                      proximity= T)
print(model)

# looking at the attributes of the model

attributes(model)

# Confusion Matrix

library(caret)
library(e1071)

# cheaking the accuracy for the training data

p1 <- predict(model, train)

confusionMatrix(p1, train$NSP) 

#cheacking the accuracy for the testing data

p2 <- predict(model, test)

confusionMatrix(p2, test$NSP)

# Error rate analysing for selecting the optimal Mtree

plot(model)

# Mtry analysing for selecting the optimal Mtry

tune <- tuneRF(train[,-22], train[,22],
               stepFactor = .7,
               improve = .05,
               plot = T,
               ntreeTry = 320,
               trace = T)

# No. of nodes

nodes <- hist(treesize(model),
              main = "No. of nodes",
              col = "blue")

#Variable Importance

varImpPlot(model,
           sort = T,
           n.var = 10)
importance(model)
varUsed(model)
MDSplot(model, train$NSP)
MDSplot(model, test$NSP)