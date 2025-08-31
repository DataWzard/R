library(neuralnet)
library(caTools)
library(randomForest)
df <- read.csv("C:\\Users\\jstac\\Downloads\\BankNote_Authentication.csv")
print(head(df))

set.seed(101)

split <- sample.split(df$class, SplitRatio = 0.7)

train <- subset(df, split==TRUE)
test <- subset(df, split==FALSE)

nn <- neuralnet(class ~ variance + skewness + curtosis + entropy,data = train,hidden = c(5,3),linear.output = FALSE)
predicted.nn.values <- compute(nn,test[1:4])
head(predicted.nn.values$net.result)

predictions <- sapply(predicted.nn.values$net.result,round)
head(predictions)

table(predictions,test$class)

#randomForest
df$class <- factor(df$class)

set.seed(101)

split <- sample.split(df$class, SplitRatio = 0.7)

train <- subset(df, split==TRUE)
test <- subset(df, split==FALSE)

rf.model <- randomForest(class ~ .,data=train)
rf.pred <- predict(rf.model,test)
table(rf.pred,test$class)
