library(ISLR)
head(College)

df <- College

##plots
library(ggplot2)
ggplot(df, aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private),size=4,alpha=0.5)

ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50,alpha=0.5) + theme_bw()

ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50,alpha=0.6) + theme_bw()

subset(df,Grad.Rate > 100)
df['Cazenovia College','Grad.Rate'] <- 100

###Train/Test
library(caTools)
set.seed(101)

sample <- sample.split(df$Private, SplitRatio = 0.70)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

###
##Train model
library(rpart)
tree <- rpart(Private ~ . ,method='class',data=train)
summary(tree)

tree.preds <- predict(tree,test)
head(tree.preds)
tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if (x >= 0.5){
    return('Yes')
  }else{
    return('No')
  }
}

tree.preds$Private <- sapply(tree.preds$Yes,joiner)
print(head(tree.preds))
table(tree.preds$Private,test$Private)

##
install.packages("rpart.plot")
library(rpart.plot)
prp(tree)
##
#Random Forest
install.packages("randomForest")
library(randomForest)
rf.model <- randomForest(Private ~ ., data=train, importance=TRUE)
rf.model$confusion
rf.model$importance

rf.preds <- predict(rf.model,test)
table(rf.preds,test$Private)
