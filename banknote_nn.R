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

med_curt <- median(train$curtosis, na.rm = TRUE)
med_entr <- median(train$entropy,  na.rm = TRUE)

grid <- expand.grid(
  variance = seq(min(df$variance), max(df$variance), length.out = 200),
  skewness = seq(min(df$skewness), max(df$skewness), length.out = 200)
)
grid$curtosis <- med_curt
grid$entropy  <- med_entr

nn_grid_prob <- compute(nn, grid[, c("variance","skewness","curtosis","entropy")])$net.result
grid$nn_prob  <- as.numeric(nn_grid_prob)
grid$nn_class <- ifelse(grid$nn_prob >= 0.5, 1, 0)

#plot nn
ggplot() +
  geom_raster(data = grid, aes(variance, skewness, fill = nn_prob), alpha = 0.7, interpolate = TRUE) +
  geom_contour(data = grid, aes(variance, skewness, z = nn_prob), breaks = 0.5, color = "black") +
  geom_point(data=test,aes(variance,skewness,color=factor(class),shape=factor(class)),size=2,alpha=0.85)+
  scale_color_manual(values=c("0"="steelblue","1"="forestgreen"))+
  scale_fill_gradientn(colors=c("lightgray","lightyellow","lightcoral"), values=scales::rescale(c(0,0.5,1)), name="P(class=1)") +
  labs(title = "Neural net decision map (variance vs skewness)", x = "variance", y = "skewness", color = "true class", shape = "true class") 

#randomForest
df$class <- factor(df$class)

set.seed(101)

split <- sample.split(df$class, SplitRatio = 0.7)

train <- subset(df, split==TRUE)
test <- subset(df, split==FALSE)

rf.model <- randomForest(class ~ .,data=train)
rf.pred <- predict(rf.model,test)
table(rf.pred,test$class)
