library(ggplot2)
library(caTools)

df1 <- read.csv("C:\\Users\\jstac\\Downloads\\wine_quality\\winequality-red.csv", sep=';')
df2 <- read.csv("C:\\Users\\jstac\\Downloads\\wine_quality\\winequality-white.csv", sep=';')
print(head(df1))

df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})
print(head(df1))
print(head(df2))

wine <- rbind(df1,df2)

pal_wine <- c(red="#ae4554", white="#faf7ea")
scales_wine <- list(
  scale_color_manual(values=pal_wine),
  scale_fill_manual(values=pal_wine)
)


###histograms
plrs <- ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill=label), color='black', bins=50)

plrs + scales_wine + theme_bw()

plca <- ggplot(wine,aes(citric.acid)) + geom_histogram(aes(fill=label), color='black', bins=50)

plca + scales_wine + theme_bw()

pla <- ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=label), color='black', bins=50)

pla + scales_wine + theme_bw()

##plot
plcars <- ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(color=label),alpha=0.2)

plcars + scales_wine + theme_dark()

plvars <- ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(color=label),alpha=0.2)

plvars + scales_wine + theme_dark()

#kmeans cluster
clus.data <- wine[,1:12]
head(clus.data)

wine.cluster <- kmeans(clus.data,3)
print(wine.cluster$centers)

table(wine$label,wine.cluster$cluster)
