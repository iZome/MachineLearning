
library(randomForest)
library(doParallel)

titanic <- read.csv("titanic.csv", header = T)

titanic$Pclass <- as.factor(titanic$Pclass)

trees <- 20000

bag <- randomForest(Survived ~ ., data = titanic, mtry=7, ntree = trees, importance = TRUE, na.action = na.exclude, do.trace = 10)

str(bag)

head(bag$err.rate)

plot(bag$err.rate[,"OOB"], type = "l", xlab = "Number of trees", ylab = "OOB error")

varImpPlot(bag, type = 2)


rf <- randomForest(Survived ~ ., data=titanic, ntree = trees, importance = TRUE, na.action = na.exclude, do.trace = 100)

rf2 <- randomForest(Survived ~ ., data=titanic, ntree = trees, importance = FALSE, na.action = na.exclude, do.trace = 100)

str(rf)

str(rf2)

plot(rf$err.rate[,"OOB"], type = "l", xlab = "Number of trees", ylab = "OOB error")
lines(bag$err.rate[,"OOB"], col = "red")
lines(rf2$err.rate[,"OOB"], col = "blue")

cl <- makeCluster(3)
registerDoParallel(cl)

system.time(
  rf_par <- foreach(i = 1:3, .combine="combine", .packages="randomForest") %dopar% {
    randomForest(Survived ~ ., data=titanic, ntree=2000, importance=TRUE, na.action=na.exclude)
  }
)

str(rf_par)

stopCluster(cl)

varImpPlot(rf_par)
