
library(gbm)

data <- read.csv("cadata.csv")

logHousePrice <- log(data$HousePrice)

AveRooms <- data$Rooms/data$Households

Avebedrms <- data$Bedrooms/data$Households

AveOccup <- data$Population/data$Households

calif <- data.frame(logHousePrice, Income=data$Income, HouseAge = data$HouseAge, AveRooms, Avebedrms, AveOccup, Population = data$Population, Latitude = data$Latitude, Longitude = data$Longitude)

head(calif)

system.time(
  boost <- gbm(logHousePrice ~., data=calif, distribution = "gaussian", n.trees = 5000, interaction.depth = 2, shrinkage = 0.001, bag.fraction = 1, cv.folds = 10, n.cores = 4)
)

attributes(boost)


plot(boost$cv.error, type = "l")

summary(boost)
