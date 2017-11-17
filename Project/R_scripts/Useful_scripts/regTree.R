library(tree)


calif <- read.csv("cadata.csv")

head(calif)

fulltree <- tree(log(HousePrice) ~ Longitude + Latitude, data = calif)

str(fulltree)

summary(fulltree)

plot(fulltree)
text(fulltree, cex = 0.8)

partition.tree(fulltree, ordvars = c("Longitude", "Latitude"))

stopcrit <- tree.control(nobs=nrow(calif), mincut = 5, minsize = 10, mindev = 0.00432)

bigtree <- tree(log(HousePrice) ~., data = calif, control = stopcrit)

crossval <- cv.tree(fulltree)

crossval$k[1] <- 0

plot(crossval$size, crossval$dev, type = "b")

tree.prune <- prune.tree(fulltree, best = 6)
plot(tree.prune)
text(tree.prune, cex = 0.9)
