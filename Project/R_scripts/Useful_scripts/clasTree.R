
library(tree)

titanic <- read.csv("titanic.csv", header = T)
head(titanic)

titanic$Pclass <- as.factor(titanic$Pclass)

tree.titanic <- tree(Survived ~ ., data=titanic, split = "deviance")

summary(tree.titanic)

plot(tree.titanic)
text(tree.titanic,cex = 0.9, pretty = 1)

set.seed(210)
crossval <- cv.tree(tree.titanic, FUN = prune.misclass)

plot(crossval$size, crossval$dev, type = "b", xlab = "Number of terminal nodes", ylab = "CV error")

crossval$k[1]<-0

alpha <- round(crossval$k,1)
axis(3, at = crossval$size, lab = alpha, cex = 0.8)
mtext(expression(alpha), 3, line=2.5, cex =1.2)

tree.pruned <- prune.misclass(tree.titanic, best=4)
plot(tree.pruned)
text(tree.pruned, pretty=1)
