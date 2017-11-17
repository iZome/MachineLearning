library(colorRamps)
library(ggplot2)
library(reshape2)
set.seed(1234)

#Please check all libraries are available before running

current_directory <- getwd()

train_path <- paste0(current_directory, "/data/Train_Digits_20171108.csv")
test_path <- paste0(current_directory, "/data/Test_Digits_20171108.csv")

train_df <- read.csv(train_path)
print(train_df[(1:4), (1:10)])
print(names(train_df))
test_df <- read.csv(test_path)

random_categories <- sample.int(10,4) - 1 # sample numbers between 0 and 9
random_categories <- c(2,3,4,8)

train_df <- train_df[train_df$Digit %in% random_categories,]
test_df <- test_df[test_df$Digit %in% random_categories,]

# Plot one piece of clothing with it's category in the name
plot_clothing <- function(
    id = 1,
    fashion_vector,
    category
    )
    {
        pic <- matrix(0L, nrow = 28, ncol = 28)

        for(i in 1:784){
            pic[floor(i/28), i %% 28] <- fashion_vector[ i + 1]
        }

        image(z = pic, col = matlab.like(10))
        gg <- melt(pic)
        ggplot(gg,aes(x=Var1,y=Var2))+
            geom_tile(aes(fill = value)) +
            scale_fill_gradient(low = "white", high = "black", limits = c(0,255)) +
            labs(x=expression(italic("column")), y=expression(italic("row")),
                 title= paste("Fashion clothing:", category)) +
            theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
            axis.text.y=element_text(size=11),
            plot.title=element_text(size=11)) +
            coord_flip() +
            scale_x_reverse()
        ggsave(paste0("results/digit_pictures/picture", category, id, ".png"))
    }

# Currently plot 7 pictures from each of the selected categories
plot_N_cloths <- function(
    N = 7,
    random_indexes = FALSE
    )
    {
        category_labels <- c(0,1,2,3,4,5,6,7,8,9)
        category_names <- c("T-shirt/top", "Trousers", "Pullover", "Dress", "Coat",
                            "Sandal", "Shirt", "Sneakers", "Bag", "Ankle")
        category_df <- data.frame(category_labels, category_names)

        #making vector with 7 pictures from each category
        pec <- 7 # pictures each category
        print(train_df[(1:4),(1:10)])
        id_vec <- sample(which(train_df[,1] == random_categories[1]),pec)
        id_vec <- c(id_vec, sample(which(train_df[,1] == random_categories[2]),pec))
        id_vec <- c(id_vec, sample(which(train_df[,1] == random_categories[3]),pec))
        id_vec <- c(id_vec, sample(which(train_df[,1] == random_categories[4]),pec))
        print(id_vec)
        print(train_df[id_vec,1:9])

        for(i in id_vec){
            fashion_vector <- as.integer(train_df[i, ])
            print(category_df[fashion_vector[1]+1,])
            category <- category_df[which(category_labels == fashion_vector[1]),"category_names"]
            plot_clothing(i, fashion_vector, category)
        }
    }

plot_N_cloths(7)
