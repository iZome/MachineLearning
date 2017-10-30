## Libraries and seed
library(pixmap)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(gtools)
## Help functions

#Load all faces from "Faces" folder
load_faces <- function(
    pattern,
    directory
    )
    {
        temp = paste0("Faces/", list.files(path = "Faces", pattern = "*.pgm"))
        #Files are read in alpha order, need to use mixedsort to get
        #right order
        temp <- mixedsort(temp)
        print(temp)
        myfiles = lapply(temp, read.pnm)
        return(myfiles)
    }

#Get and transform the diffrent needed data
get_image_data <- function(
    faces_list,
    n_pictures,
    height,
    width
    )
    {
        image_data <<- array(0, dim = c(n_pictures, height, width))
        image_transpose <<- array(0, dim = c(n_pictures, width, height))
        image_up <<- image_transpose
        image_mean <<- matrix(0, nrow = width, ncol = height)
        image_standard_deviation <<- image_mean
        image_scale <<- image_transpose
        for(i in 1:length(faces_list)){
            image <- faces_list[[i]]@grey
            image_data[i, , ] <<- image
            image_transpose[i, , ] <<- t(image)
        }

        for(i in 1:n_pictures){
            for(j in 1:height){
                image_up[i, , (height - j + 1)] <<- image_transpose[i, , j]
            }
        }

        for(k in 1:width){
            for(l in 1: height){
                image_up_value <- image_up[, k, l]
                image_mean[k,l] <<- mean(image_up_value)
                image_standard_deviation[k,l] <<- sd(image_up_value)
            }
        }

        for(m in 1:n_pictures){
            image_scale[m, , ] <<- (image_up[m  , , ] - image_mean) / image_standard_deviation
        }

    }

#Plot a face to pdf in two different ways
plot_face_pdf <- function(
    gg_melt,
    shades_of_grey = 256
    )
    {
        gg <- ggplot(gg_melt, aes(x = Var1, y = Var2)) +
            geom_tile(aes(fill = value)) +
            theme_classic()
        gg2 <- ggplot(gg_melt, aes(x = Var1, y = Var2)) +
            geom_tile(aes(fill = value)) +
            #scale_fill_gradientn(colours = brewer.pal(shades_of_grey,"Greys")) +
            scale_colour_brewer(palette = brewer.pal(shades_of_grey, "Greys"), direction = -1)
            theme_classic()
        #print(gg)
        print(gg2)
    }

#Plot a face to the wanted folder as png
plot_face_png <-function(
    gg_melt,
    shades_of_grey = 256,
    name
    )
    {
        name <- paste0("Pictures/Task3/", name)
        gg <- ggplot(gg_melt, aes(x = Var1, y = Var2)) +
            geom_tile(aes(fill = value)) +
            scale_fill_gradientn(colours = rev(brewer.pal(shades_of_grey,"Greys"))) +
            theme_classic()
        #Remove all axis and legends from the plot to only view the image
        gg <- gg + theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
        ggsave(name)
    }

#Convert data to input_matrix
convert_to_input_matrix <- function(
    n_faces,
    width,
    height
    )
    {
        X <<- matrix(0, nrow = n_faces, ncol = width * height)
        for(i in 1: n_faces){
            picture <- image_scale[i, 1, ]
            for(j in 2: width){
                picture <- cbind(picture, image_scale[i, j, ])
            }
            X[i, ] <<- picture
        }
        A <<- X %*% t(X)
        store <<- prcomp(A)
        print(str(summary(store)))
        image_eigen <<- t(X) %*% store$rotation
    }

#make a eigen face image
eigen_image <- function(
    face_vector,
    width,
    height
    )
    {
        eigen_face <- matrix(NA, ncol = height, nrow = width)
        for(i in 1: width){
            eigen_face[i, ] <- face_vector[(height * (i - 1) + 1):(height * i)]
        }
        return(eigen_face)
    }

#compress an image
compress_image <- function(
    image,
    eigen_vector,
    R
    )
    {
        b <- rep(0, R)
        height <- dim(image)[2]
        width <- dim(image)[1]
        picture <- image[1, ]
        compress_image <- rep(0, height * width)
        for(i in 2:width){
            picture <- c(picture, image[i, ])
        }

        for(i in 1:R){
            eigen_vector_element <- eigen_vector[, i]
            b_element <- crossprod(picture, eigen_vector_element)
            b[i] <- b_element
            compress_image <- compress_image + (b_element * eigen_vector_element)
        }
        recreate <- eigen_image(compress_image, width, height)
        return(recreate)
    }

plot_different_compressions <- function(
    K_vector,
    image_nr
    )
    {
        pdf("compress_faces.pdf")
            plot_face_pdf(melt(image_scale[image_nr, , ]))
            for(K in K_vector){
                recreate <- compress_image(image_scale[image_nr, , ], image_eigen, K)
                plot_face_pdf(melt(recreate))
            }
        dev.off()
    }

## Main functions

task3i <- function(
    image_nr
    )
    {
        gg_mean <- melt(image_mean)
        gg_standard_deviation <- melt(image_standard_deviation)

        gg_up <- melt(image_up[image_nr, , ])
        gg_scale <- melt(image_scale[image_nr, , ])

        plot_face_png(gg_mean, name = "i/mean.png")
        plot_face_png(gg_standard_deviation, name = "i/sd.png")
        plot_face_png(gg_up, name = "i/org.png")
        plot_face_png(gg_scale, name = "i/scale.png")
    }

task3ii <- function(
    n_eigen_faces,
    width,
    height
    )
    {
        for(i in 1:n_eigen_faces){
            eigen <- image_eigen[, i]
            plot_face_png(melt(eigen_image(eigen, width, height)), name = paste0("ii/eigen_face", i, ".png"))
        }
    }

task3iii <- function(
    image_nr
    )
    {
        eigen_n_vector <- c(1, 5, 50, 220)
        for(K in eigen_n_vector){
            recreate <- compress_image(image_scale[image_nr, , ], image_eigen, K)
            #print(recreate)
            plot_face_png(melt(recreate), name = paste0("iii/image", image_nr, "eigen_faces", K, ".png"))
            #plot_face_png(melt(image_eigen[K, ]), name = paste0("iii/image_test", K, ".png"))
        }
    }

## Run

main <- function()
    {
        # matrices and df
        # image_data
        # image_transpose
        # image_up
        # image_mean
        # image_standard_deviation
        # image_scale
        # X
        # A
        # store
        # image_eigen

        n_faces = 400
        height = 112
        width = 92
        shades_of_grey = 256
        faces <- load_faces()
        #str(head(faces))
        get_image_data(faces, n_faces, height, width)
        convert_to_input_matrix(n_faces, width, height)
        #image_nr = sample(1:n_faces, 1)
        #plot_different_compressions(c(2, 20, 50, 100), image_nr)

        str(names(image_data))
        #task3i(image_nr = 168)
        #task3ii(n_eigen_faces = 10, width = width, height = height)
        #task3iii(image_nr = 168)

    }

main()


#warnings()

## To plot or not to plot
