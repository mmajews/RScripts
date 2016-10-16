library(ggplot2)
library(plotly)
data <- read.csv('nyt-frame.csv', sep = ",", dec=".")

#Printing out some of the words
data <- data[,c(-1,-3:-8)]

#Words
print(colnames(data[,2:10]))
matrix <- data.matrix(data)

#Printing out two BOW (Bag of Words)
print(sort(matrix[3,][matrix[3,] > 0], decreasing = FALSE))
print(sort(matrix[4,][matrix[4,] > 0], decreasing = FALSE))

#PCA stuff
pca <- prcomp(matrix[,-1])

components <- as.data.frame(pca$rotation[,1:2])

components <- components[order(components$PC1,decreasing = TRUE),]
print(head(components[,-2, drop=FALSE], n= 30))
components <- components[order(components$PC1,decreasing = FALSE),]
print(head(components[,-2, drop=FALSE], n= 30))

components <- components[order(components$PC2,decreasing = TRUE),]
print(head(components[,-1, drop=FALSE], n= 30))
components <- components[order(components$PC2,decreasing = FALSE),]
print(head(components[,-1, drop=FALSE], n= 30))


visual_data <- cbind(cbind(matrix[,1], matrix[,-1] %*% data.matrix(pca$rotation[,1]), matrix[,-1] %*% data.matrix(pca$rotation[,2]),matrix[,-1] %*% data.matrix(pca$rotation[,3])))
visual_data <- as.data.frame(visual_data)
colnames(visual_data) <- c('group','x','y','z')
visual_data$group[visual_data$group == 1] <- "art"
visual_data$group[visual_data$group == 2] <- "music"

#2D Plot
p <- ggplot(visual_data, aes(x=x, y=y, group=group, col=group, fill=group)) + geom_point()
print(p)

#3D Plot
plot_ly(visual_data, x = ~x, y = ~y, z = ~z, color = ~group) %>% add_markers()



