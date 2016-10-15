library(ggfortify)
library(ggplot2)
library(plotly)

list <- read.csv('nyt-frame.csv')
data <- as.matrix(list)

#Printing out some of the words
only_words <- data[,-1:-8]
print(colnames(only_words[,1:10]))

#Printing out two BOW (Bag of Words)
bow_1 <- as.data.frame(data[1,-1:-8])
bow_2 <- as.data.frame(data[2,-1:-8])
colnames(bow_1)[1] <- "Frequency"
colnames(bow_2)[1] <- "Frequency"

bow_1$Frequency <- sapply(as.character(bow_1$Frequency), as.numeric)
bow_1 <- subset(bow_1, bow_1$Frequency > 0)

bow_2$Frequency <- sapply(as.character(bow_2$Frequency), as.numeric)
bow_2 <- subset(bow_2, bow_2$Frequency > 0)

print(bow_1)
print(bow_2)

#PCA transformation
convert.whole_dataframe_columns <- function(obj, type){
  FUN1 <- switch(type,
                 character = as.character,
                 numeric = as.numeric,
                 factor = as.factor)
  out <- lapply(obj, FUN1)
  as.data.frame(out)
}

data_df <- as.data.frame(data[,c(-1,-3:-8)])

arts_df <- subset(data_df, data_df$class.labels=='art')
music_df <- subset(data_df, data_df$class.labels=='music')

#Arts
arts_df <- as.data.frame(arts_df[,-1])
arts_df <- convert.whole_dataframe_columns(arts_df, "character")
arts_df <- convert.whole_dataframe_columns(arts_df, "numeric")

#Music
music_df <- as.data.frame(music_df[,-1])
music_df <- convert.whole_dataframe_columns(music_df, "character")
music_df <- convert.whole_dataframe_columns(music_df, "numeric")

#Whole
data_df <- as.data.frame(data_df[,-1])
data_df <- convert.whole_dataframe_columns(data_df, "character")
data_df <- convert.whole_dataframe_columns(data_df, "numeric")

pca_arts = prcomp(arts_df)
pca_music = prcomp(music_df)
pca <- prcomp(data_df)


components <- as.data.frame(pca$rotation[,1:2])

components <- components[order(components$PC1,decreasing = TRUE),]
print(head(components[,-2, drop=FALSE], n= 30))

components <- components[order(components$PC2,decreasing = TRUE),]
print(head(components[,-1, drop=FALSE], n= 30))


#2D Plot
visual1 <- as.data.frame(pca_arts$rotation[,1:2])
visual2 <- as.data.frame(pca_music$rotation[,1:2])
visual1$group <- 'Arts'
visual2$group <- 'Music'
visual12 <- rbind(visual1, visual2)
p <- ggplot(visual12, aes(x=PC1, y=PC2, group=group, col=group, fill=group)) + geom_point()
print(p)


#3D Plot
visual1 <- as.data.frame(pca_arts$rotation[,1:3])
visual2 <- as.data.frame(pca_music$rotation[,1:3])
visual1$group <- 'Arts'
visual2$group <- 'Music'
visual12 <- rbind(visual1, visual2)
p <- ggplot(visual12, aes(x=PC1, y=PC2, group=group, col=group, fill=group)) + geom_point()
print(p)

