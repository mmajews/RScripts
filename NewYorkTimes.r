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
