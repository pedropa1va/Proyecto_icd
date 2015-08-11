library(tm)
library(Snowball)
library(RWeka)
library(rJava)
library(RWekajars)
library(wordcloud)
library(devtools)
library(rCharts)
library(FactoMineR)

# Leyendo posts de user 1 y 2 con codificación ANSI
data <- read.table("data.csv", header = TRUE, sep = ";", row.names = 1, encoding = "ANSI",
                      nrows = 2000)
# Leyendo todos los posts con codificación UTF-8
posts <- read.table("data.csv", header = TRUE, sep = ";", row.names = 1, encoding = "UTF-8")

# Se quitan los posts de user 1 y 2 con UTF-8 para reemplazarlos con el ANSI
posts <- posts[-c(1:2000),]
posts <- rbind(data, posts)

# Ignorando las filas con posts NA
posts <- na.omit(posts)

# División de data frames por usuarios
u1 <- posts[which(posts$id_user == 1),]
u2 <- posts[which(posts$id_user == 2),]
u3 <- posts[which(posts$id_user == 3),]
u4 <- posts[which(posts$id_user == 4),]
u5 <- posts[which(posts$id_user == 5),]

# Corpus de cada usuario
myCorpus1 <- Corpus(VectorSource(u1$post))
myCorpus2 <- Corpus(VectorSource(u2$post))
myCorpus3 <- Corpus(VectorSource(u3$post))
myCorpus4 <- Corpus(VectorSource(u4$post))
myCorpus5 <- Corpus(VectorSource(u5$post))

# Funciones de limpieza
removeURL <- function(x){
  gsub("http[[:alnum:]]*", "", x)
  gsub("www[[:alnum:]]*", "", x)
}
removeLaugh <- function(x){
  gsub("\b(?:a*(?:ja)+j?|(?:l+o+)+l+)\b", "", x)
  gsub("\b(?:a*(?:ha)+h?|(?:l+o+)+l+)\b", "", x)
}

# add extra stop words
myStopwords <- c(stopwords('english'), stopwords('spanish'), "xd", "xD", "like", "RT", "etc",
                 "csm", "para", "ser", "wtf", "sin", "mas", "una", "los", "nos")

# Limpieza de cada corpus
cleanCorpus = function(myCorpus){
  myCorpus <- tm_map(myCorpus, PlainTextDocument)
  # convert to lower case
  myCorpus <- tm_map(myCorpus, tolower, lazy=TRUE)
  # remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation, lazy=TRUE)
  # remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers, lazy=TRUE)
  # remove URLs
  myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE)
  # remove laughter
  myCorpus <- tm_map(myCorpus, removeLaugh, lazy=TRUE)
  # remove stopwords
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords, lazy=TRUE)
}

cleanCorpus(myCorpus1)
cleanCorpus(myCorpus2)
cleanCorpus(myCorpus3)
cleanCorpus(myCorpus4)
cleanCorpus(myCorpus5)

# Stemming words
# keep a copy of corpus to use later as a dictionary for stem
myCorpusCopy <- myCorpus

myCorpus1 <- tm_map(myCorpus1, stemDocument,language="english",lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, stemDocument,language="english",lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, stemDocument,language="english",lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, stemDocument,language="english",lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, stemDocument,language="english",lazy=TRUE)

myStopwords2 <- c("est","esto","xd")

myCorpus1 <- tm_map(myCorpus1, PlainTextDocument)
myCorpus1 <- tm_map(myCorpus1, removeWords, myStopwords2,lazy=TRUE)

myCorpus2 <- tm_map(myCorpus2, PlainTextDocument)
myCorpus2 <- tm_map(myCorpus2, removeWords, myStopwords2,lazy=TRUE)

myCorpus3 <- tm_map(myCorpus3, PlainTextDocument)
myCorpus3 <- tm_map(myCorpus3, removeWords, myStopwords2,lazy=TRUE)

myCorpus4 <- tm_map(myCorpus4, PlainTextDocument)
myCorpus4 <- tm_map(myCorpus4, removeWords, myStopwords2,lazy=TRUE)

myCorpus5 <- tm_map(myCorpus5, PlainTextDocument)
myCorpus5 <- tm_map(myCorpus5, removeWords, myStopwords2,lazy=TRUE)

#para ver que hay en los primeros 500
for (i in 1:500) {
  
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus2[[i]], width=73))
  
}
#Matrices de term document para cada usuario

myTdm1 <- TermDocumentMatrix(myCorpus1,control=list(wordLengths=c(1,Inf))) #Matriz
myTdm2 <- TermDocumentMatrix(myCorpus2,control=list(wordLengths=c(1,Inf)))
myTdm3 <- TermDocumentMatrix(myCorpus3,control=list(wordLengths=c(1,Inf)))
myTdm4 <- TermDocumentMatrix(myCorpus4,control=list(wordLengths=c(1,Inf)))
myTdm5 <- TermDocumentMatrix(myCorpus5,control=list(wordLengths=c(1,Inf)))


#myTdm <- TermDocumentMatrix(myCorpus,control=list(minWordLength=1))
inspect(myTdm1)
# inspect frequent words por cada usuario
findFreqTerms(myTdm1, lowfreq=25)
termFrequency1 <- rowSums(as.matrix(myTdm1))
termFrequency1 <- subset(termFrequency1, termFrequency1>=15)

findFreqTerms(myTdm2, lowfreq=25)
termFrequency2 <- rowSums(as.matrix(myTdm2))
termFrequency2 <- subset(termFrequency2, termFrequency2>=15)


findFreqTerms(myTdm3, lowfreq=25)
termFrequency3 <- rowSums(as.matrix(myTdm3))
termFrequency3 <- subset(termFrequency3, termFrequency3>=15)


findFreqTerms(myTdm4, lowfreq=25)
termFrequency4 <- rowSums(as.matrix(myTdm4))
termFrequency4 <- subset(termFrequency4, termFrequency4>=15)


findFreqTerms(myTdm5, lowfreq=25)
termFrequency5 <- rowSums(as.matrix(myTdm5))
termFrequency5 <- subset(termFrequency5, termFrequency5>=15)



#Impresion Grafica usuario 1
barplot(termFrequency1, las=2)
library(wordcloud)
m <- as.matrix(myTdm1)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,random.order=F, colors=grayLevels)


# remove sparse terms usuario 1
myTdmAux <- removeSparseTerms(myTdm1, sparse=0.985) 
m2 <- as.matrix(myTdmAux)
frequency <- colSums(m2)
frequency <- sort(frequency, decreasing=TRUE)
#frequency
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="single")
plot(fit)
# cut tree into 9 clusters
rect.hclust(fit, k=5)
groups <- cutree(fit, k=5)
#groups

#write.csv(groups, file = "MyGroups.csv") #-->van a carpeta personal

# count frequency 
temp <- inspect(myTdmAux)
FreqMat1 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat1) <- NULL
FreqMat1
#FreqMat2 <- FreqMat

#FreqMat2$grupos <- groups

# Ahora exportamos el archivo FreqMat2 que tiene las palabras con su grupo y la frecuencia
#write.csv(FreqMat2, file = "grupos.csv")


# USUARIO 2

#Impresion Grafica usuario 2
barplot(termFrequency2, las=2)
library(wordcloud)
m <- as.matrix(myTdm2)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,random.order=F, colors=grayLevels)


# remove sparse terms usuario 2
myTdmAux <- removeSparseTerms(myTdm2, sparse=0.985) 
m2 <- as.matrix(myTdmAux)
frequency <- colSums(m2)
frequency <- sort(frequency, decreasing=TRUE)
frequency
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="single")
plot(fit)
# cut tree into 9 clusters
rect.hclust(fit, k=5)
groups <- cutree(fit, k=5)
#groups

#write.csv(groups, file = "MyGroups.csv") #-->van a carpeta personal

# count frequency 
temp <- inspect(myTdmAux)
FreqMat2 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat2) <- NULL
FreqMat2
#FreqMat2 <- FreqMat

#FreqMat2$grupos <- groups

# Ahora exportamos el archivo FreqMat2 que tiene las palabras con su grupo y la frecuencia
#write.csv(FreqMat2, file = "grupos.csv")


# USUARIO 3


#Impresion Grafica usuario 3
barplot(termFrequency3, las=2)
library(wordcloud)
m <- as.matrix(myTdm3)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,random.order=F, colors=grayLevels)


# remove sparse terms usuario 3
myTdmAux <- removeSparseTerms(myTdm3, sparse=0.985) 
m2 <- as.matrix(myTdmAux)
frequency <- colSums(m2)
frequency <- sort(frequency, decreasing=TRUE)
frequency
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="single")
plot(fit)
# cut tree into 9 clusters
rect.hclust(fit, k=5)
groups <- cutree(fit, k=5)
#groups

#write.csv(groups, file = "MyGroups.csv") #-->van a carpeta personal

# count frequency 
temp <- inspect(myTdmAux)
FreqMat3 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat3) <- NULL
FreqMat3
#FreqMat2 <- FreqMat

#FreqMat2$grupos <- groups

# Ahora exportamos el archivo FreqMat2 que tiene las palabras con su grupo y la frecuencia
#write.csv(FreqMat2, file = "grupos.csv")




# USUARIO 4


#Impresion Grafica usuario 4
barplot(termFrequency4, las=2)
library(wordcloud)
m <- as.matrix(myTdm4)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,random.order=F, colors=grayLevels)


# remove sparse terms usuario 4
myTdmAux <- removeSparseTerms(myTdm4, sparse=0.985) 
m2 <- as.matrix(myTdmAux)
frequency <- colSums(m2)
frequency <- sort(frequency, decreasing=TRUE)
frequency
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="single")
plot(fit)
# cut tree into 9 clusters
rect.hclust(fit, k=5)
groups <- cutree(fit, k=5)
#groups

#write.csv(groups, file = "MyGroups.csv") #-->van a carpeta personal

# count frequency 
temp <- inspect(myTdmAux)
FreqMat4 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat4) <- NULL
FreqMat4
#FreqMat2 <- FreqMat

#FreqMat2$grupos <- groups

# Ahora exportamos el archivo FreqMat2 que tiene las palabras con su grupo y la frecuencia
#write.csv(FreqMat2, file = "grupos.csv")



# USUARIO 5

#Impresion Grafica usuario 5
barplot(termFrequency5, las=2)
library(wordcloud)
m <- as.matrix(myTdm5)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,random.order=F, colors=grayLevels)


# remove sparse terms usuario 5
myTdmAux <- removeSparseTerms(myTdm5, sparse=0.985) 
m2 <- as.matrix(myTdmAux)
frequency <- colSums(m2)
frequency <- sort(frequency, decreasing=TRUE)
frequency
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="single")
plot(fit)
# cut tree into 9 clusters
rect.hclust(fit, k=5)
groups <- cutree(fit, k=5)
#groups

#write.csv(groups, file = "MyGroups.csv") #-->van a carpeta personal

# count frequency 
temp <- inspect(myTdmAux)
FreqMat5 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat5) <- NULL
FreqMat5
#FreqMat2 <- FreqMat

#FreqMat2$grupos <- groups

# Ahora exportamos el archivo FreqMat2 que tiene las palabras con su grupo y la frecuencia
#write.csv(FreqMat2, file = "grupos.csv")






