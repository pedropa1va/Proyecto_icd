library(tm)
library(Snowball)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(wordcloud)
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

# Para ver qué hay dentro del corpus
inspect(myCorpus1)
inspect(myCorpus1[1])
writeLines(as.character(myCorpus1[[1]]))

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
                 "csm", "para", "ser", "wtf", "mas", "una", "nos", "est","esto", "este")
skipWords <- function(x) removeWords(x, myStopwords)

# Stemming words
# por alguna no razón no agarra el spanish... Igual no creo que sea necesario hacer stemming
#stemDoc_es <- function(x) stemDocument(x, language = meta(x, "spanish"))
#stemDoc_es <- function(x) stemDocument(x, language ="spanish")

cleanFuns <- list(PlainTextDocument, tolower, removePunctuation, removeNumbers, removeURL,
                  removeLaugh, stemDocument, skipWords)

# Limpieza de cada corpus
myCorpus1 <- tm_map(myCorpus1, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus2 <- tm_map(myCorpus2, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus3 <- tm_map(myCorpus3, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus4 <- tm_map(myCorpus4, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus5 <- tm_map(myCorpus5, FUN = tm_reduce, tmFuns = cleanFuns)

# Para ver que hay en los primeros 500 documentos
for (i in 1:500) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus1[[i]], width=85))
}

# Matriz Term-Document para cada usuario
myTdm1 <- TermDocumentMatrix(myCorpus1, control=list(wordLengths=c(2,Inf)))
myTdm2 <- TermDocumentMatrix(myCorpus2, control=list(wordLengths=c(2,Inf)))
myTdm3 <- TermDocumentMatrix(myCorpus3, control=list(wordLengths=c(2,Inf)))
myTdm4 <- TermDocumentMatrix(myCorpus4, control=list(wordLengths=c(2,Inf)))
myTdm5 <- TermDocumentMatrix(myCorpus5, control=list(wordLengths=c(2,Inf)))

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




# USUARIO 1

#Impresion Grafica usuario 1
barplot(termFrequency1, las=2)
m <- as.matrix(myTdm1)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
wordcloud(words = names(wordFreq), freq = wordFreq, min.freq = 8, random.order = FALSE,
          colors = c("grey","dark grey","blue","green","orange","red"))

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

# count frequency 
temp <- inspect(myTdmAux)
FreqMat1 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat1) <- NULL
FreqMat1




# USUARIO 2

#Impresion Grafica usuario 2
barplot(termFrequency2, las=2)
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

# count frequency 
temp <- inspect(myTdmAux)
FreqMat2 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat2) <- NULL
FreqMat2




# USUARIO 3


#Impresion Grafica usuario 3
barplot(termFrequency3, las=2)
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

# count frequency 
temp <- inspect(myTdmAux)
FreqMat3 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat3) <- NULL
FreqMat3




# USUARIO 4


#Impresion Grafica usuario 4
barplot(termFrequency4, las=2)
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

# count frequency 
temp <- inspect(myTdmAux)
FreqMat4 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat4) <- NULL
FreqMat4




# USUARIO 5

#Impresion Grafica usuario 5
barplot(termFrequency5, las=2)
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

# count frequency 
temp <- inspect(myTdmAux)
FreqMat5 <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat5) <- NULL
FreqMat5


