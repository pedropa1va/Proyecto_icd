library(tm)
library(Snowball)
library(RWeka)
library(rJava)
library(RWekajars)
library(wordcloud)
library(devtools)
library(rCharts)
library(FactoMineR)
posts <- read.table("data2.csv",header=TRUE,sep=";",dec=",",row.names=1)
#Split Data frames por usuarios
u1 <- posts[which(posts$id_user == 1),]
u2 <- posts[which(posts$id_user == 2),]
u3 <- posts[which(posts$id_user == 3),]
u4 <- posts[which(posts$id_user == 4),]
u5 <- posts[which(posts$id_user == 5),]
#Data Frame con los posts de cada usuario
df1 <- do.call("rbind", lapply(u1$post, as.data.frame))
df2 <- do.call("rbind", lapply(u2$post, as.data.frame))
df3 <- do.call("rbind", lapply(u3$post, as.data.frame))
df4 <- do.call("rbind", lapply(u4$post, as.data.frame))
df5 <- do.call("rbind", lapply(u5$post, as.data.frame))
# Corpus y limpieza de cada data frame de cada usuario
myCorpus1 <- Corpus(VectorSource(df1$X))
myCorpus2 <- Corpus(VectorSource(df2$X))
myCorpus3 <- Corpus(VectorSource(df3$X))
myCorpus4 <- Corpus(VectorSource(df4$X))
myCorpus5 <- Corpus(VectorSource(df5$X))

tm_map(myCorpus1, function(x) iconv(enc2utf8(x), sub = "byte")) # problemas con el utf-8 en las pos impares
tm_map(myCorpus2, function(x) iconv(enc2utf8(x), sub = "byte"))
tm_map(myCorpus3, function(x) iconv(enc2utf8(x), sub = "byte"))
tm_map(myCorpus4, function(x) iconv(enc2utf8(x), sub = "byte"))
tm_map(myCorpus5, function(x) iconv(enc2utf8(x), sub = "byte"))

myCorpus1 <- tm_map(myCorpus1, PlainTextDocument)
myCorpus2 <- tm_map(myCorpus2, PlainTextDocument)
myCorpus3 <- tm_map(myCorpus3, PlainTextDocument)
myCorpus4 <- tm_map(myCorpus4, PlainTextDocument)
myCorpus5 <- tm_map(myCorpus5, PlainTextDocument)
# convert to lower case
myCorpus1 <- tm_map(myCorpus1, tolower,lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, tolower,lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, tolower,lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, tolower,lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, tolower,lazy=TRUE)

# remove punctuation
myCorpus1 <- tm_map(myCorpus1, removePunctuation,lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, removePunctuation,lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, removePunctuation,lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, removePunctuation,lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, removePunctuation,lazy=TRUE)

# remove numbers
myCorpus1 <- tm_map(myCorpus1, removeNumbers,lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, removeNumbers,lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, removeNumbers,lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, removeNumbers,lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, removeNumbers,lazy=TRUE)

# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
removeURL2 <- function(x) gsub("www[[:alnum:]]*", "", x)
removejaja <- function(x) gsub("\b(?:a*(?:ja)+j?|(?:l+o+)+l+)\b", "", x)
removehaha <- function(x) gsub("\b(?:a*(?:ha)+h?|(?:l+o+)+l+)\b", "", x)

myCorpus1 <- tm_map(myCorpus1, removeURL,lazy=TRUE)
myCorpus1 <- tm_map(myCorpus1, removeURL2,lazy=TRUE)
myCorpus1 <- tm_map(myCorpus1, removejaja,lazy=TRUE)
myCorpus1 <- tm_map(myCorpus1, removehaha,lazy=TRUE)

myCorpus2 <- tm_map(myCorpus2, removeURL,lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, removeURL2,lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, removejaja,lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, removehaha,lazy=TRUE)

myCorpus3 <- tm_map(myCorpus3, removeURL,lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, removeURL2,lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, removejaja,lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, removehaha,lazy=TRUE)

myCorpus4 <- tm_map(myCorpus4, removeURL,lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, removeURL2,lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, removejaja,lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, removehaha,lazy=TRUE)

myCorpus5 <- tm_map(myCorpus5, removeURL,lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, removeURL2,lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, removejaja,lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, removehaha,lazy=TRUE)

# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'),stopwords('spanish'),"NA","xd","xD","like","RT","etc","csm","para","ser","wtf","sin","mas","una","los","nos")
# remove "r" and "big" from stopwords
#myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus1 <- tm_map(myCorpus1, removeWords, myStopwords,lazy=TRUE)
myCorpus2 <- tm_map(myCorpus2, removeWords, myStopwords,lazy=TRUE)
myCorpus3 <- tm_map(myCorpus3, removeWords, myStopwords,lazy=TRUE)
myCorpus4 <- tm_map(myCorpus4, removeWords, myStopwords,lazy=TRUE)
myCorpus5 <- tm_map(myCorpus5, removeWords, myStopwords,lazy=TRUE)



#Stemming words
library(Snowball)
library(RWeka)
library(rJava)
library(RWekajars)

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






