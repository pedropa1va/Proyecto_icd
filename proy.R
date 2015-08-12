library(tm)
library(SnowballC)
library(wordcloud)

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
                 "csm", "para", "ser", "wtf", "mas", "una", "nos", "est","esto", "este",
                 ,"jajaja", "jajajaja", "si")
skipWords <- function(x) removeWords(x, myStopwords)

# Stemming words
# por alguna no razón no agarra el spanish... Igual no creo que sea necesario hacer stemming
#stemDoc_es <- function(x) stemDocument(x, language = meta(x, "spanish"))
#stemDoc_es <- function(x) stemDocument(x, language ="spanish")

cleanFuns <- list(PlainTextDocument, tolower, removePunctuation, removeNumbers, removeURL,
                  removeLaugh, skipWords)

# Limpieza de cada corpus
myCorpus1 <- tm_map(myCorpus1, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus2 <- tm_map(myCorpus2, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus3 <- tm_map(myCorpus3, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus4 <- tm_map(myCorpus4, FUN = tm_reduce, tmFuns = cleanFuns)
myCorpus5 <- tm_map(myCorpus5, FUN = tm_reduce, tmFuns = cleanFuns)

# Para ver que hay en los primeros 50 documentos
for (i in 1:50) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus1[[i]], width=90))
}

# Matriz Term-Document para cada usuario
myTdm1 <- TermDocumentMatrix(myCorpus1, control=list(wordLengths=c(2,Inf), stopwords = T))
myTdm2 <- TermDocumentMatrix(myCorpus2, control=list(wordLengths=c(2,Inf), stopwords = T))
myTdm3 <- TermDocumentMatrix(myCorpus3, control=list(wordLengths=c(2,Inf), stopwords = T))
myTdm4 <- TermDocumentMatrix(myCorpus4, control=list(wordLengths=c(2,Inf), stopwords = T))
myTdm5 <- TermDocumentMatrix(myCorpus5, control=list(wordLengths=c(2,Inf), stopwords = T))

# Para ver la matriz
View(as.matrix(myTdm1))
#inspect(myTdm1)

# remove sparse terms usuario 1
myTdmAux1 <- removeSparseTerms(myTdm1, sparse=0.985)
#View(as.matrix(myTdmAux1))
# count frequency 
temp <- inspect(myTdmAux)
FreqMat1 <- data.frame(Term = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat1) <- NULL
FreqMat1

# remove sparse terms usuario 2
myTdmAux <- removeSparseTerms(myTdm2, sparse=0.985)
# count frequency 
temp <- inspect(myTdmAux)
FreqMat2 <- data.frame(Term = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat2) <- NULL
FreqMat2

# remove sparse terms usuario 3
myTdmAux <- removeSparseTerms(myTdm3, sparse=0.985) 
# count frequency 
temp <- inspect(myTdmAux)
FreqMat3 <- data.frame(Term = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat3) <- NULL
FreqMat3

# remove sparse terms usuario 4
myTdmAux <- removeSparseTerms(myTdm4, sparse=0.985) 
# count frequency 
temp <- as.matrix(myTdmAux)
FreqMat4 <- data.frame(Term = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat4) <- NULL
FreqMat4

# remove sparse terms usuario 5
myTdmAux <- removeSparseTerms(myTdm5, sparse=0.985)
# count frequency
temp <- as.matrix(myTdmAux)
#View(temp)
FreqMat5 <- data.frame(Term = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat5) <- NULL
FreqMat5

inters12 <- paste(intersect(FreqMat1$Term, FreqMat2$Term), collapse = " ")
inters13 <- paste(intersect(FreqMat1$Term, FreqMat3$Term), collapse = " ")
inters14 <- paste(intersect(FreqMat1$Term, FreqMat3$Term), collapse = " ")
inters15 <- paste(intersect(FreqMat1$Term, FreqMat5$Term), collapse = " ")

inters23 <- paste(intersect(FreqMat2$Term, FreqMat3$Term), collapse = " ")
inters24 <- paste(intersect(FreqMat2$Term, FreqMat4$Term), collapse = " ")
inters25 <- paste(intersect(FreqMat2$Term, FreqMat5$Term), collapse = " ")

inters34 <- paste(intersect(FreqMat3$Term, FreqMat4$Term), collapse = " ")
inters35 <- paste(intersect(FreqMat3$Term, FreqMat5$Term), collapse = " ")

inters45 <- paste(intersect(FreqMat4$Term, FreqMat5$Term), collapse = " ")

user1 <- c(NA,			inters12,	inters13,	inters14,	inters15)
user2 <- c(inters12,	NA, 		inters23,	inters24,	inters25)
user3 <- c(inters13,	inters23,	NA,			inters34,	inters35)
user4 <- c(inters14,	inters24,	inters34,	NA,			inters45)
user5 <- c(inters15,	inters25,	inters35,	inters45,	NA)

users.df <- cbind(user1, user2, user3, user4, user5)
write.csv(users.df, file = "20822923_facebook_usuarios_grupos.csv", quote = T)


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
          colors=brewer.pal(6, "Dark2"))
wordcloud(words = names(wordFreq), freq = wordFreq, min.freq = 8, random.order = FALSE,
          colors = c("grey","dark grey","blue","green","orange","red"))


# USUARIO 2

#Impresion Grafica usuario 2
barplot(termFrequency2, las=2)
m <- as.matrix(myTdm2)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=10,random.order=F,
          colors=brewer.pal(6, "Dark2"))


# USUARIO 3

#Impresion Grafica usuario 3
barplot(termFrequency3, las=2)
m <- as.matrix(myTdm3)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=10,random.order=F,
          colors=brewer.pal(6, "Dark2"))


# USUARIO 4

#Impresion Grafica usuario 4
barplot(termFrequency4, las=2)
m <- as.matrix(myTdm4)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=10,random.order=F,
          colors=brewer.pal(6, "Dark2"))


# USUARIO 5

#Impresion Grafica usuario 5
barplot(termFrequency5, las=2)
m <- as.matrix(myTdm5)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,random.order=F,
          colors=brewer.pal(6, "Dark2"))
