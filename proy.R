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
#Data Frame con los posts
df <- do.call("rbind", lapply(posts$post, as.data.frame))
# build a corpus, and specify the source to be character vectors
      
myCorpus <- Corpus(VectorSource(df$X))
tm_map(myCorpus, function(x) iconv(enc2utf8(x), sub = "byte")) # problemas con el utf-8 en las pos impares
myCorpus <- tm_map(myCorpus, PlainTextDocument)
# convert to lower case
myCorpus <- tm_map(myCorpus, tolower,lazy=TRUE)

# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation,lazy=TRUE)

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers,lazy=TRUE)

# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
removeURL2 <- function(x) gsub("www[[:alnum:]]*", "", x)
removejaja <- function(x) gsub("\b(?:a*(?:ja)+j?|(?:l+o+)+l+)\b", "", x)
removehaha <- function(x) gsub("\b(?:a*(?:ha)+h?|(?:l+o+)+l+)\b", "", x)
myCorpus <- tm_map(myCorpus, removeURL,lazy=TRUE)
myCorpus <- tm_map(myCorpus, removeURL2,lazy=TRUE)
myCorpus <- tm_map(myCorpus, removejaja,lazy=TRUE)
myCorpus <- tm_map(myCorpus, removehaha,lazy=TRUE)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'),stopwords('spanish'),"NA","xd","xD","like","RT","etc","csm","para","ser","wtf","sin","mas","una","los","nos")
# remove "r" and "big" from stopwords
#myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords,lazy=TRUE)


#Stemming words
library(Snowball)
library(RWeka)
library(rJava)
library(RWekajars)

# keep a copy of corpus to use later as a dictionary for stem
myCorpusCopy <- myCorpus


myCorpus <- tm_map(myCorpus, stemDocument,language="english",lazy=TRUE)

myStopwords2 <- c("est","esto","xd")

myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords2,lazy=TRUE)

#Split Data frames por usuarios
u1 <- posts[which(posts$id_user == 1),]
u2 <- posts[which(posts$id_user == 2),]
u3 <- posts[which(posts$id_user == 3),]
u4 <- posts[which(posts$id_user == 4),]
u5 <- posts[which(posts$id_user == 5),]

#para ver que hay en los primeros 100
for (i in 1:3000) {
  
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))
  
}


myTdm <- TermDocumentMatrix(myCorpus,control=list(wordLengths=c(1,Inf))) #Matriz

myTdm <- TermDocumentMatrix(myCorpus,control=list(minWordLength=1))
inspect(myTdm)
# inspect frequent words
findFreqTerms(myTdm, lowfreq=25)
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=15)

#Impresion Grafica
barplot(termFrequency, las=2)
library(wordcloud)
m <- as.matrix(myTdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed (375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3,random.order=F, colors=grayLevels)
# remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.985) 
m2 <- as.matrix(myTdm2)
frequency <- colSums(m2)
frequency <- sort(frequency, decreasing=TRUE)
frequency
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="single")
plot(fit)
# cut tree into 9 clusters
rect.hclust(fit, k=4)
groups <- cutree(fit, k=4)
groups

write.csv(groups, file = "MyGroups.csv") #-->van a carpeta personal

# count frequency 
temp <- inspect(myTdm2)
FreqMat <- data.frame(ST = rownames(temp), Freq = rowSums(temp))
row.names(FreqMat) <- NULL
FreqMat
FreqMat2 <- FreqMat

FreqMat2$grupos <- groups

# Ahora exportamos el archivo FreqMat2 que tiene las palabras con su grupo y la frecuencia
write.csv(FreqMat2, file = "grupos.csv")

