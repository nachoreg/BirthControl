library(dplyr)
library(wordcloud)
library(tidytext)
library(tidyverse)
library(tm)
library(stringr)


# Load the dataset
drugsComTest_raw <- read.delim("~/Desktop/IS Individual Project/drugsCom_raw/drugsComTest_raw.tsv", encoding = "UTF-8")

# Setting up the data
drugs <- drugsComTest_raw[-c(1, 5, 6, 7)]
freqCondition <- table(drugs$condition)

# Bar plot more frequent conditions
barplot(freqCondition[freqCondition>1300])

# Filtering by Birth Control
bControl <- filter(drugs, condition == "Birth Control")
freqBControl <- table(bControl$drugName)

# Bar plot more frequent drugs
barplot(freqBControl[freqBControl>500], cex.names=.7)

# Filtering by Etonogestrel, Levonorgestrel and Nexplanon
FiltBControl <- filter(bControl, drugName == c("Etonogestrel", "Levonorgestrel", "Nexplanon"))
DrugEto <- filter(FiltBControl, drugName == "Etonogestrel")
DrugEto <- DrugEto[1:240,]
DrugLev <- filter(FiltBControl, drugName == "Levonorgestrel")
DrugLev <- DrugLev[1:240,]
DrugNex <- filter(FiltBControl, drugName == "Nexplanon")
FiltDrugs <- data.frame(DrugEto, DrugLev, DrugNex)
FiltDrugs <- FiltDrugs[,c(3,6,9)]

# Creating the corpus  
corpusD <- VCorpus(VectorSource(FiltBControl$review))
corpusDRaw <- VCorpus(VectorSource(FiltBControl$review))
corpusELN <- VCorpus(VectorSource(FiltDrugs))

#Cleaning up the text 
# Lowering letters 
corpusD <- tm_map(corpusD, content_transformer(tolower))
corpusELN <- tm_map(corpusELN, content_transformer(tolower))

# Removing punctuation
corpusD = tm_map(corpusD,removePunctuation)
corpusELN = tm_map(corpusELN,removePunctuation)

# Removing stopwords
corpusD = tm_map(corpusD,removeWords,stopwords())
stopW <- c(stopwords(), "nexplanon")
corpusELN = tm_map(corpusELN,removeWords, stopW)

# Removing numbers
corpusD = tm_map(corpusD,removeNumbers)
corpusELN = tm_map(corpusELN,removeNumbers)

# Removing spaces
corpusD = tm_map(corpusD,stripWhitespace)
corpusELN = tm_map(corpusELN,stripWhitespace)

# Showing differences 
inspect(corpusDRaw[[1]])
inspect(corpusD[[1]])

# Preparing data
tdm <- TermDocumentMatrix(corpusD)
freq <- rowSums(as.matrix(tdm))

# Wordcloud
wordcloud(words = names(freq), freq = freq, random.order = FALSE,
          colors = brewer.pal(10, "Dark2"), min.freq = 10)

# Preparing 3 drugs data
tdmELN <- TermDocumentMatrix(corpusELN)
tdmELN.matrix <- as.matrix(tdmELN)
colnames(tdmELN.matrix) <- c("Etonogestrel", "Levonorgestrel", "Nexplanon")

# Comparison cloud
comparison.cloud(tdmELN.matrix,title.size = 2, random.order = FALSE, min.freq = 1)

# Sentiment analysis: Dictionary based
textD <- vector(mode = "character", length = 842)

for(i in 1:842){
  textD[i] <- corpusD[[i]]$content
}

words <- str_split(textD, pattern = "\\s+")
drugWords <- unlist(words)
drugWords <- as.data.frame(drugWords)
colnames(drugWords) <- c('word')
drugWords.sentiment <- inner_join(drugWords, get_sentiments('bing'))

drugWords.positives <- drugWords.sentiment[drugWords.sentiment$sentiment == "positive",]
drugWords.negatives <- drugWords.sentiment[drugWords.sentiment$sentiment == "negative",]
countposi <- count(drugWords.positives,word)
countposi$sentiment <- rep("positive", 227)
colnames(countposi)[2] <- "p"
countnega <- count(drugWords.negatives,word)
countnega$sentiment <- rep("negative", 448)

sentimentELN <- bind_rows(countposi,countnega)
sentimentELN[is.na(sentimentELN)] <- 0
wordsELN <- sentimentELN$word
rownames(sentimentELN) <- wordsELN
sentimentELN <- sentimentELN[,c("p","n")]
colnames(sentimentELN) <- c("positive","negative")


totalSentiment <- count(drugWords.sentiment, sentiment)
totalSentiment

# Frequency table
wFreq <- rowSums(as.matrix(tdm))
topFreq <- tail(sort(wFreq), n = 15)
topFreq.df <- as.data.frame(sort(topFreq))
topFreq.df$names <- rownames(topFreq.df) 

ggplot(topFreq.df, aes(reorder(names,topFreq), topFreq,  fill = names)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Words") + ylab("Frequency") +
  ggtitle("Word frequencies") + 
  theme(plot.title = element_text(hjust = -.08,vjust = 2, color = "#535353", size = 16, face = "bold")) +
  theme(legend.position = "none")


#Sentiment Analysis drug subgroups
textE <- corpusELN[[1]]$content
textL <- corpusELN[[2]]$content
textN <- corpusELN[[3]]$content

wordsE <- str_split(textE, pattern ="\\s+")
wordsL <- str_split(textL, pattern ="\\s+")
wordsN <- str_split(textN, pattern ="\\s+")

drugWordsE <- unlist(wordsE)
drugWordsE <- as.data.frame(drugWordsE)
drugWordsL <- unlist(wordsL)
drugWordsL <- as.data.frame(drugWordsL)
drugWordsN <- unlist(wordsN)
drugWordsN <- as.data.frame(drugWordsN)

colnames(drugWordsE) <- c('word')
colnames(drugWordsL) <- c('word')
colnames(drugWordsN) <- c('word')

drugWordsE.sentiment <- inner_join(drugWordsE, get_sentiments('bing'))
drugWordsL.sentiment <- inner_join(drugWordsL, get_sentiments('bing'))
drugWordsN.sentiment <- inner_join(drugWordsN, get_sentiments('bing'))

ESentiment <- count(drugWordsE.sentiment, sentiment)
LSentiment <- count(drugWordsL.sentiment, sentiment)
NSentiment <- count(drugWordsN.sentiment, sentiment)

ESentiment <- t(ESentiment)
LSentiment <- t(LSentiment)
NSentiment <- t(NSentiment)

ESentiment <- ESentiment[2,]
ESentiment <- as.numeric(ESentiment[c(2,1)])
LSentiment <- LSentiment[2,]
LSentiment <- as.numeric(LSentiment[c(2,1)])
NSentiment <- NSentiment[2,]
NSentiment <- as.numeric(NSentiment[c(2,1)])

names(ESentiment) <- c("positive", "negative")
names(LSentiment) <- c("positive", "negative")
names(NSentiment) <- c("positive", "negative")

propE <- prop.table(ESentiment)
propL <- prop.table(LSentiment)
propN <- prop.table(NSentiment)
propELN <- cbind(propE,propL,propN)
propELN <- t(propELN)
propELN <- as.data.frame(propELN)
propELNdf <- data.frame(values=c(t(data.matrix(propELN))), ind=I(rep(colnames(propELN), nrow(propELN))))
ELNnames <- rep(c("Etonogestrel", "Levonorgestrel", "Nexplanon"),each = 2)
propELNdf$drug <- c(ELNnames)

# Proportion of positive and negative words per drug
ggplot(propELNdf, aes(x = drug, y = values, fill = factor(ind))) +
  geom_col(position = "dodge")

# Comparison cloud of all the words
comparison.cloud(sentimentELN, title.size=3, random.order = FALSE, min.freq = 1)

# Comparison cloud for each drug
drugWordsE.positives <- drugWordsE.sentiment[drugWordsE.sentiment$sentiment == "positive",]
drugWordsE.negatives <- drugWordsE.sentiment[drugWordsE.sentiment$sentiment == "negative",]

countposiE <- count(drugWordsE.positives,word)
countposiE$sentiment <- rep("positive", 119)
colnames(countposiE)[2] <- "p"
countnegaE <- count(drugWordsE.negatives,word)
countnegaE$sentiment <- rep("negative", 258)

sentimentE <- bind_rows(countposiE, countnegaE)
sentimentE[is.na(sentimentE)] <- 0
wordsE <- sentimentE$word
rownames(sentimentE) <- wordsE
sentimentE <- sentimentE[,c("p","n")]
colnames(sentimentE) <- c("positive","negative")

comparison.cloud(sentimentE, title.size=3, random.order = FALSE, min.freq = 1)

drugWordsL.positives <- drugWordsL.sentiment[drugWordsL.sentiment$sentiment == "positive",]
drugWordsL.negatives <- drugWordsL.sentiment[drugWordsL.sentiment$sentiment == "negative",]

countposiL <- count(drugWordsL.positives,word)
countposiL$sentiment <- rep("positive", 145)
colnames(countposiL)[2] <- "p"
countnegaL <- count(drugWordsL.negatives,word)
countnegaL$sentiment <- rep("negative", 220)

sentimentL <- bind_rows(countposiL, countnegaL)
sentimentL[is.na(sentimentL)] <- 0
wordsL <- sentimentL$word
rownames(sentimentL) <- wordsL
sentimentL <- sentimentL[,c("p","n")]
colnames(sentimentL) <- c("positive","negative")

comparison.cloud(sentimentL, title.size=3, random.order = FALSE, min.freq = 1)

drugWordsN.positives <- drugWordsN.sentiment[drugWordsN.sentiment$sentiment == "positive",]
drugWordsN.negatives <- drugWordsN.sentiment[drugWordsN.sentiment$sentiment == "negative",]

countposiN <- count(drugWordsN.positives,word)
countposiN$sentiment <- rep("positive", 118)
colnames(countposiN)[2] <- "p"
countnegaN <- count(drugWordsN.negatives,word)
countnegaN$sentiment <- rep("negative", 258)

sentimentN <- bind_rows(countposiL, countnegaL)
sentimentN[is.na(sentimentN)] <- 0
wordsL <- sentimentN$word
rownames(sentimentN) <- wordsL
sentimentN <- sentimentN[,c("p","n")]
colnames(sentimentN) <- c("positive","negative")

comparison.cloud(sentimentN, title.size=3, random.order = FALSE, min.freq = 1)



