
# -- Twitter Text Mining and Sentiment Analysis --
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(openssl)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

Tweets <- userTimeline('@Cristiano', n = 1000,includeRts = F) # Name of the content to be extracted
TweetsDF <- twListToDF(Tweets) # Convert the extracted content to a Data frame
dim(TweetsDF) # Dimension of the Data frame

review_data <- TweetsDF$text # Importing Data
View(review_data)

# Loading Positive, Negative and Stop Words
pos.words <- scan(file.choose(), what = "character")	# reading positive-words.txt
neg.words <- scan(file.choose(), what = "character") # reading negative-words.txt
stop_words <- scan(file.choose(), what = "character") # reading stop-words.txt

library(tm)
text_corpus <- Corpus(VectorSource(review_data)) # Creating Corpus Data

text_corpus <- tm_map(text_corpus, content_transformer(tolower)) # Convert to lower case

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) # Remove URLs Function
text_corpus <- tm_map(text_corpus, content_transformer(removeURL)) # Removing URLs

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) # Function to Remove anything other than English letters or space 
text_corpus <- tm_map(text_corpus, content_transformer(removeNumPunct))

text_corpus <- tm_map(text_corpus, removeWords, stop_words) # Removing Stop words

text_corpus <- tm_map(text_corpus, stripWhitespace) # Removing extra whitespace


tf <- TermDocumentMatrix((text_corpus)) # Creating a TDM
tf_idf <- TermDocumentMatrix(text_corpus, control = list(weighting = function(p) weightTfIdf(p, normalize = F), stopwords=T))#,stemming=T))

n <- NULL
for (i in 1:ncol(tf)){ if (sum(tf[, i]) == 0) {n = c(n, i)} } # Removing reviews with no content

m <- NULL
for (j in 1:ncol(tf_idf)){ if (sum(tf_idf[, j]) == 0) {m = c(m, j)} } # Removing reviews with no content

tdm_1 <- tf[,-n]
tdm_2 <- tf_idf[,-m]
dtm_1 <- t(tdm_1) # Creating a DTM
dtm_2 <- t(tdm_2) # Creating a DTM


# Function to build Word Cloud
library(wordcloud)
word_cloud = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors = brewer.pal(8,"Dark2"))
} 

word_cloud(tdm_1) # Word Cloud TDM containing All Keywords
word_cloud(tdm_2)

# Function to build a Positive Word Cloud
positive_wc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), pos.words)
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

positive_wc(tdm_1) # Word Cloud TDM containing Positive Keywords
positive_wc(tdm_2)

# Function to build a Negative Word Cloud
negative_wc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  wordcloud(names,freq_neg,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

negative_wc(tdm_1) # Word Cloud TDM containing Negative Keywords
negative_wc(tdm_2)

# Function to build a Barplot with Frequency
wc_barplot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") + ggtitle("Most frequent words")
}

wc_barplot(tdm_1) # Bar plot of Word Cloud containing all keywords
wc_barplot(tdm_2)

# Function to build a Positive Barplot with Frequency
pwc_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") + ggtitle("Most frequent positive words")
}

pwc_bar_plot(dtm_1) # Plot of Positive words frequency
pwc_bar_plot(dtm_2)

# Function to build a Negative Barplot with Frequency
nwc_barplot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}

nwc_barplot(dtm_1) # Plot of Negative words frequency
nwc_barplot(dtm_2)
