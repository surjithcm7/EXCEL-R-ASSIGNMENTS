# -- AMAZON Web reviews on "redmi note 8 pro" - Text Mining 

library(rvest)
aurl <- "https://www.amazon.in/Redmi-Note-Pro-Storage-Processor/dp/B07X4PXKZQ/ref=sr_1_2?crid=VKEYIOD85OL2&keywords=redmi+note+8+pro+mobile&qid=1583755449&sprefix=redmi+%2Caps%2C420&sr=8-2"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl, i, sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews, rev)
}
write.table(amazon_reviews,"8pro.txt", row.names = F)
View(amazon_reviews)

getwd()
review_data <- readLines((choose.files()))
View(review_data)


# Loading Positive, Negative and Stop Words
pos.words <- scan(file.choose(), what="character")	
neg.words <- scan(file.choose(), what="character") 
stop_words <- readLines(file.choose()) 


library(tm)
text_corpus <- Corpus(VectorSource(review_data)) 

text_corpus = tm_map(text_corpus, stripWhitespace) 	# removes white space
text_corpus = tm_map(text_corpus, tolower)		# converts to lower case
text_corpus = tm_map(text_corpus, removePunctuation)	# removes punctuation marks
text_corpus = tm_map(text_corpus, removeNumbers)		# removes numbers in the documents
text_corpus = tm_map(text_corpus, removeWords, stop_words)

tf <- TermDocumentMatrix((text_corpus)) # Creating a TDM

tf_idf <- TermDocumentMatrix(text_corpus, control = list(weighting = function(p) weightTfIdf(p, normalize = F), stopwords=T))#,stemming=T))

n <- NULL
for (i in 1:ncol(tf)){ if (sum(tf[, i]) == 0) {n = c(n, i)} } # Removing reviews with no content

m <- NULL
for (j in 1:ncol(tf_idf)){ if (sum(tf_idf[, j]) == 0) {m = c(m, j)} } # Removing reviews with no content

tdm_1 <- tf[, -n]
tdm_2 <- tf_idf[, -m]
dtm_1 <- t(tdm_1) # Creating a DTM
dtm_2 <- t(tdm_2) # Creating a DTM

# Function to build Word Cloud
library(wordcloud)
word_cloud = function(x){	
  freq = sort(rowSums(as.matrix(x)), decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120], scale = c(4,.5),random.order = F, colors = brewer.pal(8, "Dark2"))
} 
windows()
word_cloud(tdm_1) # Word Cloud TDM containing All Keywords
word_cloud(tdm_2)

# Function to build a Positive Word Cloud
positive_wc = function(x){
  freq = sort(rowSums(as.matrix(x)), decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  wordcloud(names, freq_pos, scale=c(4, .5), colors = brewer.pal(8, "Dark2"))
}

positive_wc(tdm_1) # Word Cloud TDM containing Positive Keywords
positive_wc(tdm_2)

# Function to build a Negative Word Cloud
negative_wc = function(x){	
  freq = sort(rowSums(as.matrix(x)), decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  wordcloud(names, freq_neg,scale=c(4,.5), colors = brewer.pal(8, "Dark2"))
}
windows()
negative_wc(tdm_1) # Word Cloud TDM containing Negative Keywords
negative_wc(tdm_2)

# Function to build a Barplot with Frequency
wc_barplot <- function(x){
  freq = sort(rowSums(as.matrix(x)), decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  ggplot(head(freq.df, 50), aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Words") + ylab("Frequency") + ggtitle("Most frequent words")
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
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq, decreasing=T),]
  ggplot(head(pos_words_freq, 30), aes(reorder(word,freq), freq)) + geom_bar(stat = "identity") + coord_flip() + xlab("Positive words") + ylab("Frequency") + ggtitle("Most frequent positive words")
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
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq, decreasing=T),]
  ggplot(head(neg_words_freq, 30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() + xlab("words") + ylab("Frequency") + ggtitle("Most frequent negative words")
}

nwc_barplot(dtm_1) # Plot of Negative words frequency
nwc_barplot(dtm_2)



