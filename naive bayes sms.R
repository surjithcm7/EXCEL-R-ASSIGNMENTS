library(tm)
library(e1071)         #For Naive Bayes
library(caret)         #For the Confusion Matrix

#Import data
sms_raw <- read.csv(file.choose())
head(sms_raw)
#Select & rename appropriate columns of the dataset
sms_raw <- sms_raw[, 1:2]
colnames(sms_raw) <- c("type", "text")
str(sms_raw)

#Find the proportions of junk vs legitimate sms messages
table(sms_raw$type)
prop.table(table(sms_raw$type))

sms_corpus <- VCorpus(VectorSource(sms_raw$text))

sms_dtm <- DocumentTermMatrix(sms_corpus, control = 
                                list(tolower = TRUE,
                                     removeNumbers = TRUE,
                                     stopwords = TRUE,
                                     removePunctuation = TRUE,
                                     stripWhitespace = TRUE,
                                     PlainTextDocument= TRUE,
                                     stemming = TRUE))

dim(sms_dtm)
#Training & Test set
sms_dtm_train <- sms_dtm[1:4457, ]
sms_dtm_test <- sms_dtm[4458:5559, ]

#Training & Test Label
sms_train_labels <- sms_raw[1:4457, ]$type
sms_test_labels <- sms_raw[4458:5559, ]$type

#Proportion for training & test labels
prop.table(table(sms_train_labels))
#here we can see that 86.6% are ham in the train data and 13.39% are spam
prop.table(table(sms_test_labels))
#here too the proportion of the test data is same as train data 
threshold <- 0.1

min_freq = round(sms_dtm$nrow*(threshold/100),0)

min_freq

# Create vector of most frequent words
freq_words <- findFreqTerms(x = sms_dtm, lowfreq = min_freq)

str(freq_words)

#Filter the DTM
sms_dtm_freq_train <- sms_dtm_train[ , freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , freq_words]

dim(sms_dtm_freq_train)
sms_dtm_freq_train <- sms_dtm_train[ , freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , freq_words]

dim(sms_dtm_freq_train)
convert_values <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_values)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_values)
#Create model from the training dataset
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#Make predictions on test set
sms_test_pred <- predict(sms_classifier, sms_test)

#Create confusion matrix
confusionMatrix(data = sms_test_pred, reference = sms_test_labels,
                positive = "spam", dnn = c("Prediction", "Actual"))
#the model accuracy is 97.55%
#here we can see that out of 952 ham messages 4 were predicted as spam 948 were correct
#out of 150 spam messeges 23 were classified as ham and 123 were correct
