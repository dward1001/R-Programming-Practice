sms_raw <- read.csv("sms_spam_starting_hope.csv", stringsAsFactors = FALSE)

str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
levels(sms_raw$type)

str(sms_raw$type)
table(sms_raw$type)

install.packages("tm")
library(tm)

# delete unnecessary words such as 'a'
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])

# capital to lower & remove numbers
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
inspect(corpus_clean[1:3])

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean
inspect(corpus_clean[1:3])
sms_dtm <- DocumentTermMatrix(corpus_clean)

# divide test data & train data
sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5558,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4179:5558,]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5558]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

install.packages("wordcloud")
library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 40, random.order = F)

spam <- subset(sms_raw_train, type=="spam")
ham <- subset(sms_raw_train, type=="ham")

wordcloud(spam$text, max.words = 40, scale=c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale=c(5, 0.5))

findFreqTerms(sms_dtm_train, 5)

