# walkerkq 2.3.2016 
# textmining_southpark
########################
# R script to create term document matrices from scraped South Park transcripts using
# the tm, RWeka and stringr packages.
########################
# Input: 
### southpark_byperson_scripts.csv created in 01_southpark_scrape.R
########################
# Output: five  .csv files.
### southpark_tdm.csv: data frame of term document matrix of unigrams
### southpark_bi_tdm_wstop.csv: data frame of term document matrix of bigrams, including stopwords
### southpark_tri_tdm_wstop.csv: data frame of term document matrix of trigrams, including stopwords
### southpark_quad_tdm_wstop.csv: data frame of term document matrix of 4-grams, including stopwords
### southpark_quint_tdm_wstop.csv: data frame of term document matrix of 5-grams, including stopwords
########################

library(tm)
library(RWeka)
library(stringr)

setwd("/Users/kaylinwalker/R/textmining_southpark/raw data")
by_person <- read.csv("southpark_byperson_scripts.csv", stringsAsFactors=FALSE)

# keep the speakers with the most words, this keeps 23
by_person2 <- by_person[nchar(by_person$text) > 10000, ]

# save the rest of the text into one big speaker "ALL.OTHERS"
kept.speakers <- unique(by_person2$speaker)
other.text <- by_person[!(by_person$speaker %in% kept.speakers), ]
other.text <- str_c(other.text$text, collapse=" ")
other <- data.frame(speaker="ALL.OTHERS", text=other.text)

# add it back in
by_person2 <- rbind(by_person2, other); rm(by_person)

# create corpus
myReader <- readTabular(mapping=list(content="text", id="speaker"))
corpus <- Corpus(DataframeSource(by_person2), readerControl=list(reader=myReader))

# pre-process text
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus.stop.gone <- tm_map(corpus, removeWords, stopwords("english"))


#################################### UNIGRAMS

# create term document matrices
corpus.tdm <- TermDocumentMatrix(corpus.stop.gone)
# remove sparse terms
tdm.80 <- removeSparseTerms(corpus.tdm, 0.8)

# save as a simple data frame
count.tdm <- data.frame(inspect(tdm.80))
count.tdm$word <- row.names(count.tdm)


#################################### BIGRAMS

options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bi.tdm <- TermDocumentMatrix(corpus.stop.gone, control = list(tokenize = BigramTokenizer))

# remove sparse terms
bi.tdm.80 <- removeSparseTerms(bi.tdm, 0.8)

# save as a simple data frame
count.bi.tdm <- data.frame(inspect(bi.tdm.80)) 
count.bi.tdm$word <- row.names(count.bi.tdm)


#################################### TRIGRAMS

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tri.tdm <- TermDocumentMatrix(corpus.stop.gone, control = list(tokenize = TrigramTokenizer))

# remove sparse terms
tri.tdm.80 <- removeSparseTerms(tri.tdm, 0.80)

# save as a simple data frame
count.tri.tdm <- data.frame(inspect(tri.tdm.80)) 
count.tri.tdm$word <- row.names(count.tri.tdm)


#################################### 4-GRAMS

QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quad.tdm <- TermDocumentMatrix(corpus.stop.gone, control = list(tokenize = QuadgramTokenizer))

# remove sparse terms
quad.tdm.80 <- removeSparseTerms(quad.tdm, 0.80)

# save as a simple data frame
count.quad.tdm <- data.frame(inspect(quad.tdm.80)) 
count.quad.tdm$word <- row.names(count.quad.tdm)


#################################### 5-GRAMS

QuintgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
quint.tdm <- TermDocumentMatrix(corpus.stop.gone, control = list(tokenize = QuintgramTokenizer))

# remove sparse terms
quint.tdm.95 <- removeSparseTerms(quint.tdm, 0.95)
count.quint.tdm <- data.frame(inspect(quint.tdm.95))
sum(colSums(count.quint.tdm))
count.quint.tdm$word <- row.names(count.quint.tdm)


#################################### WRITE FILES

#write.csv(count.tdm, "southpark_tdm.csv", row.names=FALSE)
#write.csv(count.bi.tdm, "southpark_bi_tdm.csv", row.names=FALSE)
#write.csv(count.tri.tdm, "southpark_tri_tdm.csv", row.names=FALSE)
#write.csv(count.quad.tdm, "southpark_quad_tdm.csv", row.names=FALSE)
#write.csv(count.quint.tdm, "southpark_quint_tdm.csv", row.names=FALSE)


