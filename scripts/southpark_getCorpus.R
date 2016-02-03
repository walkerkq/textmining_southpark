library(tm)
library(RWeka)
library(stringr)

set.wd("/Users/kaylinwalker/R/textmining_southpark/raw data")
by_person <- read.csv("southpark_byperson_scripts.csv", stringsAsFactors=FALSE)

# keep the speakers with the most words, this keeps 23
by_person2 <- by_person[nchar(by_person$text) > 10000, ]

# save the rest of the text into one big blog
kept.speakers <- unique(by_person2$speaker)
other.text <- by_person[!(by_person$speaker %in% kept.speakers), ]
other.text <- str_c(other.text$text, collapse=" ")
other <- data.frame(speaker="ALL.OTHERS", text=other.text)

# add it back in
by_person2 <- rbind(by_person2, other)

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

############ unigrams
# create term document matrices
corpus.tdm <- TermDocumentMatrix(corpus.stop.gone)
# remove sparse terms
tdm.80 <- removeSparseTerms(corpus.tdm, 0.8)

# save as a simple data frame
count.tdm <- data.frame(inspect(tdm.80))
count.tdm$word <- row.names(count.tdm)

#1805 people, sum = 211,708 
#103 people, sum = 162,520 
#103, 85% sparse, sum = 105,046
#23 main speakers + other = 211,708
#23 main + other, 80% sparse = 172,269

#write.csv(count.tdm, "southpark_tdm.csv", row.names=FALSE)

############### bigrams
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bi.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

# remove sparse terms
bi.tdm.80 <- removeSparseTerms(bi.tdm, 0.8)

# save as a simple data frame
count.bi.tdm <- data.frame(inspect(bi.tdm.80)) # 80% sparse = 32,625 bigrams / with stop words = 168,320 bigrams total, 5849 unique
count.bi.tdm$word <- row.names(count.bi.tdm)

#write.csv(count.bi.tdm, "southpark_bi_tdm_wstop.csv", row.names=FALSE)

############### trigrams
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tri.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))

# remove sparse terms
tri.tdm.80 <- removeSparseTerms(tri.tdm, 0.80)

# save as a simple data frame
count.tri.tdm <- data.frame(inspect(tri.tdm.80)) 
# 90% sparse = 4,730 trigrams & 881 unique 
# 80% sparse = 1,184 trigrams & 101 unique / 31,027 trigrams & 1,927 unique w/stopwords **
# 85% sparse = 2,374 trigrams & 295 unique ** / 42,586 trigrams & 3,764 unique w/stopwords
count.tri.tdm$word <- row.names(count.tri.tdm)

#write.csv(count.tri.tdm, "southpark_tri_tdm_wstop.csv", row.names=FALSE)

############### quadgrams
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quad.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = QuadgramTokenizer))

# remove sparse terms
quad.tdm.85 <- removeSparseTerms(quad.tdm, 0.85)

# save as a simple data frame
count.quad.tdm <- data.frame(inspect(quad.tdm.85)) 
# 90% sparse = 2,032 unique, 12,022 quadgrams
# 80% sparse = 329 unique, 4401 quadgrams
# 85% sparse = 741 unique, 6911 quadgrams **
count.quad.tdm$word <- row.names(count.quad.tdm)

#write.csv(count.quad.tdm, "southpark_quad_tdm_wstop.csv", row.names=FALSE)


################ 5-grams
QuintgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
quint.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = QuintgramTokenizer))

# remove sparse terms
quint.tdm.85 <- removeSparseTerms(quint.tdm, 0.8)
# 90% sparse = 879 5-grams & 4877 strings
# 80% sparse = 131 5-grams & 1610 strings **
# 85% sparse = 285 5-grams & 2543 strings
count.quint.tdm <- data.frame(inspect(quint.tdm.85))
sum(colSums(count.quint.tdm))
count.quint.tdm$word <- row.names(count.quint.tdm)

#write.csv(count.quint.tdm, "southpark_quint_tdm_wstop.csv", row.names=FALSE)
