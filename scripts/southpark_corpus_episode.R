library(stringr)
library(tm)
library(RWeka)

# read in the data set
setwd("/Users/kaylinwalker/R/textmining_southpark/data/raw data")
if(file.exists("all-seasons.csv")) {
    all <- read.csv("all-seasons.csv", stringsAsFactors=FALSE)
} else { 
    url <- "https://raw.githubusercontent.com/BobAdamsEE/SouthParkData/master/All-seasons.csv"
    all <- download.file(url, "all-seasons.csv", method="curl")
    all <- read.csv("all-seasons.csv", stringsAsFactors=FALSE)
}

# create a unique identifier for each episode
for(h in seq_along(all[,1])) if(nchar(all$Episode[h]) < 2) all$Episode[h] <- paste("0", all$Episode[h], sep="")
all$ES <- paste(all$Season, all$Episode, sep=".")
for(j in c(1,5)) all[,j] <- as.numeric(all[,j])
all <- all[complete.cases(all), ]
episodes <- unique(all$ES)
episodes <- episodes[order(episodes)]

# condense into episodes
by.episode <- NULL
for(g in seq_along(episodes)){
     subset <- all[all$ES==episodes[g], ]
     subset <- subset[complete.cases(subset), ]
     text <- str_c(subset$Line, collapse=" ")
     row <- data.frame(episode.number=g, episode.code=episodes[g], season=subset$Season[1], text=text)
     by.episode <- rbind(by.episode, row)
}

# create corpus
myReader <- readTabular(mapping=list(content="text", id="episode.number"))
corpus <- Corpus(DataframeSource(by.episode), readerControl=list(reader=myReader))

# pre-process text
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# create term document matrix
options(mc.cores=1)
allTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
ep.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))

# remove sparse terms
ep.tdm.90 <- removeSparseTerms(ep.tdm, 0.90) # 1279 / 27536

# save as a simple data frame
count.ep <- data.frame(inspect(ep.tdm.90)) 
count.ep$word <- row.names(count.ep)
#setwd("/Users/kaylinwalker/Desktop")
#write.csv(count.ep, "southpark_tdm_episode.csv", row.names=FALSE)
