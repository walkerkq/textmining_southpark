# walkerkq 2.3.2016 
# textmining_southpark
########################
# R script to calculate the log likelihood of each ngram for each speaker compared to the rest 
# of the text. Final filters each ngram to only a most a least likely speaker
########################
# Input: (all created in 02_southpark_corpus.R, dependent on 01_southpark_scrape.R) 
### southpark_tdm.csv: data frame of term document matrix of unigrams
### southpark_bi_tdm.csv: data frame of term document matrix of bigrams
### southpark_tri_tdm.csv: data frame of term document matrix of trigrams
### southpark_quad_tdm.csv: data frame of term document matrix of 4-grams
### southpark_quint_tdm.csv: data frame of term document matrix of 5-grams
########################
# Output: 
### southpark_ngrams.csv: contains ngram, log likelihood and associated figures for each speaker.  
### southpark_ngrams_filtered.csv: filtered ngrams based on most/least likely speakers
########################

library(tm)
library(stringr)

setwd("/Users/kaylinwalker/R/textmining_southpark/tidy data/tdm/")
count.tdm <- read.csv("southpark_tdm.csv", stringsAsFactors=FALSE)
count.bi.tdm <- read.csv("southpark_bi_tdm.csv", stringsAsFactors=FALSE)
count.tri.tdm <- read.csv("southpark_tri_tdm.csv", stringsAsFactors=FALSE)
count.quad.tdm <- read.csv("southpark_quad_tdm.csv", stringsAsFactors=FALSE)
count.quint.tdm <- read.csv("southpark_quint_tdm.csv", stringsAsFactors=FALSE)

################################### DEFINE FUNCTIONS

LL.all <- function(df, speaker) {
     
     LL.df <- NULL
          
     for(word in seq_along(df[,1])) {
          
          word <- df$word[word]
          
          speaker.sums <- data.frame(speaker = names(colSums(df[,1:24])) , total=colSums(df[,1:24]), row.names=NULL)
          word.sums <- data.frame(word = df$word , total=rowSums(df[ ,1:24]), row.names=NULL)
          all.words.total <- sum(speaker.sums$total)
          
          word.total <- word.sums[word.sums$word==word, 2]
          
          speaker.total <- speaker.sums[speaker.sums$speaker==speaker, 2]
          other.total <- all.words.total - speaker.total
          
          speaker.word <- df[df$word==word, ]
          speaker.word <- data.frame(speaker=names(speaker.word), count=t(speaker.word), row.names=NULL)
          speaker.word <- as.numeric(as.character(speaker.word[speaker.word$speaker==speaker, 2]))
          other.word <- word.total - speaker.word
          
          if(speaker.word == 0) speaker.word <- 0.0001
          E1 <- (speaker.total*word.total)/all.words.total
          E2 <- (other.total*word.total)/all.words.total
          LL <- 2*(speaker.word*log(speaker.word/E1) + other.word*log(other.word/E2))
          
          if(E1 > speaker.word) LL <- -1*LL
          
          speaker.word <- round(speaker.word)
          row <- data.frame(speaker, word, word.total, speaker.total, speaker.word, E1, E2, LL)
          
          LL.df <- rbind(LL.df, row)
          
     }
     LL.df <- LL.df[order(-LL.df$LL), ]
     return(LL.df)
}


LL_pass <- function(df, threshold) {
     output <- NULL     
     df <- df[rowSums(df[,1:24]) > threshold, ]
     people <- colnames(df[,c(1:(length(df[1,])-1))])
     for(person in people) {
          temp.p <- subset(df, select=person)
          temp.count <- subset(df, select=word)
          temp.w <- cbind(temp.p, temp.count)
          temp <- LL.all(df=df, speaker=person)
          output <- rbind(output, temp)
     }
     return(output)
}


################################### RUN

system.time(uniLL <- LL_pass(count.tdm, 50)) # 173.1s
system.time(biLL <- LL_pass(count.bi.ws.tdm, 25)) # 618.3s
system.time(triLL <- LL_pass(count.tri.ws.tdm, 15)) # 138.2s
system.time(quadLL <- LL_pass(count.quad.ws.tdm, 10)) # 35.2s
system.time(quintLL <- LL_pass(count.quint.ws.tdm, 5)) # 23.3s


#################################### COMBINE

uniLL$ngram <- 1
biLL$ngram <- 2
triLL$ngram <- 3
quadLL$ngram <- 4
quintLL$ngram <- 5
southpark_ngrams <- rbind(uniLL, biLL, triLL, quadLL, quintLL)

#write.csv(southpark_ngrams, "southpark_ngrams.csv", row.names=FALSE)
#southpark_ngrams <- read.csv("southpark_ngrams.csv", stringsAsFactors=FALSE)

# for each ngram, keep only the highest and lowest LL / reduce from 75552 to 1477 obs. 
ngrams <- southpark_ngrams[abs(southpark_ngrams$LL) >= 10.83, ] 
n.unique <- function(df){
    ngrams.unique <- NULL
    words <- unique(df$word)
    for(h in seq_along(words)) {
        subset <- df[df$word==words[h],]
        if(length(subset[,1]) > 1) subset <- subset[order(-abs(subset$LL)), ]
        ngrams.unique <- rbind(ngrams.unique, subset[1,])
    } 
    return(ngrams.unique)
}
ngrams.unique <- rbind(n.unique(ngrams[ngrams$LL >= 0, ]),
                       n.unique(ngrams[ngrams$LL < 0, ]))


#write.csv(ngrams.unique, "southpark_ngrams_filtered.csv", row.names=FALSE)

