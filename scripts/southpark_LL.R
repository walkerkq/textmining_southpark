library(tm)
library(stringr)
setwd("/Users/kaylinwalker/R/textmining_southpark/tidy data/tdm/")
count.tdm <- read.csv("southpark_tdm.csv", stringsAsFactors=FALSE)
count.bi.tdm <- read.csv("southpark_bi_tdm.csv", stringsAsFactors=FALSE)
count.tri.tdm <- read.csv("southpark_tri_tdm.csv", stringsAsFactors=FALSE)
count.bi.ws.tdm <- read.csv("southpark_bi_tdm_wstop.csv", stringsAsFactors=FALSE)
count.tri.ws.tdm <- read.csv("southpark_tri_tdm_wstop.csv", stringsAsFactors=FALSE)
count.quad.ws.tdm <- read.csv("southpark_quad_tdm_wstop.csv", stringsAsFactors=FALSE)
count.quint.ws.tdm <- read.csv("southpark_quint_tdm_wstop.csv", stringsAsFactors=FALSE)


LL.all <- function(df, wordlist, speaker) {
     
     LL.df <- NULL
     
     if(missing(wordlist)) wordlist <- df
     
     for(word in seq_along(wordlist[,1])) {
          
          word <- wordlist$word[word]
          
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
          
          if(speaker.word != 0) { 
               E1 <- (speaker.total*word.total)/all.words.total
               E2 <- (other.total*word.total)/all.words.total
               LL <- 2*(speaker.word*log(speaker.word/E1) + other.word*log(other.word/E2))
               
               if(E1 < speaker.word) direction <- "HIGH"
               if(E1 >= speaker.word) direction <- "LOW"
               
               row <- data.frame(speaker, word, word.total, speaker.total, speaker.word, E1, E2, LL, direction)
               
               LL.df <- rbind(LL.df, row)
          }
          
     }
     LL.df <- LL.df[order(LL.df$direction, -LL.df$LL), ]
     return(LL.df)
}

############################ UNIGRAMS ############################

everybody <- NULL
people <- c("CARTMAN", "STAN", "KENNY", "CHEF", "MR..GARRISON", "WENDY", "NARRATOR", "ANNOUNCER", "JIMBO", "MAYOR", "JESUS", "TERRANCE", "REPORTER", "COUNSELOR.MACKEY", "SHARON", "RANDY", "JIMMY", "BUTTERS", "CHRIS", "MS..CHOKSONDIK", "MEPHESTO", "MS..CARTMAN", "KYLE", "ALL.OTHERS")
for(person in people) {
     temp <- LL.all(count.tdm, count.tdm, person)
     everybody <- rbind(everybody, temp)
}
#write.csv(everybody, "LL_each.csv", row.names=FALSE)


############################ BI GRAMS ############################
bi.everybody <- NULL
for(person in people) {
     temp <- LL.all(df=count.bi.ws.tdm, count.bi.ws.tdm, speaker=person)
     bi.everybody <- rbind(bi.ws.everybody, temp)
}
#write.csv(bi.everybody, "LL_bi_each.csv", row.names=FALSE)
bi.ws.everybody <- NULL
for(person in people) {
     temp.p <- subset(count.bi.ws.tdm, select=person)
     temp.count <- subset(count.bi.ws.tdm, select=word)
     temp.c <- cbind(temp.p, temp.count)
     temp.c <- temp.c[temp.c[,1] > 5, ]
     temp <- LL.all(df=count.bi.ws.tdm, wordlist=temp.c, speaker=person)
     bi.ws.everybody <- rbind(bi.ws.everybody, temp)
}
#write.csv(bi.ws.everybody, "LL_bi_ws_each.csv", row.names=FALSE)

############################ TRI GRAMS ############################

tri.everybody <- NULL
for(person in people) {
     temp <- LL.all(df=count.tri.ws.tdm, wordlist=temp.c, speaker=person)
     tri.everybody <- rbind(tri.everybody, temp)
}
#write.csv(tri.everybody, "LL_tri_each.csv", row.names=FALSE)
tri.ws.everybody <- NULL
for(person in people) {
     temp.p <- subset(count.tri.ws.tdm, select=person)
     temp.count <- subset(count.tri.ws.tdm, select=word)
     temp.c <- cbind(temp.p, temp.count)
     temp.c <- temp.c[temp.c[,1] > 5, ]
     if(length(temp.c[,1]) > 0) {
          temp <- LL.all(df=count.tri.ws.tdm, wordlist=temp.c, speaker=person)
          tri.ws.everybody <- rbind(tri.ws.everybody, temp)
     }
}
#write.csv(tri.everybody, "LL_tri_ws_each.csv", row.names=FALSE)


############################ QUAD GRAMS ############################

quad.ws.everybody <- NULL
for(person in people) {
     temp.p <- subset(count.quad.ws.tdm, select=person)
     temp.count <- subset(count.quad.ws.tdm, select=word)
     temp.c <- cbind(temp.p, temp.count)
     temp.c <- temp.c[temp.c[,1] > 2, ]
     if(length(temp.c[,1]) > 0) {
          temp <- LL.all(df=count.quad.ws.tdm, wordlist=temp.c, speaker=person)
          quad.ws.everybody <- rbind(quad.ws.everybody, temp)
     }
}
#write.csv(quad.ws.everybody, "LL_quad_ws_each.csv", row.names=FALSE)

############################ 5 GRAMS ############################

quint.ws.everybody <- NULL
for(person in people) {
     temp.p <- subset(count.quint.ws.tdm, select=person)
     temp.count <- subset(count.quint.ws.tdm, select=word)
     temp.c <- cbind(temp.p, temp.count)
     temp.c <- temp.c[temp.c[,1] > 2, ]
     if(length(temp.c[,1]) > 0) {
          temp <- LL.all(df=count.quint.ws.tdm, wordlist=temp.c, speaker=person)
          quint.ws.everybody <- rbind(quint.ws.everybody, temp)
     }
}
#write.csv(quint.ws.everybody, "LL_quad_ws_each.csv", row.names=FALSE)


################## Combine it all together ###################### 
everybody$ngram <- 1
bi.ws.everybody$ngram <- 2
tri.ws.everybody$ngram <- 3
quad.ws.everybody$ngram <- 4
quint.ws.everybody$ngram <- 5
southpark_ngrams <- rbind(everybody, bi.ws.everybody, tri.ws.everybody, quad.ws.everybody, quint.ws.everybody)

#write.csv(southpark_ngrams, "southpark_ngrams.csv", row.names=FALSE)



