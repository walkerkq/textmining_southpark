## exploring southpark data
######## questions: 
## who is in each episode?
## who talks the most?
## who swears the most?
## who says other people's names the most?
## what words are most frequent overall, by person and by episode?
## 
library(ggplot2)
setwd("/Users/kaylinwalker/R/textmining_southpark/")
ep <- read.csv("raw data/southpark_byepisode_scripts.csv", stringsAsFactors=FALSE)


## number of episodes each person is in
presence <- as.data.frame.matrix(table(ep$speaker, ep$episode))
# eliminate non-recurring characters (> 5 appearances)
presence$total <- rowSums(presence)
presence <- presence[presence$total > 20, ] # cuts 1808 down to 102
presence$speaker <- row.names(presence)
presence <- presence[order(-presence$total), ]
presence$speaker <- factor(presence$speaker, levels=presence$speaker)
ggplot(presence, aes(speaker, total)) + geom_bar(stat="identity")

presence2 <- as.data.frame.matrix(table(ep$episode,ep$speaker))
sums <- colSums(presence2)
presence2 <- presence2[, sums > 50]
cumulative <- NULL
for(column in seq_along(presence2)) { 
     person <- presence2[ , column]
     person <- data.frame(ep=row.names(presence2), present=person)
     person$num <- sapply(person$ep, function (x) as.numeric(strsplit(as.character(x), " ")[[1]][1]))
     person <- person[order(person$num), ]
     person$cum <- cumsum(person$present)
     item <- data.frame(num=person$num, present=person$present, cum=person$cum, person=colnames(presence2)[column])
     cumulative <- rbind(cumulative, item)
}
ggplot(cumulative, aes(num, cum)) + geom_line(aes(group=person, color=person))


# who talks the most per episode?
ep$text <- iconv(ep$text, "ISO-8859-1", "UTF-8")
ep$text <- tolower(ep$text)
ep$text <- gsub("[[:punct:]]", "", ep$text)

for(h in seq_along(ep[,1])){
     j <- strsplit(ep$text[h], " ")     
     ep$total[h] <- length(j[[1]])
     ep$unique[h] <- length(unique(j[[1]]))
}

recurring <- data.frame(table(ep$speaker))
recurring <- recurring[recurring$Freq > 20, ]
combine.others <- NULL
for(episode in unique(ep$episode)){
     sub <- ep[ep$episode==episode, ]
     total.words <- sum(sub$total)
     sub$share <- round(sub$total/total.words, 2)
     sub.top <- sub[sub$share >= 0.03 & sub$speaker %in% recurring$Var1, ]
     sub.other <- sub[sub$share < 0.03 | !(sub$speaker %in% recurring$Var1), ]
     sub.other <- sub.other[complete.cases(sub.other),]
     sub.other.text <- str_c(sub.other$text, collapse=" ")
     sub.other.text <- strsplit(sub.other.text, " ")
     total <- length(sub.other.text[[1]])
     unique <- length(unique(sub.other.text[[1]]))
     share <- round(total/total.words, 2)
     row <- data.frame(episode, speaker="ALL.OTHERS", total, unique, share)
     combine.others <- rbind(combine.others, sub.top[,c(1,2,4,5,6)], row)
}
combine.others <- combine.others[order(combine.others$episode, combine.others$share),]
table(combine.others$speaker)
ggplot(combine.others, aes(episode, share)) + geom_bar(stat="identity", aes(fill=speaker)) + theme_classic()

max <- NULL
for(episode in unique(combine.others$episode)) {
     subset <- combine.others[combine.others$episode==episode & combine.others$speaker!="ALL.OTHERS", ]
     max2 <- max(subset$share)
     keep <- subset[subset$share==max2, ]
     max <- rbind(max, keep)
}
table(max$speaker)

h <- aggregate(total ~ speaker, ep, mean)
j <- aggregate(unique ~ speaker, ep, mean)

epcount <- NULL
for(episode in unique(ep$episode)){
     sub <- ep[ep$episode==episode, ]
     sub <- sub[complete.cases(sub),]
     text <- str_c(sub$text, collapse=" ")
     texts <- strsplit(text, " ")
     total <- length(texts[[1]])
     unique <- length(unique(texts[[1]]))
     row <- data.frame(episode, total, unique)
     epcount <- rbind(epcount, row)
}
ggplot(epcount, aes(episode, unique)) + geom_line(group=1)
