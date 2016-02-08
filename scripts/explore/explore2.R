library(ggplot2)
setwd("/Users/kaylinwalker/R/textmining_southpark/")
ep <- read.csv("raw data/southpark_byepisode_scripts.csv", stringsAsFactors=FALSE)

# who talks the most per episode?
ep$text <- iconv(ep$text, "ISO-8859-1", "UTF-8")
ep$text <- tolower(ep$text)
ep$text <- gsub("[[:punct:]]", "", ep$text)

for(h in seq_along(ep[,1])){
     j <- strsplit(ep$text[h], " ")     
     ep$total[h] <- length(j[[1]])
     ep$unique[h] <- length(unique(j[[1]]))
}

avgs <- NULL
allscores <- NULL
for(speaker in unique(ep$speaker)){
     subset <- ep[ep$speaker==speaker, ]
     subset$avg.ratio <- round(subset$unique/subset$total, 2)
     subset <- subset[subset$total > 100, ]
     ep.avg <- round(mean(subset$avg.ratio),4)
     
     total <- sum(subset$total)
     if(total > 4000) { 
          subset <- subset[complete.cases(subset), ]
          alltext <- str_c(subset$text, collapse=" ")
          alltext <- strsplit(alltext, " ")
          unique <- length(unique(alltext[[1]]))
          overall <- round(unique/total, 4)
          
          row <- data.frame(speaker, total, unique, ep.avg, overall)
          avgs <- rbind(avgs, row)
     }
}