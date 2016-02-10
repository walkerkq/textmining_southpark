library(wordcloud)

setwd("/Users/kaylinwalker/R/textmining_southpark/")
count.all <- read.csv("data/tidy data/southpark_tdm_all.csv", stringsAsFactors=FALSE)

findFreq <- function(df){
     df$total <- rowSums(df[,1:28])
     freqs <- df[,29:30]
     freqs <- freqs[order(-freqs$total), ]
     return(freqs)
}
freq1 <- findFreq(count.all)
cloud <- wordcloud(freq1$word, freq1$total, scale=c(2.5,0.5), min.freq=100, max.words=200, random.order=FALSE)

dev.copy(png,'plots/southpark_wordcloud.png')
dev.off()
