# walkerkq 2.5.2016 
# textmining_southpark
########################
# R script to refine the ngram results and display visually. 
########################
# Input: (created in 03_southpark_likelihood.R, dependent on 02_southpark_corpus.R and 01_southpark_scrape.R) 
### southpark_ngrams.csv: contains ngram, log likelihood and associated figures for each speaker
########################
# Output: 
### southpark_ranked_plot.png
########################

library(ggplot2)
library(RColorBrewer)

#setwd("/Users/kaylinwalker/R/textmining_southpark/scripts")
#source("01_southpark_scrape.R") 
#source("02_southpark_corpus.R") 
#source("03_southpark_loglikelihood.R") 

#setwd("/Users/kwalker/git_projects/textmining_southpark/tidy data/")
setwd("/Users/kaylinwalker/R/textmining_southpark/tidy data")
ngrams <- read.csv("southpark_ngrams_filtered_nostop.csv", stringsAsFactors=FALSE)

# keep just main speakers 
main.speakers <- c("CARTMAN", "STAN", "KYLE", "KENNY", "RANDY", 
                   "BUTTERS", "MR..GARRISON", "MS..CARTMAN")
plot <- ngrams[ngrams$speaker %in% main.speakers, ]

# split by speaker, rank by log likelihood  * ngram length, keep the top 25
rankbyspeaker <- function(df, direction) {
    rank <- NULL
    speakers <- unique(df$speaker)
    for(j in speakers) {
        subset <- df[df$speaker==j,]
        subset$rank <- subset$LL*subset$ngram
        subset <- subset[order(-subset$rank),]
        if(length(subset[,1]) > 25) subset <- subset[1:25,] 
        row.names(subset) <- NULL
        subset$rank2 <- as.numeric(row.names(subset))
        rank <- rbind(rank, subset)
    }
    return(rank)
}
ranked <- rankbyspeaker(plot)

# generate plot
ggplot(ranked, aes(speaker, (rank2*-1))) + 
    geom_point(color="white") + 
    geom_label(aes(label=ranked$word,fill=ranked$speaker), color='white', fontface='bold') +
    scale_fill_brewer(palette="Paired") +
    theme_classic() +
    theme(legend.position=1,plot.title = element_text(size=22)) + 
    labs(title="Most Characteristic Words & Phrases by South Park Character") + 
    xlab("") + ylab("Ranking") +
    scale_y_continuous(breaks = c(-20, -10, -1), labels = c("#20", "#10", "#1"))
