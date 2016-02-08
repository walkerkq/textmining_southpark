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
library(png)
library(grid)

#setwd("/Users/kaylinwalker/R/textmining_southpark/scripts")
#source("01_southpark_scrape.R") 
#source("02_southpark_corpus.R") 
#source("03_southpark_loglikelihood.R") 

setwd("/Users/kwalker/git_projects/textmining_southpark")
#setwd("/Users/kaylinwalker/R/textmining_southpark")
ngrams <- read.csv("tidy data/southpark_ngrams_filtered.csv", stringsAsFactors=FALSE)

# keep just main speakers 
main.speakers <- c("CARTMAN", "STAN", "KYLE", "KENNY", 
                   "BUTTERS", "MR..GARRISON")
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
ranked <- ranked[order(-ranked$speaker.total), ]
ranked$speaker <- factor(ranked$speaker, levels=c('CARTMAN', 'STAN', 'KYLE','KENNY', 
                                                  'BUTTERS', 'MR..GARRISON'))

# generate plot
ec <- readPNG("plots/images/cartman.png"); cartman <- rasterGrob(ec, interpolate=TRUE)
sm <- readPNG("plots/images/stan.png"); stan <- rasterGrob(sm, interpolate=TRUE)
km <- readPNG("plots/images/kenny.png"); kenny <- rasterGrob(km, interpolate=TRUE)
kb <- readPNG("plots/images/kyle.png"); kyle <- rasterGrob(kb, interpolate=TRUE)
mg <- readPNG("plots/images/garrison.png"); garrison <- rasterGrob(mg, interpolate=TRUE)
bs <- readPNG("plots/images/butters.png"); butters <- rasterGrob(bs, interpolate=TRUE)

mycolors <- c("#C20631", "#673e1e", "#21B726", "#F5871F", "#5BE1C6", "#266E35")

ggplot(ranked, aes(speaker, ((rank2*-1)-4))) + 
    geom_point(color="white") + 
    geom_label(aes(label=ranked$word,fill=ranked$speaker), color='white', fontface='bold', size=5) +
    scale_fill_manual(values = mycolors) +
    theme_classic() +
    theme(legend.position=1,plot.title = element_text(size=22), axis.title.y=element_text(margin=margin(0,10,0,0))) + 
    labs(title="Most Characteristic Ngrams of South Park Characters") + 
    xlab("") + ylab("Ranking") +
    scale_y_continuous(limits=c(-29,-1), breaks=c(-24, -14, -4.5), labels=c("#20", "#10", "#1")) +
    annotation_custom(cartman, xmin=.5, xmax=1.5, ymin=0, ymax=-4) + 
    annotation_custom(stan, xmin=1.5, xmax=2.5, ymin=0, ymax=-4) + 
    annotation_custom(kyle, xmin=2.5, xmax=3.5, ymin=0, ymax=-4) + 
    annotation_custom(kenny, xmin=3.5, xmax=4.5, ymin=0, ymax=-4) + 
    annotation_custom(butters, xmin=4.5, xmax=5.5, ymin=0, ymax=-4) + 
    annotation_custom(garrison, xmin=5.5, xmax=6.5, ymin=0, ymax=-4) 

