# requires southpark_tdm_episode.csv, generated in southpark_corpus_episode.R
# creates southpark_profanitychanges_plot.png

library(ggplot2)
library(RColorBrewer)
library(reshape2)

setwd("/Users/kwalker/git_projects/textmining_southpark/")
by.ep <- read.csv("data/tidy data/southpark_tdm_episode.csv", stringsAsFactors=FALSE)

# look at two profanities
subset <- data.frame(t(by.ep[by.ep$word %in% c("ass", "fuck"), ]))
colnames(subset) <- c("ass", "fuck")
subset$episode <- row.names(subset)
subset <- subset[-length(subset[,1]), ]
subset$episode <- as.numeric(substring(subset$episode, 2))
subset$season <- cut(subset$episode, breaks=c(0,14,32,49,66,80,97,112,126,140,154,168,182,196,210,224,238,248,257), labels=1:18)
for(j in 1:(length(subset)-1)) subset[,j] <- as.numeric(as.character(subset[,j]))

# prepare to plot
subset2 <- melt(subset[,-(length(subset)-1)], id="season")
subset2 <- aggregate(value ~ season + variable, subset2, sum)
colnames(subset2) <- c("Season", "Word", "Number")
mycolors <- c("#C20631", "#F5871F")
ggplot(subset2, aes(Season, Number)) + 
    geom_bar(aes(fill=Word), stat="identity", position="dodge") + 
    theme_classic() + 
    scale_fill_manual(values = mycolors) +
    labs(title="Profanity in South Park Over Time") +
    ylab("Mentions in Script") +
    theme(plot.title = element_text(size=18),
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          legend.text = element_text(size = 14), 
          legend.position=c(.9,.75)) 

#dev.copy(png, 'plots/southpark_profanitychanges_plot.png')
#dev.off()

