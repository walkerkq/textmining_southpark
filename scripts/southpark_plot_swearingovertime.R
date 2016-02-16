# requires southpark_tdm_episode.csv, generated in southpark_corpus_episode.R
# creates southpark_profanitytime_plot.png

library(ggplot2)
library(RColorBrewer)
library(reshape2)

setwd("/Users/kaylinwalker/R/textmining_southpark/")
by.ep <- read.csv("data/tidy data/southpark_tdm_episode.csv", stringsAsFactors=FALSE)

# get profanity counts 
# think up all the swear words they might use
swears <- c("fuck", "shit", "damn", "hell", "ass", "fatass", "asshole", "motherfucker", 
            "goddman", "goddamnit",  "smartass",
            "goddamned","goddammit", "fucking", "fuckin", "damnit", "damned", "dammit", "bitch", 
            "bitches", "assholes", "asses", "whore")

# subset the unigram .csv
swear.grams <- by.ep[by.ep$word %in% swears, ]
# melt the swear grams df
swearing <- melt(swear.grams, id="word")
swearing$variable <- as.numeric(swearing$variable)
# combine similar words
swearing$word <- gsub("asses|asshole|assholes|fatass", "ass", swearing$word)
swearing$word <- gsub("bitches", "bitch", swearing$word)
swearing$word <- gsub("damnit|damned|dammit|goddammit|goddamned|goddamn", "damn", swearing$word)
swearing$word <- gsub("damnit", "damn", swearing$word)
swearing$word <- gsub("fuckin|fucking", "fuck", swearing$word)
swearing <- aggregate(value ~ word + variable, swearing, sum)

mycolors <- c("#C20631", "#673e1e", "#5BE1C6", "#F5871F", "#333333","#266E35", "#000000")
ggplot(swearing, aes(variable, value)) + geom_line(aes(color=word)) + 
    labs(title="Profanity in South Park Over Time") + 
    xlab("Season") + ylab("Times in Script") +
    scale_color_manual(values = mycolors) +
    scale_x_continuous(limits=c(0,257), 
                       breaks=c(1,14,32,49,66,80,97,112,126,140,154,168,182,196,210,224,238,248), 
                       labels=c(1:18)) +
    annotate("text", x=190, y=47, label="1309: Butters' Bottom Bitch", size=5, color="#673e1e") + 
    annotate("text", x=66, y=134, label="0501: It Hits the Fan", size=6, color="#266E35") +
    annotate("text", x=15, y=38, label="0109: Mr. Hankey,\nthe Christmas Poo", size=4, color="#673e1e") + 
    annotate("text", x=220, y=35, label="1507: You're Getting Old", size=4, color="#266E35") +
    annotate("text", x=45, y=52, label="0409: Do the Handi-\ncapped Go to Hell?", size=4, color="#333333") + 
    annotate("text", x=50, y=42, label="0410: Probably", size=4, color="#333333") +
    annotate("text", x=120, y=36, label="0912: Stupid Spoiled\nWhore Video Playset", size=4, color="#000000") + 
    annotate("text", x=161, y=30, label="1108: Le Petit Tourette", size=4, color="#C20631") + 
    theme_classic() + 
    theme(plot.title = element_text(size=18), 
          axis.title=element_text(size=16), 
          legend.text = element_text(size = 14), 
          legend.position=c(.9,.75))

#dev.copy(png, 'plots/southpark_profanitytime_plot.png')
#dev.off()
