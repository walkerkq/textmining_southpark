# requires southpark_tdm_speaker.csv, generated in southpark_corpus_speaker.R
# creates southpark_swearsalot_plot.png

library(ggplot2)
library(RColorBrewer)

setwd("/Users/kaylinwalker/R/textmining_southpark/")
count.all <- read.csv("data/tidy data/southpark_tdm_speaker.csv", stringsAsFactors=FALSE)

# think up all the swear words they might use
swears <- c("fuck", "shit", "damn", "hell", "ass", "fatass", "asshole", "motherfucker", 
            "goddman", "goddamnit",  "smartass", "screwed", "screw", "screwing",
            "goddamned","goddammit", "fucking", "fuckin", "damnit", "damned", "dammit", "bitch", 
            "bitches", "assholes", "asses")

# subset the unigram .csv
swear.grams <- count.all[count.all$word %in% swears, ]
swear.totals <- rbind(names(swear.grams[,1:28]), colSums(swear.grams[,1:28]))
totals <- rbind(names(count.all[,1:28]), colSums(count.all[,1:28]))
totes <- data.frame(t(rbind(totals, swear.totals[2,])))
colnames(totes) <- c("speaker", "total", "swears")
totes$total <- as.numeric(as.character(totes$total))
totes$swears <- as.numeric(as.character(totes$swears))

# calculate the overall rate of swearing
overall <- round(sum(totes$swears)/sum(totes$total), 4)

# calculate the swearing rate per person
totes$swear.rate <- round(totes$swears/totes$total, 4)

# create the plot 
totes <- totes[(totes$total > 3500 | totes$speaker=="Kenny" ) & totes$speaker!="All.others", ]
totes <- totes[order(-totes$swear.rate), ]
totes$speaker <- factor(totes$speaker, levels=totes$speaker)
mycolors <- c("#F5871F", "#C20631", "#760411", "#21B726", "#b7aa97", "#673e1e",
              "#266E35", "#04392c", "#75bed1", "#5BE1C6", "#8d6798")
swearing <- ggplot(totes, aes(speaker, swear.rate*1000)) + 
     geom_bar(stat="identity", aes(fill=speaker)) + 
     geom_hline(yintercept=overall*1000, linetype="dashed") + 
     theme_classic() + 
     labs(title="Kenny Swears at the Highest Rate") + 
     ylab("Profanities per 1000 Words") + 
     xlab("") + 
     annotate("text", x=11, y=(overall*1000)+1, label="Average", size=5) + 
     scale_fill_manual(values = mycolors) +
     geom_text(aes(x=speaker, y=(swear.rate*1000)-2, label=swear.rate*1000), 
               color="white", fontface="bold", size=6) + 
     theme(legend.position=1,plot.title = element_text(size=20), 
           axis.title.y=element_text(margin=margin(0,10,0,0)),
           axis.text.x = element_text(angle = 15, hjust = 1, size=14),
           axis.text.y = element_text(size=14),
           axis.title=element_text(size=16))

#dev.copy(png, 'plots/southpark_swearsalot_plot.png')
#dev.off()

