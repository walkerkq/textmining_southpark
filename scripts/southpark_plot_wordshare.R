library(ggplot2)
library(RColorBrewer)

setwd("/Users/kaylinwalker/R/textmining_southpark/")
count.all <- read.csv("data/tidy data/southpark_tdm_speaker.csv", stringsAsFactors=FALSE)

totals <- rbind(names(count.all[,1:28]), colSums(count.all[,1:28]))
total <- data.frame(t(totals))

colnames(total) <- c("speaker", "total")
total$total <- as.numeric(as.character(total$total))
small <- total[total$total < 10000 | total$speaker=="All.others", ]
row <- data.frame(speaker="All.others", total=sum(small$total))
total <- rbind(total[total$total >= 10000 & total$speaker!="All.others", ], row)

# get the average words per episode, assuming that the main speakers are in each
total$per.Episode <- round(as.numeric(as.character(total$total))/264, 2)
total <- total[order(-total$per.Episode), ]
total$share <- round(total$per.Episode/sum(total$per.Episode), 3)*100

# make the plot
total$speaker <- factor(total$speaker, levels=c("All.others", "Cartman", "Stan", "Kyle", "Randy", "Butters"))
total$group <- " "
mycolors <- c("#666666", "#C20631", "#673e1e",  "#21B726", "#266E35", "#5BE1C6")
wordshare <- ggplot(total, aes(group,share)) + 
     geom_bar(stat="identity", aes(fill=speaker)) +
     theme_classic() + labs(title="Cartman Talks the Most") + 
     ylab("Share of Words Spoken (Episode Avg. %)") + 
     scale_fill_manual(values = mycolors[1:6]) + xlab("") +
     theme(legend.position=1,plot.title = element_text(size=18), 
           axis.title.y=element_text(margin=margin(0,10,0,0)),
           axis.title=element_text(size=16)) +
     geom_text(aes(x=group, y=cumsum(share)-share*0.5, 
                   label=paste(speaker,": ",share,"%", sep="")), 
               color="white", fontface="bold", size=4)

#dev.copy(png, 'plots/southpark_wordshare_plot.png')
#dev.off()
