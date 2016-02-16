# generates southpark_seasonshare_plot.png

library(stringr)
library(ggplot2)

# read in the data set
setwd("/Users/kaylinwalker/R/textmining_southpark/data/raw data")
if(file.exists("all-seasons.csv")) {
    scripts <- read.csv("all-seasons.csv", stringsAsFactors=FALSE)
} else { 
    url <- "https://raw.githubusercontent.com/BobAdamsEE/SouthParkData/master/All-seasons.csv"
    scripts <- download.file(url, "all-seasons.csv", method="curl")
    scripts <- read.csv("all-seasons.csv", stringsAsFactors=FALSE)
}
scripts <- scripts[-7977, ] # a line in season 12 referring to a chef and not Chef

# get total count by season 
season.count <- NULL
for(n in 1:18) {
    subset <- scripts[scripts$Season==n, ]
    text <- str_c(subset$Line, collapse=" ")
    text <- gsub("\\n", "", text)
    text <- gsub("[[:punct:]]", "", text)
    text <- tolower(text)
    count <- length(strsplit(text, " ")[[1]])
    unique <- length(unique(strsplit(text, " ")[[1]]))
    row <- data.frame(season=n, count, unique)
    season.count <- rbind(season.count, row)
}

# get text by speaker + season 
scripts$Character <- gsub("Mrs. Garrison", "Mr. Garrison", scripts$Character) # combine Garrisons
speakers <- c("Cartman", "Randy", "Kyle", "Stan", "Kenny", "Butters", "Chef", "Mr. Garrison") # keep only top speakers
speaker.season <- NULL
for(speaker in speakers){
    subset <- scripts[scripts$Character==speaker, ]
    for(season in unique(subset$Season)) { 
        subset2 <- subset[subset$Season==season, ]
        text <- str_c(subset2$Line, collapse=" ")
        row <- data.frame(season, speaker, text)
        speaker.season  <- rbind(speaker.season, row)
    }
}
rm(scripts)

# clean up the data frame
for(u in 1:3) speaker.season[,u] <- as.character(speaker.season[,u])
speaker.season$season <- as.numeric(speaker.season$season)
speaker.season$text <- tolower(speaker.season$text)
speaker.season$text <- gsub("[[:punct:]]", "", speaker.season$text)
speaker.season$text <- gsub("\\n", "", speaker.season$text)

# get word counts
for(r in seq_along(speaker.season[,1])) speaker.season$count[r] <- length(strsplit(speaker.season$text[r], " ")[[1]])
for(r in seq_along(speaker.season[,1])) speaker.season$unique[r] <- length(unique(strsplit(speaker.season$text[r], " ")[[1]]))

ss <- speaker.season[,c(1,2,4,5)]
ss$Season.Count <- 0
for(i in seq_along(ss[,1])) ss$season.count[i] <- season.count[season.count$season==ss$season[i], 2]
ss$season.share <- round(ss$count/ss$season.count, 4)
ss$speaker <- factor(ss$speaker, levels=c("Cartman", "Stan", "Kyle", "Butters", "Randy", "Chef", "Mr. Garrison", "Kenny"))
mycolors <- c("#C20631", "#673e1e", "#21B726", "#5BE1C6", "#266E35",  "#666666", "#000000",  "#F5871F")
ggplot(ss, aes(season, season.share)) + 
    geom_bar(stat="identity", aes(fill=speaker)) + 
    scale_fill_manual(values = mycolors) +
    facet_grid(. ~ speaker) +
    theme_bw() + labs(title="Share of Words Spoken by Season") + xlab("Season") + ylab("Share (%) of Words") +
    theme(plot.title = element_text(size=18),
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          legend.text = element_text(size = 14), 
          legend.position=1,
          strip.text.x = element_text(size = 14)) 

#dev.copy(png, 'plots/southpark_seasonshare_plot.png')
#dev.off()