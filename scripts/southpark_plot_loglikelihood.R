library(ggplot2)
library(RColorBrewer)
library(png)
library(grid)

setwd("/Users/kaylinwalker/R/textmining_southpark/")
count.all <- read.csv("tidy data/southpark_tdm_all.csv", stringsAsFactors=FALSE)

# function to get the log likelihood of each of a 
# speaker's words vs. the rest of the dataframe
LL.all <- function(df, speaker) {
     
     LL.df <- NULL
     
     for(word in seq_along(df[,1])) {
          
          word <- df$word[word]
          
          speaker.sums <- data.frame(speaker = names(colSums(df[,1:28])) , total=colSums(df[,1:28]), row.names=NULL)
          word.sums <- data.frame(word = df$word , total=rowSums(df[ ,1:28]), row.names=NULL)
          all.words.total <- sum(speaker.sums$total)
          
          word.total <- word.sums[word.sums$word==word, 2]
          
          speaker.total <- speaker.sums[speaker.sums$speaker==speaker, 2]
          if(speaker.total == 0) speaker.total <- 0.0001
          other.total <- all.words.total - speaker.total
          
          speaker.word <- df[df$word==word, ]
          speaker.word <- data.frame(speaker=names(speaker.word), count=t(speaker.word), row.names=NULL)
          speaker.word <- as.numeric(as.character(speaker.word[speaker.word$speaker==speaker, 2]))
          other.word <- word.total - speaker.word
          
          if(speaker.word == 0) speaker.word <- 0.0001
          E1 <- (speaker.total*word.total)/all.words.total
          E2 <- (other.total*word.total)/all.words.total
          LL <- 2*(speaker.word*log(speaker.word/E1) + other.word*log(other.word/E2))
          
          if(abs(LL) > 10.83) {
               if(E1 > speaker.word) LL <- -1*LL
               speaker.word <- round(speaker.word)
               speaker.total <- round(speaker.total)
               row <- data.frame(speaker, word, word.total, speaker.total, speaker.word, E1, E2, LL)
               LL.df <- rbind(LL.df, row)
          }
          
     }
     LL.df <- LL.df[order(-LL.df$LL), ]
     return(LL.df)
}

# create a staging function to pass a speaker at a time through the LL function
LL_pass <- function(df, threshold) {
     output <- NULL     
     df <- df[rowSums(df[,1:28]) > threshold, ]
     people <- colnames(df[,c(1:(length(df[1,])-1))])
     for(person in people) {
          temp.p <- subset(df, select=person)
          temp.count <- subset(df, select=word)
          temp.w <- cbind(temp.p, temp.count)
          temp <- LL.all(df=df, speaker=person)
          output <- rbind(output, temp)
     }
     return(output)
}

# pass the words through the log likelihood function
#### FYI: this takes a while.
allLL <- LL_pass(count.all, 25)

### save the file for easy later access.
#write.csv(allLL, "data/tidy data/southpark_ngrams.csv", row.names=FALSE)
#allLL <- read.csv("data/tidy data/southpark_ngrams.csv", stringsAsFactors=FALSE)

# for each ngram, keep only the highest and lowest LL  
ngrams <- allLL[abs(allLL$LL) >= 10.83, ] 
n.unique <- function(df){
     ngrams.unique <- NULL
     words <- unique(df$word)
     for(h in seq_along(words)) {
          subset <- df[df$word==words[h],]
          if(length(subset[,1]) > 1) subset <- subset[order(-abs(subset$LL)), ]
          ngrams.unique <- rbind(ngrams.unique, subset[1,])
     } 
     return(ngrams.unique)
}
ngrams.unique <- rbind(n.unique(ngrams[ngrams$LL >= 0, ]),
                       n.unique(ngrams[ngrams$LL < 0, ]))


# keep just main speakers 
main.speakers <- c("Cartman", "Stan", "Kyle", "Kenny", "Butters", "Randy")
plot <- allLL[allLL$speaker %in% main.speakers, ]

# remove phrases like 'ha ha' and 'you you'
plot <- plot[!grepl("\\b(\\w+)\\s\\1\\b", plot$word, perl=TRUE), ]

# split by speaker, rank by log likelihood  * ngram length, keep the top 25
rankbyspeaker <- function(df, direction) {
     rank <- NULL
     speakers <- unique(df$speaker)
     for(j in speakers) {
          subset <- df[df$speaker==j,]
          subset$ngram <- sapply(subset$word, function(x) length(strsplit(as.character(x), " ")[[1]]))
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
ranked$speaker <- factor(ranked$speaker, levels=c('Cartman', 'Stan', 'Kyle','Kenny', 
                                                  'Butters', 'Randy'))

# generate plot
ec <- readPNG("plots/images/cartman.png"); cartman <- rasterGrob(ec, interpolate=TRUE)
sm <- readPNG("plots/images/stan.png"); stan <- rasterGrob(sm, interpolate=TRUE)
km <- readPNG("plots/images/kenny.png"); kenny <- rasterGrob(km, interpolate=TRUE)
kb <- readPNG("plots/images/kyle.png"); kyle <- rasterGrob(kb, interpolate=TRUE)
mg <- readPNG("plots/images/randy.png"); randy <- rasterGrob(mg, interpolate=TRUE)
bs <- readPNG("plots/images/butters.png"); butters <- rasterGrob(bs, interpolate=TRUE)

mycolors <- c("#C20631", "#673e1e", "#21B726", "#F5871F", "#5BE1C6", "#266E35")

ggplot(ranked, aes(speaker, ((rank2*-1)-4))) + 
     geom_point(color="white") + 
     geom_label(aes(label=ranked$word,fill=ranked$speaker), color='white', fontface='bold', size=5) +
     scale_fill_manual(values = mycolors) +
     theme_classic() +
     theme(legend.position=1,plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0))) + 
     labs(title="Most Characteristic Words/Phrases per Person") + 
     xlab("") + ylab("Ranking") +
     scale_y_continuous(limits=c(-29,-1), breaks=c(-24, -14, -4.5), labels=c("#20", "#10", "#1")) +
     annotation_custom(cartman, xmin=.5, xmax=1.5, ymin=0, ymax=-4) + 
     annotation_custom(stan, xmin=1.5, xmax=2.5, ymin=0, ymax=-4) + 
     annotation_custom(kyle, xmin=2.5, xmax=3.5, ymin=0, ymax=-4) + 
     annotation_custom(kenny, xmin=3.5, xmax=4.5, ymin=0, ymax=-4) + 
     annotation_custom(butters, xmin=4.5, xmax=5.5, ymin=0, ymax=-4) + 
     annotation_custom(randy, xmin=5.5, xmax=6.5, ymin=0, ymax=-4) 

dev.copy(png, 'plots/southpark_ranked_plot.png')
dev.off()