# walkerkq 2.3.2016 
# textmining_southpark
########################
# R script to scrape South Park transcripts from the Internet Movie Script Database
# located at http://www.imsdb.com/TV/South%20Park.html using the XML, stringr and 
# RCurl packages. 
########################
# Input: none 
########################
# Output: 
### southpark_all_scripts.csv: each line of the transcript and its speaker, episode and episode number
### southpark_byepisode_scripts.csv: combined text from each speaker in each episode
### southpark_byperson_scripts.csv: combined text from each speaker across all episodes  
########################

library("RCurl")
library("XML")
library("stringr")

# set directory
setwd("/Users/kaylinwalker/R/textmining_southpark/")

# get list of episodes
URL <- "http://www.imsdb.com/TV/South%20Park.html"
listing <- htmlTreeParse(getURL(URL), useInternal=TRUE)
listing.raw <- xpathApply(listing, "//p", xmlValue)
listing.u <- unlist(listing.raw)
listing.s <- strsplit(listing.u, "\\(")
listing.f <- NULL
for(x in seq_along(listing.s)) listing.f[x] <- listing.s[[x]][1]
listing.f <- gsub("^\\s+|\\s+$", "", listing.f)
listing.f <- tolower(listing.f)
listing.f <- gsub("\\s", "-", listing.f)

# loop through episode pages, scrape lines and store in all.text

URLstart <- "http://www.imsdb.com/transcripts/South-Park-"
all.text <- NULL
for(h in 1:length(listing.f)) {
     URL <- paste(URLstart, listing.f[h], ".html", sep="")
     script <- readLines(URL)
     end <- length(script) - 80
     script <- script[242:end]
     script <- gsub("\\<b>|\\</b>|<|\\/", "", script)
     script <- gsub("^\\s+|\\s+$", "", script)
     x <- script
     
     locations <- NULL
     blank.locations <- NULL
     
     for (j in seq_along(x)) {
          location <- j
          if(x[j]=="") blank.locations <- c(blank.locations, location)
          test <- gsub("\\.| ", "", x[j])
          if(grepl("^[[:upper:]]+$", test)) {
               if(nchar(x[j]) < 20) {
                    speaker <- x[j]
                    locations <- rbind(locations, c(speaker, location))
               }
          }
     }
     lines <- NULL
     for (line in seq_along(locations[,2])) {
          speaker <- locations[line,1]
          start <- as.numeric(locations[line,2])
          #find delimiting blank line
          cond <- sapply(blank.locations, function(x) as.numeric(x) > start)
          end <- blank.locations[cond]
          if(length(end)==0) { end1 <- max(length(x)) 
          } else { end <- end[1]; end1 <- end - 1 }
          start1 <- start + 1
          words <- x[start1:end1]
          if(length(words) > 1) words <- str_c(words, collapse=" ")
          row <- data.frame(speaker=speaker, words=words, episode=listing.f[h], num=h)
          lines <- rbind(lines, row)
     }
     
     all.text <- rbind(all.text, lines)
     
}

#write.csv(all.text, "southpark_all_scripts.csv", row.names=FALSE)
all.text$episode.order <- paste(all.text$num, all.text$episode, sep=" ")

### by speaker and by episode
by_episode <- NULL
episodes <- unique(all.text$episode.order)
for(episode in episodes) {
     subset <- all.text[all.text$episode.order==episode, ]
     speakers <- unique(subset$speaker)
     for(speaker in speakers) {
          subset2 <- subset[subset$speaker==speaker, ]
          text <- str_c(subset2$words, collapse=" ")
          row <- data.frame(episode, speaker, text)
          by_episode <- rbind(by_episode, row)
     }
}

#write.csv(by_episode, "southpark_byepisode_scripts.csv", row.names=FALSE)

# combine into speaker
by_person <- NULL
speakers <- unique(asp$speaker)
for(speaker in speakers){
    subset2 <- asp[asp$speaker==speaker, ]
    subset2 <- subset2[complete.cases(subset2),]
    text <- str_c(subset2$words, collapse=" ")
    text <- iconv(text, "ISO-8859-1", "UTF-8")
    row <- data.frame(speaker, text)
    by_person <- rbind(by_person, row)
}
# combine similar speaker names
mfix <- by_person[by_person$speaker %in% c("MR. MEPHESTO", "MEPHESTO"), ]
mfix <- str_c(mfix$text, collapse=" ")
by_person <- by_person[(by_person$speaker!="MEPHESTO"), ]
by_person <- by_person[(by_person$speaker!="MR. MEPHESTO"), ]
by_person <- rbind(by_person, data.frame(speaker="MEPHESTO", text=mfix))

mfix <- by_person[by_person$speaker %in% c("MS.CARTMAN", "LIANE"), ]
mfix <- str_c(mfix$text, collapse=" ")
by_person <- by_person[(by_person$speaker!="MS. CARTMAN"), ]
by_person <- by_person[(by_person$speaker!="LIANE"), ]
by_person <- rbind(by_person, data.frame(speaker="MS. CARTMAN", text=mfix))

mfix <- by_person[by_person$speaker %in% c("KYLE", "KYLE TWO"), ]
mfix <- str_c(mfix$text, collapse=" ")
by_person <- by_person[(by_person$speaker!="KYLE"), ]
by_person <- by_person[(by_person$speaker!="KYLE TWO"), ]
by_person <- rbind(by_person, data.frame(speaker="KYLE", text=mfix))

#write.csv(by_person, "southpark_byperson_scripts.csv", row.names=FALSE)




