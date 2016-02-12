# scrape south park ratings from IMDb
library(RCurl)
library(XML)

# base URL http://www.omdbapi.com/?r=xml&i=tt0121955 where tt__ is the episode id

# first, need to get all the ids
# loop through season pages one at a time and grab the ids
ids <- NULL
for(j in 1:18) {
    startURL <- "http://www.imdb.com/title/tt0121955/episodes?season="
    URL <- paste(startURL, j, sep="")
    html.raw <- htmlTreeParse(getURL(URL), useInternal=TRUE)
    season.ids <- unlist(xpathApply(html.raw, "//div[@class='info']//strong//a", xmlGetAttr, "href"))
    season.ids <- strsplit(season.ids, "/")
    for(x in seq_along(season.ids)) season.ids[x] <- season.ids[[x]][3]
    ids <- c(ids, unlist(season.ids))
}

# loop through the ids to get info for each episode
ratings <- NULL
for(episode in ids) {
    startURL <- "http://www.omdbapi.com/?r=xml&i="
    URL <- paste(startURL, episode, sep="")
    html.raw <- htmlTreeParse(getURL(URL), useInternal=TRUE)
    rating <- unlist(xpathApply(html.raw, "//movie", xmlGetAttr, "imdbrating"))
    votes <- unlist(xpathApply(html.raw, "//movie", xmlGetAttr, "imdbvotes"))
    date <- unlist(xpathApply(html.raw, "//movie", xmlGetAttr, "released"))
    season <- unlist(xpathApply(html.raw, "//movie", xmlGetAttr, "season"))
    episode <- unlist(xpathApply(html.raw, "//movie", xmlGetAttr, "episode"))
    year <- unlist(xpathApply(html.raw, "//movie", xmlGetAttr, "year"))
    title <- unlist(xpathApply(html.raw, "//movie", xmlGetAttr, "title"))
    row <- data.frame(season, episode, year, date, title, rating, votes)
    ratings <- rbind(ratings, row)
}
ratings$votes <- gsub("\\,", "", ratings$votes)
for(u in c(1,2,3,6,7)) ratings[,u] <- as.numeric(as.character(ratings[,u]))
ratings$date <- as.Date(as.character(ratings$date), format="%d %b %Y")

#write.csv(ratings, "southpark_imdb_ratings.csv", row.names=FALSE)
