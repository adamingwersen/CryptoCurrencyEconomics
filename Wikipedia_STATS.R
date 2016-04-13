library("rvest")
library("dplyr")
library("stringr")
library("XML")

wikilink = "http://stats.grok.se/en/201601/Bitcoin"
css.selector = "body > form #year"

## Fetch string of dates ##
 # The HTML-structure of "stats.grok.se" does not allow for a view of the entire period of interest.
 # Furthermore, it does not contain embedded link for redirection - thus something else must be done..
 # Investigating; a date-string variable is contained in a css.selector element, which can be utilized for -
 # -creating the relevant links needed for a crawler.

BTC_WIKI.dates = read_html(wikilink) %>%
  html_nodes(css = css.selector) %>%
  html_text()

## Construct usable format of the imported data ##
 # Dates are contained as one long string, "\n 201604\n 201603..."
 # String manipulation is needed - a list containing seperate dates in correct format
 #.. without whitespace is the aim of this maneuver.

data.manipulated = as.data.frame(str_split(BTC_WIKI.dates, pattern = "\n"))
data.manipulated = data.manipulated[,1]
data.manipulated = as.character(data.manipulated)
data.manipulated = data.manipulated[-1]

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
data.manipulated = trim(data.manipulated)

## Now, create the links needed for the crawler ##
 # Call base-link to be replicated with varying dates, "LANK"
 # Search term can be adjusted by replacing /Bitcoin 
 # link_str_replace simply inserts and element(date) into the link

groks.link = "http://stats.grok.se/json/en/LANK/Bitcoin"

link_str_replace = function(x){
  link.replacement = gsub("\\LANK", x, groks.link)
}

## Loop through list of dates and create correspodning links using HW's "plyr"  package##
library("plyr")

links = ldply(data.manipulated, link_str_replace)
links.list = links$V1
 # The last element is not relevant - exclude.
links.list = links.list[1:length(links.list)-1]
 # Now we have a list of viable links. 

## Crawler for the wiki-site ##
 # The content on groks.se is formatted as a JSON table
 # This requires some extra work as the content is nested
 # Proceed as follows: Get raw content, parse using RJSONIO, ldply to dataframe
library("RJSONIO")

crawl_groks = function(x){
  searchinfo_wiki = fromJSON(x, simplifyMatrix = TRUE, flatten = TRUE)
    get.inf = ldply(searchinfo_wiki$daily_views, data.frame)
  return(cbind(get.inf))
}
wiki.get = lapply(links.list, crawl_groks)

 # Inspecting wiki.get: Nested list of data.frames
 # This can be combatted by ldply to data.frame
wiki.get.df = ldply(wiki.get, data.frame)

## Cleaning data ##
wiki.get.df$date = as.Date(wiki.get.df$.id)                                             # Create new variables 
wiki.get.df$queries = as.numeric(wiki.get.df$X..i..)

wiki.get.df$X..i.. = NULL                                                               # Deleting old variables
wiki.get.df$.id = NULL
wiki.get.df = wiki.get.df[!(wiki.get.df$queries==0 & wiki.get.df$date>"2010-01-01"),]   # Align
wiki.get.df = na.omit(wiki.get.df)

##################################################### VISUALIZATIONS ###########################################################
################################################################################################################################

## INTRO GGPLOT ##
##################
library(ggplot2)
p = ggplot(data = wiki.get.df2, aes(x=date, y=log(queries)))
p = p + geom_line(na.omit = TRUE) + geom_line(data = btccsv.df2, aes(y = log(Close.Price)))
p = p + theme_minimal() + ggtitle("Bitcoin\nLogarithmic transformation of Daily Wikipedia Search Queries")
plot(p)



## DUAL AXIS PLOT ##
####################

btccsv.df = read.csv("C:/Users/Adam/Downloads/coindesk-bpi-USD-close_data-2010-07-18_2016-03-18.csv")
btccsv.df$date = as.Date(btccsv.df$Date)
dates.price = btccsv.df$date

wikidates = dates.wiki.btc[dates.wiki.btc %in% dates.price]

datespan = Bitcoin_TS.df$Datespan
dates.wiki.btc = wiki.get.df$date

wikidates = dates.wiki.btc[dates.wiki.btc %in% datespan]

btccsv.df2 = btccsv.df[btccsv.df$date %in% as.Date(wikidates),]
Bitcoin_TS.df2 = Bitcoin_TS.df[Bitcoin_TS.df$Datespan %in% as.Date(wikidates),]
wiki.get.df2 = wiki.get.df[wiki.get.df$date %in% as.Date(wikidates),]
Bitcoin_TS.df2$date = Bitcoin_TS.df2$Datespan

pricebtc = as.numeric(btccsv.df2$Close.Price)
BTCPrice.wikidates = as.numeric(Bitcoin_TS.df2$Index)
WikiQuery.wikidates = as.numeric(wiki.get.df2$queries)

par(mar=c(5,4,4,5)+.1)
plot(wikidates,log(pricebtc),type="l",col="red")
par(new=TRUE)
plot(wikidates, log(WikiQuery.wikidates),,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("log(Google.Trends)",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("$-Price","Search"))
title("Log of BitCoins dollar-value & Search queries")

## ORDER Bitcoin_TS.df by ascending ##
library(plyr)
btccsv.df2 = arrange(btccsv.df2, desc(date))

## Create plotvars ##
#####################
library("dplyr")
joined2 = left_join(btccsv.df2, wiki.get.df2, by = "date")
write.csv(joined2, "C:/Users/Adam/Downloads/joinedx.csv")
