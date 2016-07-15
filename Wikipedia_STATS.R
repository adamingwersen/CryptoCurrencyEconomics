library("rvest")
library("dplyr")
library("stringr")
library("XML")

wikilink = "http://stats.grok.se/en/201601/Bitcoin"
css.selector = "body > form > #year"

## Fetch string of dates ##
 # The HTML-structure of "stats.grok.se" does not allow for a view of the entire period of interest.
 # Furthermore, it does not contain embedded link for redirection - thus something else must be done..
 # Investigating; a date-string variable is contained in a css.selector element, which can be utilized for -
 # -creating the relevant links needed for a crawler.

BTC_WIKI.dates = read_html(wikilink) %>%
  html_nodes(css = css.selector) %>%
  html_text(trim = FALSE)

BTC_WIKI.dates = sapply(seq(from=1, to=nchar(BTC_WIKI.dates), by=6), function(i) substr(BTC_WIKI.dates, i, i+5))

## Construct usable format of the imported data ##
 # Dates are contained as one long string, "\n 201604\n 201603..."
 # String manipulation is needed - a list containing seperate dates in correct format
 #.. without whitespace is the aim of this maneuver.

data.manipulated = as.data.frame(sapply(seq(from=1, to=nchar(BTC_WIKI.dates), by=6), function(i) substr(BTC_WIKI.dates, i, i+5)))
names(data.manipulated)[1] = "dates"
data.manipulated$dates = as.character(data.manipulated$dates)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Now, create the links needed for the crawler ##
 # Call base-link to be replicated with varying dates, "LANK"
 # Search term can be adjusted by replacing /Bitcoin 
 # link_str_replace simply inserts and element(date) into the link

## What do you want to search for on wiki?
searchterm = "Bitcoin"

groks.base = "http://stats.grok.se/json/en/LANK/"
groks.link = paste0(groks.base, searchterm)


link_str_replace = function(x, y){
  link.replacement = gsub("\\LANK", x, groks.link)
}

## Loop through list of dates and create correspodning links using HW's "plyr"  package##
library("plyr")

links = llply(data.manipulated$dates, link_str_replace)
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
wiki.jsonget = lapply(links, crawl_groks)

 # Inspecting wiki.get: Nested list of data.frames
 # This can be combatted by ldply to data.frame
wiki.get.df = ldply(wiki.jsonget, data.frame)

## Cleaning data ##
wiki.get.df$date = as.Date(wiki.get.df[,1])                                             # Create new variables 
wiki.get.df$queries = as.numeric(wiki.get.df[,2])

wiki.get.df$X..i.. = NULL                                                               # Deleting old variables
wiki.get.df$.id = NULL
wiki.get.df = wiki.get.df[!(wiki.get.df$queries==0 & wiki.get.df$date>"2010-01-01"),]   # Align
wiki.get.df = na.omit(wiki.get.df)

##################################################### VISUALIZATIONS ###########################################################
################################################################################################################################

## INTRO GGPLOT ##
##################
library(ggplot2)
p = ggplot(data = wiki.get.df, aes(x=wikidates, y=log(queries)))
p = p + geom_line(data = wiki.get.df, aes(y = log(queries)))
p = p + theme_minimal() + ggtitle("Bitcoin\nLogarithmic transformation of Daily Wikipedia Search Queries")
plot(p)


## DUAL AXIS PLOT ##
####################

# Download as CSV:http:   //www.coindesk.com/price/
# Set Period : All
csvdir = "/home/adam/Documents/Data/BSCThesis/coindesk-bpi-USD-close_data-2010-07-18_2016-07-15.csv"

btccsv.df = read.csv(csvdir)
btccsv.df$date = as.Date(btccsv.df$Date)
dates.price = btccsv.df$date
dates.wiki = wiki.get.df$date

wikidates = dates.wiki[dates.wiki %in% dates.price]


btccsv.df = btccsv.df[btccsv.df$date %in% as.Date(wikidates),]
wiki.get.df2 = wiki.get.df[wiki.get.df$date %in% as.Date(wikidates),]

pricebtc = as.numeric(btccsv.df$Close.Price)
WikiQuery.wikidates = as.numeric(wiki.get.df2$queries)

par(mar=c(5,4,4,5)+.1)
plot(wikidates,log(pricebtc),type="l",col="red")
par(new=TRUE)
plot(wikidates, log(WikiQuery.wikidates),,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("log(Google.Trends)",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("$-Price","Search"))
title("Log of BitCoins dollar-value & Search queries")


