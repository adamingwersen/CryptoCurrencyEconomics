install.packages("rvest")
library("rvest")
library("dplyr")
library("stringr")
library("XML")

wikilink = "http://stats.grok.se/en/201601/Bitcoin"
css.selector = "body > script"
css.selector2 = "body > form #year"

# Test structure of HTML

BTC_WIKI = read_html(wikilink) %>%
  html_nodes(css = css.selector) %>%
  html_text()

# Fetch string of dates

BTC_WIKI.dates = read_html(wikilink) %>%
  html_nodes(css = css.selector2) %>%
  html_text()

# Seperate string by delimiter into list

data.manipulated = as.data.frame(str_split(BTC_WIKI.dates, pattern = "\n"))
data.manipulated = data.manipulated$c........201603......201602......201601......201512......201511...
data.manipulated = as.character(data.manipulated)
data.manipulated = data.manipulated[-1]

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
data.manipulated = trim(data.manipulated)

# Link for which we need to feed the datestrings into, "LANK";
  ## Search term can be adjusted by replacing Bitcoin:

groks.link = "http://stats.grok.se/json/en/LANK/Bitcoin"

link_str_replace = function(data.manipulated){
  link.replacement = gsub("\\LANK", data.manipulated, groks.link)
}

list.of.links = ldply(data.manipulated, link_str_replace)
list.of.links = list.of.links$V1
list.of.links = list.of.links[1:100]

# Now we have a list of viable links. 
# This enables us to create a crawler for the wiki-site
  # The content on groks.se is formatted as a JSON table
  # This requires some extra work as the content is nested
  # Proceed as follows: Get raw content, parse using RJSONIO, ldply to dataframe
library("RJSONIO")


crawl_groks = function(list.of.links){
  searchinfo_wiki = fromJSON(list.of.links, simplifyMatrix = TRUE, flatten = TRUE)
    get.inf = ldply(searchinfo_wiki$daily_views, data.frame)
  return(cbind(get.inf))
}
wiki.get = lapply(list.of.links, crawl_groks)
wiki.get.df = ldply(wiki.get, data.frame)

wiki.get.df$date = as.Date(wiki.get.df$.id)
wiki.get.df$queries = as.numeric(wiki.get.df$X..i..)

wiki.get.df$X..i.. = NULL
wiki.get.df$.id = NULL
wiki.get.df = wiki.get.df[!(wiki.get.df$queries==0 & wiki.get.df$date>"2010-01-01"),]
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
