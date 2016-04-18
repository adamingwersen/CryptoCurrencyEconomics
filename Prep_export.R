##Exporting 

 #ORDER Bitcoin_TS.df by ascending ##
library(plyr)
wiki.get.df = arrange(wiki.get.df, desc(date))
WIKIDATE.list = wiki.get.df$date

Bitcoin_TS.df = arrange(Bitcoin_TS.df, desc(Datespan))
Bitcoin_TS.df = rename(Bitcoin_TS.df,c("Datespan"="date"))
GTRENDDATE.list = Bitcoin_TS.df$date

coindesk_bpi.df = read.csv("C:/Users/Adam/Downloads/coindesk.bpi.csv")
coindesk_bpi.df$Date = as.Date(coindesk_bpi.df$Date)
coindesk_bpi.df= head(coindesk_bpi.df, -2)
coindesk_bpi.df = rename(coindesk_bpi.df,c("Date"="date"))
coindesk_bpi.df = arrange(coindesk_bpi.df, desc(date))
BTCPRICEDATE.list = coindesk_bpi.df$date

 # These files are exported as .csv's
 # Then aggregated by week in OxMetrics

write.csv(wiki.get.df, "C:/Users/Adam/Downloads/WIKIQUERY.csv")
write.csv(Bitcoin_TS.df, "C:/Users/Adam/Downloads/GTRENDS.csv")
write.csv(coindesk_bpi.df, "C:/Users/Adam/Downloads/BTCPRICE.csv")

 #Imported back into R
WIKIQUERY.df = read.csv("C:/Users/Adam/Downloads/WIKIQUERY.csv")
GTRENDS.df = read.csv("C:/Users/Adam/Downloads/GTRENDS.csv")
BTCPRICE.csv = read.csv("C:/Users/Adam/Downloads/BTCPRICE.csv")

BITCOIN.data = join_all(list(WIKIQUERY.df, GTRENDS.df, BTCPRICE.csv), by = "date", type = "full")
BITCOIN.data = arrange(BITCOIN.data, desc(date))
write.csv(BITCOIN.data, "C:/Users/Adam/Downloads/BITCOINDATA.csv")
