library("plyr")
library("rvest")
hashratelink = "https://blockchain.info/charts/hash-rate?showDataPoints=false&timespan=all&show_header=true&daysAverageString=1&scale=1&format=csv&address="
hashratelink.json = "https://blockchain.info/charts/hash-rate?showDataPoints=false&timespan=all&show_header=true&daysAverageString=1&scale=1&format=json&address="
HASHRATE = read_html(hashratelink) %>%
  html_text()

write.csv(HASHRATE, file = "C:/Users/Adam/Downloads/HASHRATE.csv")
HASHRATE.df = read.csv("C:/Users/Adam/Downloads/HASHRATE.csv")
HASHRATE.df$date = HASHRATE.df[,1]
HASHRATE.df$Hashrate = HASHRATE.df[,2]
HASHRATE.df$X09.01.2009.18.15.05 = NULL
HASHRATE.df$X0.0006959437748148148 = NULL

library("stringr")
HASHRATE.df$date = word(HASHRATE.df$date, 1L)
library("lubridate")
HASHRATE.df$date = parse_date_time(HASHRATE.df$date, c('dmy'))
HASHRATE.df$date = as.Date(HASHRATE.df$date)

HASHRATE.df = HASHRATE.df[!(HASHRATE.df$date<"2013-01-01"),]
HASHRATE.df = HASHRATE.df[!(HASHRATE.df$date>"2016-04-23"),]
HASHRATE.df$Hashrate = HASHRATE.df$Hashrate/1000000





library(ggplot2)
hr.plot = ggplot(data = HASHRATE.df, aes(x = date, y = Hashrate))
hr.plot = hr.plot + geom_point() + labs(color = "Legend") + xlab("Year \n \n Source: http://blockchain.info") +
          ggtitle("Figure 1: Computing Power on The Bitcoin Network") + theme_minimal() + ylab("Hash Rate TH/s")
plot(hr.plot)

hr.plot = ggplot(data = HASHRATE.df, aes(x = date, y = Hashrate))
hr.plot = hr.plot + geom_jitter() + labs(color = "Legend") + ggtitle("Evolution of Bitcoin Hashrates") + theme_minimal()
plot(hr.plot)


## Difficulty
difflink = "https://blockchain.info/charts/difficulty?showDataPoints=false&timespan=all&show_header=true&daysAverageString=1&scale=0&format=csv&address="
DIFF = read_html(difflink) %>%
  html_text()
write.csv(DIFF, file = "C:/Users/Adam/Downloads/DIFF.csv")
DIFF.df = read.csv("C:/Users/Adam/Downloads/DIFF.csv")
DIFF.df$date = DIFF.df[,1]
DIFF.df$Difficulty = DIFF.df[,2]
DIFF.df$X04.01.2009.18.15.05 = NULL
DIFF.df$X0.0 = NULL

library("stringr")
DIFF.df$date = word(DIFF.df$date, 1L)
library("lubridate")
DIFF.df$date = parse_date_time(DIFF.df$date, c('dmy'))
DIFF.df$date = as.Date(DIFF.df$date)

DIFF.df = DIFF.df[!(DIFF.df$date<"2013-01-01"),]
DIFF.df = DIFF.df[!(DIFF.df$date>"2016-04-23"),]

DIFF.df$Difficulty = DIFF.df$Difficulty/1000000


## Transaction freq
FREQ.link = "https://blockchain.info/charts/n-transactions?showDataPoints=false&timespan=all&show_header=true&daysAverageString=1&scale=0&format=csv&address="
FREQ = read_html(FREQ.link) %>%
  html_text()
write.csv(FREQ, file = "C:/Users/Adam/Downloads/FREQ.csv")
FREQ.df = read.csv("C:/Users/Adam/Downloads/FREQ.csv")
FREQ.df$date = FREQ.df[,1]
FREQ.df$frequency = FREQ.df[,2]
FREQ.df$X04.01.2009.18.15.05 = NULL
FREQ.df$X0.0 = NULL

library("stringr")
FREQ.df$date = word(FREQ.df$date, 1L)
library("lubridate")
FREQ.df$date = parse_date_time(FREQ.df$date, c('dmy'))
FREQ.df$date = as.Date(FREQ.df$date)
FREQ.df = FREQ.df[!(FREQ.df$date<"2013-01-01"),]
FREQ.df = FREQ.df[!(FREQ.df$date>"2016-04-23"),]
FREQ.df$frequency = FREQ.df$frequency/1000000

## Trade volume
VOL.link = "https://blockchain.info/charts/trade-volume?showDataPoints=false&timespan=all&show_header=true&daysAverageString=1&scale=0&format=csv&address="
VOL = read_html(VOL.link)%>%
  html_text()
write.csv(VOL, file="C:/Users/Adam/Downloads/VOL.csv")
VOL.df = read.csv("C:/Users/Adam/Downloads/VOL.csv")
VOL.df$date = VOL.df[,1]
VOL.df$volume = VOL.df[,2]
VOL.df$X04.01.2009.18.15.05 = NULL
VOL.df$X0.0 = NULL

VOL.df$date = word(VOL.df$date, 1L)
VOL.df$date = parse_date_time(VOL.df$date, c('dmy'))
VOL.df$date = as.Date(VOL.df$date)
VOL.df = VOL.df[!(VOL.df$date<"2013-01-01"),]
VOL.df = VOL.df[!(VOL.df$date>"2016-04-23"),]
VOL.df$volume = VOL.df$volume/1000000


##############
## PLOTTING ##
##############


  # Hashrate plot
hr.plot = ggplot(data = HASHRATE.df, aes(x = date, y = Hashrate))
hr.plot = hr.plot + geom_line() + labs(color = "Legend") + xlab("Date \n \n Source: http://blockchain.info") +
  ggtitle("Figure 1: Computing Power on The Bitcoin Network") + theme_minimal() + ylab("Hash Rate TH/s")
plot(hr.plot)

  # Difficulty of hash plot
df.plot = ggplot(data = DIFF.df, aes(x = date, y = Difficulty))
df.plot = df.plot + geom_line(colour = "red") + xlab("Date \n \n Source: http://blockchain.info") +
  ggtitle("Figure 2: Difficulty of Cryptographic Hash ") + theme_minimal() + ylab("Difficulty")
plot(df.plot)

## Difficuly/Hashrate dataframe merge for division of vars + plot
HRDF.df = join_all(list(DIFF.df, HASHRATE.df), by = "date", type = "full")
HRDF.df$ratio = HRDF.df$Difficulty/HRDF.df$Hashrate

hrdf.plot = ggplot(data = HRDF.df, aes(x = date, y = ratio))
hrdf.plot = hrdf.plot + geom_line(colour = "black") + labs(color = "Legend") + xlab("Date \n \n Source: http://blockchain.info") +
  ggtitle("Figure A.1: Difficulty to Hash Rate ratio") + theme_minimal() + ylab("Difficulty/Hashrate(TH/s)")
plot(hrdf.plot)

  # Trade freq plot
frq.plot = ggplot(data = FREQ.df, aes(x = date, y = frequency))
frq.plot = frq.plot + geom_point(colour = "black") + xlab("Date \n \n Source: http://blockchain.info") +
            ylab("Daily Trades in Millions") + ggtitle("Figure 3: Trading Frequency on The Bitcoin Platform") + theme_minimal()
plot(frq.plot)

  # Trade volume plot
vol.plot = ggplot(data = VOL.df, aes(x = date, y = volume))
vol.plot = vol.plot + geom_point(colour = "red") + xlab("Date \n \n Source: http://blockchain.info") +
            ylab("Daily Trade Volume in Millions") + ggtitle("Figure 4: Trade Volume on The Bitcoin Platform") + theme_minimal()
plot(vol.plot)
