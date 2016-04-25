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
read.csv(DIFF)
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

df.plot = ggplot(data = DIFF.df, aes(x = date, y = Difficulty))
df.plot = df.plot + geom_line(colour = "red") + xlab("Date \n \n Source: http://blockchain.info") +
  ggtitle("Figure 2: Difficulty of Cryptographic Hash ") + theme_minimal() + ylab("Difficulty")
plot(df.plot)

hr.plot = ggplot(data = HASHRATE.df, aes(x = date, y = Hashrate))
hr.plot = hr.plot + geom_line() + labs(color = "Legend") + xlab("Year \n \n Source: http://blockchain.info") +
  ggtitle("Figure 1: Computing Power on The Bitcoin Network") + theme_minimal() + ylab("Hash Rate TH/s")
plot(hr.plot)

HRDF.df = join_all(list(DIFF.df, HASHRATE.df), by = "date", type = "full")
HRDF.df$ratio = HRDF.df$Difficulty/HRDF.df$Hashrate

hrdf.plot = ggplot(data = HRDF.df, aes(x = date, y = ratio))
hrdf.plot = hrdf.plot + geom_line(colour = "black") + labs(color = "Legend") + xlab("Year \n \n Source: http://blockchain.info") +
  ggtitle("Figure A.1: Difficulty to Hash Rate ratio") + theme_minimal() + ylab("Difficulty/Hashrate(TH/s)")
plot(hrdf.plot)









