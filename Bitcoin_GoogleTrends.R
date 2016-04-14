# Load required libraries
library(RCurl)		# For getURL() and curl handler / cookie / google login
library(stringr)	# For str_trim() to trip whitespace from strings
library(dplyr)    # Loop

# Google account settings
username <- "***"
password <- "***"

# URLs
loginURL 		<- "https://accounts.google.com/accounts/ServiceLogin"
authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
trendsURL 		<- "http://www.google.com/trends/TrendsRepport?"

btc = "Bitcoin"                     # Insert search-term
out <- "C:/Users/Adam/Downloads"    # Preferred local output directory



############################################
## This gets the GALX cookie which we need to pass back with the login form
############################################
getGALX <- function(curl) {
  txt = basicTextGatherer()
  curlPerform( url=loginURL, curl=curl, writefunction=txt$update, header=TRUE, ssl.verifypeer=FALSE )
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  strsplit(val, "[:=;]")[[1]][3]
  
  return( strsplit( val, "[:=;]")[[1]][3]) 
}


############################################
## Function to perform Google login and get cookies ready
############################################
gLogin <- function(username, password) {
  ch <- getCurlHandle()
  
  ans <- (curlSetOpt(curl = ch,
                     ssl.verifypeer = FALSE,
                     useragent = getOption('HTTPUserAgent', "R"),
                     timeout = 60,         
                     followlocation = TRUE,
                     cookiejar = "./cookies",
                     cookiefile = ""))
  
  galx <- getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=username, Passwd=password, GALX=galx, PersistentCookie="yes", continue="http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  return(ch)
}

############################################
## Read data for a query
############################################
ch <- gLogin( username, password)
authenticatePage2 <- getURL("http://www.google.com", curl=ch)
res <- getForm(trendsURL, q="Bitcoin", content=1, export=1, graph="all_csv", curl=ch)
res
# Check if quota limit reached
if( grepl( "You have reached your quota limit", res ) ) {
  stop( "Quota limit reached; You should wait a while and try again later" )
}

# Execute Google Trends Setup w.o. Date-specc
Google_Static_Fetch = function(x){
  ch = gLogin(username, password)
  authenticatePage2 = getURL("http://www.google.com", curl = ch)
  for(i in x){
    res <- getForm(trendsURL, q=x, content=1, export=1, graph="all_csv", curl=ch)
    myfile <- file.path(out, paste0("GTrends", "_", i, ".csv"))
    write.table(res, file = myfile, sep = ";", row.names = FALSE, col.names = FALSE,
                quote = FALSE, append = FALSE)
              }
            }
if( grepl( "You have reached your quota limit", res) ) {
  stop( "Quota limit reached; You should wait a while and try again later" )
  }

#################################################################################
################ Utilize above defined functions to search Google ###############


## BITCOIN ##
#############
Google_Static_Fetch(btc)
Bitcoin.df = read.csv("C:/Users/Adam/Downloads/GTrends_Bitcoin.csv", header = FALSE, stringsAsFactors = FALSE, sep ="," )
# Clean data, split into multiple datasets - timeseries & Geography

Bitcoin_TS.df = Bitcoin.df[grep("^20", Bitcoin.df[,1]),]
# Dropping columns, V3, V4:
Bitcoin_TS.df[c("V3","V4")] <- list(NULL)
# Renaming variables V1/V2:
library("plyr")
Bitcoin_TS.df = rename(Bitcoin_TS.df, c("V1"="Datespan", "V2"="Index"))
#Transform vars
library("stringr")
Bitcoin_TS.df$Index = as.numeric(Bitcoin_TS.df$Index)
Bitcoin_TS.df$Datespan = word(Bitcoin_TS.df$Datespan, start = 3L)
Bitcoin_TS.df$Datespan = as.Date(Bitcoin_TS.df$Datespan)
Bitcoin_TS.df <- subset(Bitcoin_TS.df, Datespan > as.Date("2010-07-18") )

####################################################################################
################ Various other searches and visualization ##########################

## BLOCKCHAIN ##
#################
Google_Static_Fetch("Blockchain")
Blockchain.df = read.csv("C:/Users/Adam/Downloads/GTrends_Blockchain.csv", header = FALSE, stringsAsFactors = FALSE, sep = ",")
Blockchain_TS.df = Blockchain.df[grep("^20", Blockchain.df$V1),]
# Dropping columns, V3, V4:
Blockchain_TS.df[c("V3","V4")] <- list(NULL)
# Renaming variables V1/V2:
library("plyr")
Blockchain_TS.df = rename(Blockchain_TS.df, c("V1"="Datespan", "V2"="Index"))
#Transform vars
Blockchain_TS.df$Index = as.integer(Blockchain_TS.df$Index)
Blockchain_TS.df$Datespan = as.Date(Blockchain_TS.df$Datespan)
Blockchain_TS.df <- subset(Blockchain_TS.df, Datespan > as.Date("2010-01-01") )

## LITECOIN ##
##############
Google_Static_Fetch("Litecoin")
Litecoin.df = read.csv("C:/Users/Adam/Downloads/GTrends_Litecoin.csv", header = FALSE, stringsAsFactors = FALSE, sep = ",")
Litecoin_TS.df = Litecoin.df[grep("^20", Litecoin.df$V1),]
# Dropping columns, V3, V4:
Litecoin_TS.df[c("V3","V4")] <- list(NULL)
# Renaming variables V1/V2:
library("plyr")
Litecoin_TS.df = rename(Litecoin_TS.df, c("V1"="Datespan", "V2"="Index"))
#Transform vars
Litecoin_TS.df$Index = as.integer(Litecoin_TS.df$Index)
Litecoin_TS.df$Datespan = as.Date(Litecoin_TS.df$Datespan)
Litecoin_TS.df <- subset(Litecoin_TS.df, Datespan > as.Date("2010-10-08") )

## MERGE ALL ##
###############

Crypto.df = left_join(Bitcoin_TS.df, Blockchain_TS.df, by = "Datespan")
Crypto.df = left_join(Crypto.df, Litecoin_TS.df, by = "Datespan")
Crypto.df = rename(Crypto.df, c("Index.x"="Bitcoin", "Index.y" = "Blockchain", "Index" = "Litecoin"))

## DATAVIS ##
#############

#Introductory plotting of timeseries
library("reshape2")
Melt.df <- melt(Crypto.df, id.vars="", value.name="value", variable.name="Year")
library("ggplot2")
p = ggplot(Bitcoin_TS.df, aes(x = Datespan, y = Index))
p = p + geom_line(data = Bitcoin_TS.df, aes(color = "Bitcoin")) + 
        geom_line(data = Blockchain_TS.df, aes(color = "Blockchain")) + 
        geom_line(data = Litecoin_TS.df, aes(color = "Litecoin"))
p = p + labs(color="Legend") + scale_color_manual("", breaks = c("Bitcoin", "Blockchain", "Litecoin"),
                                                      values = c("#ff4500", "#d19fe8", "#006a4e"))
p = p + ggtitle("Google Trends: Cryptocurrency")
p = p + theme_minimal()
plot(p)



# This function can be instantiated if one wishes to specify the date-interval
# Allows for daily trend-statistics - will output as seperate files to the output directory
# These files has to imported and seperated, then merged. Script can be found in SDS_Project

Google_Trends_Multi = function(x, y){
  ch <- gLogin( username, password )
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  for(i in x) {
    for(j in y){
      res <- getForm(trendsURL, q=x, date = y, content=1, export=1, graph="all_csv", curl=ch)
      myfile <- file.path(out, paste0("GTrends", "_", i, ".csv"))
      write.table(res, file = myfile, sep = ";", row.names = FALSE, col.names = FALSE,
                  quote = FALSE, append = FALSE)
    }
  }
  if( grepl( "You have reached your quota limit", res) ) {
    stop( "Quota limit reached; You should wait a while and try again later" )
  }
}