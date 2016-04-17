##Exporting 

 #ORDER Bitcoin_TS.df by ascending ##
library(plyr)
wiki.get.df = arrange(wiki.get.df, desc(date))
Bitcoin_TS.df = arrange(Bitcoin_TS.df, desc(Datespan))
Bitcoin_TS.df = rename(Bitcoin_TS.df,c("Datespan"="date"))


## Create plotvars ##
#####################
library("dplyr")
joined2 = left_join(btccsv.df2, wiki.get.df2, by = "date")
write.csv(joined2, "C:/Users/Adam/Downloads/joinedx.csv")