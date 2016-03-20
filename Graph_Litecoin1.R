ltccsv.df = read.csv("C:/Users/Adam/Downloads/coindesk-ltcdailytransactions.csv")
ltccsv.df$date = as.Date(ltccsv.df$date)

## Align dates ##
#################

datespan = Litecoin_TS.df$Datespan
dates.ltc = ltccsv.df$date

ltcdates = dates.ltc[dates.ltc %in% datespan]

ltccsv.df2 = ltccsv.df[ltccsv.df$date %in% as.Date(ltcdates),]
ltcts.df = Litecoin_TS.df[Litecoin_TS.df$Datespan %in% as.Date(ltcdates),]
ltcts.df$date = ltcts.df$Datespan

## Create plotvars ##
#####################

LTC.vol = ltccsv.df2$value
LTC.index = ltcts.df$Index
LTC.vol = as.numeric(LTC.vol)
LTC.index = as.numeric(LTC.index)

## Vis: Litecoin ##
###################
  # No pkgs needed

# Absolute values
par(mar=c(5,4,4,5)+.1)
plot(x = ltcdates, y = LTC.vol ,type="l",col="red")
par(new=TRUE)
plot(ltcdates, LTC.index,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Google.Trends",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Trade Volume","Search"))
title("Litecoin trade volume & Search queries")

# Log-transformed
par(mar=c(5,4,4,5)+.1)
plot(x = ltcdates, y = log(LTC.vol) ,type="l",col="red")
par(new=TRUE)
plot(ltcdates, log(LTC.index),type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("log(Google.Trends)",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("log of Trade Volume","log of Search"))
title("Log of Litecoin trade volume & Search queries")

# log(trends)
par(mar=c(5,4,4,5)+.1)
plot(x = ltcdates, y = LTC.vol ,type="l",col="red")
par(new=TRUE)
plot(ltcdates, log(LTC.index),type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("log(Google.Trends)",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Trade Volume","Search"))
title("Litecoin trade volume & log of Search queries")



