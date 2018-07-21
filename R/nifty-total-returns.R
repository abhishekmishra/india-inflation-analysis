library(data.table)
library(ggplot2)
library(psych)
library(xts)

ntr <- fread('./data/nifty50-totalreturns-2000-1-2018-5.csv')
ntr$Date <- as.POSIXct(strptime(ntr$Date, '%d-%b-%Y'))

qplot(ntr$Date, ntr$TotalReturnsIndex) + geom_line()

summary(ntr)
hist(ntr$TotalReturnsIndex)
ggplot(ntr, aes(y=TotalReturnsIndex)) +
  geom_line()

nts <- xts(ntr$TotalReturnsIndex, order.by=ntr$Date, frequency=7)
qplot(nts)

plot(nts)

ntd <- to.period(nts,period="days")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
