library(data.table)
library(ggplot2)
library(ggthemes)

# historical annual inflation rates.
# http://www.inflation.eu/inflation-rates/india/historic-inflation/cpi-inflation-india.aspx

# historical inflation rates (countries and world)
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=IN
world_cpi_flat <- fread('./data/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_9944890.csv', header=TRUE)
world_cpi <- melt(world_cpi_flat, id.vars=c('Country Name', 'Country Code'), measure.vars=5:62)
setnames(world_cpi, 3:4, c('year', 'cpi'))
world_cpi$cpi <- as.numeric(world_cpi$cpi)
india_hist_cpi <- world_cpi[`Country Name` == "India"]

# Source http://www.inflation.eu/inflation-rates/india/historic-inflation/cpi-inflation-india.aspx
india_hist_cpi[year == "2017"]$cpi <- 2.49

ggplot(india_hist_cpi, aes(x=year, y=cpi, group=1, label=cpi)) +
#  geom_point() +
#  geom_line() +
  geom_col(fill="#a9a9a9") + #can use #a9a9a9 or #696969
  geom_text(aes(y = cpi + 2 * sign(cpi), label = sprintf("%0.1f", cpi)), 
            position = position_dodge(width = 0.9), 
            size = 3.5 , angle = 90) +
  geom_text(aes(y = 0, label = year), col="#000080",
            size = 4 , angle = 90, nudge_y = -2) +
  annotate(geom = 'text', 
           label = 'Source~"1960-2016"~":"~"data.worldbank.org,"~"2017:"~inflation.eu',
           x = Inf, y = -Inf, hjust = 1, vjust = -0.5, parse=TRUE) +
  annotate(geom = 'text',
           label = '© 2018 Abhishek Mishra', color = "#a9a9a9",
           x = 30, y = -Inf, hjust = 0.5, vjust = -0.5, parse=FALSE) + 
  scale_y_continuous(breaks=c(-5, 0, 5, 10, 15, 20, 25)) +
#  scale_y_continuous(expand = c(0,0),
#                     limits = c(0,30)) +
#  theme_solarized() +
#  theme_economist_white() +
  theme_classic() +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
#        axis.text.x = element_text(face="bold", color="#000000", 
#                                   size=10, angle=45,hjust=1,vjust=0.8),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=10)) +
  xlab('') +
  ylab('Consumer Price Inflation (CPI) in %') +
  ggtitle(label = 'India - Yearly Consumer Price Inflation (CPI) in % [ 1960 to 2017 ]')

