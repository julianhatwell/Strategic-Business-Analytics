store.df <- read.csv("http://goo.gl/QPDdMl")
store.df$storeNum <- factor(store.df$storeNum)

library(psych)
describe(store.df)

histAndSmooth <- function(ds
                          , mn="Histogram with smoother"
                          , xlb="") {
  
  hist(ds
       , main=mn
       , xlab=xlb
       , ylab="Proportion of total"
       , breaks=30
       , col="lightblue"
       , freq=FALSE
       , xaxt="n")
  axis(side=1, at=seq(floor(range(ds)[1]/10)*10, floor(range(ds)[2]/10)*10, by=20))
  lines(density(ds, bw=10) # "bw= ..." adjusts the smoothing
        , type="l", col="darkred", lwd=2)
}

histAndSmooth(store.df$p1sales, "Product 1 Weekly Sales")
histAndSmooth(store.df$p2sales, "Product 2 Weekly Sales")

boxplot(p2sales~storeNum, data=store.df
        , xlab="Weekly sales"
        , ylab="P2"
        , main="Weekly sales of P2
        by stores"
        , horizontal=TRUE)

boxplot(p2sales~p2prom, data=store.df
        , horizontal=TRUE
        , main="Weekly sales of P2 
        with and without promotion"
        , xlab="Weekly sales"
        , ylab="P2 promoted in store?"
        , yaxt="n")
axis(side=2, at=c(1,2)
     , labels=c("No", "Yes"))

qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

plot(ecdf(store.df$p1sales)
     , main="Cumulative distribution of 
     P1 Weekly Sales"
     , ylab="Cumulative Proportion"
     , xlab=c("P1 weekly sales, all stores"
              , "90% of weeks sold <= 171 units")
     , yaxt="n")
axis(side=2, at=seq(0, 1, by=0.1), las=1
     , labels=paste(seq(0,100,by=10)
                    , "%", sep=""))
abline(h=0.9, lty=3)
abline(v=quantile(store.df$p1sales, pr=0.9)
       , lty=3)

by(store.df$p1sales
   , store.df$storeNum, mean)
by(store.df$p1sales
   , list(store.df$storeNum, store.df$Year)
   , mean)

p1sales.sum <- aggregate(store.df$p1sales
          , by=list(country=store.df$country)
          , sum)

library(rworldmap)
library(RColorBrewer)

p1sales.map <- joinCountryData2Map(p1sales.sum
                                   , joinCode = "ISO2"
                                   , nameJoinColumn = "country")

mapCountryData(p1sales.map
               , nameColumnToPlot="x"
               , mapTitle="Total P1 sales 
               by Country"
               , colourPalette=brewer.pal(7, "Greens")
               , catMethod="fixedWidth"
               , addLegend=FALSE)

