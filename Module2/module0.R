library(sqldf)
library(lattice)
source("C:\\Dev\\Study\\R\\R_Themes\\MarketingTheme.R")
# Practise using sqldf

myTrendKey <- list(columns = 2
  , text = list(
  c("actuals", "smooth trend")
  , col = c(MyLatticeTheme$plot.line$col
            , myPal[1])
  , cex = 0.8)
  , lines = list(
    col = c(MyLatticeTheme$plot.line$col
              , myPal[1])
    , lty = 3
    , lwd = MyLatticeTheme$plot.line$lwd)
)

myTrendPlot <- function(x, y, d) {
  fmla <- as.formula(paste0(y, "~", x))
  xyplot(fmla, data = d, type = "l"
         , main = paste("Trend lines:", y, "by", x)
         , panel = function(x, y, ...) {
           panel.loess(x, y
                       , col = myPal[1]
                       , span = 0.8
                       , lty = 3
                       , ...)
           panel.xyplot(x, y, ...)
         }
         , par.settings = MyLatticeTheme
         , scales = MyLatticeScale
         , key = myTrendKey
  )
}

dat <- read.delim(file = 'Module2\\purchases.txt', header = FALSE, sep = '\t', dec = '.')
head(dat)
summary(dat)

# Add headers and interpret the last column as a date, extract year of purchase
colnames(dat) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
dat$date_of_purchase <- as.Date(dat$date_of_purchase, "%Y-%m-%d")
dat$year_of_purchase <- as.numeric(format(dat$date_of_purchase, "%Y"))

# Display the dat set after transformation
head(dat)
summary(dat)

# All in one
sq <- sqldf("SELECT year_of_purchase,
           COUNT(year_of_purchase) AS 'counter',
           AVG(purchase_amount) AS 'avg_amount',
           SUM(purchase_amount) AS 'sum_amount'
           FROM dat GROUP BY 1 ORDER BY 1")

myTrendPlot(x = "year_of_purchase", y = "counter", d = sq)
myTrendPlot(x = "year_of_purchase", y = "avg_amount", d = sq)
myTrendPlot(x = "year_of_purchase", y = "sum_amount", d = sq)
