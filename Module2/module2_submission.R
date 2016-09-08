library(sqldf)
library(lattice)
source("C:\\Dev\\Study\\R\\R_Themes\\MarketingTheme.R")

# run module1.R
customers_2015 <- sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM dt2 GROUP BY customer_id")

# --- CODING A MANAGERIAL SEGMENTATION ---------------------
# Complete segment solution using which, and exploiting previous test as input
customers_2015$segment <- "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] <- "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] <- "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] <- "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] <- "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] <- "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] <- "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] <- "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365 & customers_2015$amount < 100)] <- "new active low"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365 & customers_2015$amount >= 100)] <- "new active high"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] <- "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] <- "active high value"

table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# Re-order factor in a way that makes sense
customers_2015$segment <- factor(
  x = customers_2015$segment
  , levels = c("inactive"
               , "cold"
               , "warm high value"
               , "warm low value"
               , "new warm"
               , "active high value"
               , "active low value"
               , "new active high"
               , "new active low")
  )

table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# --- SEGMENTING A dt2BASE RETROSPECTIVELY ----------------
# Compute recency, frequency, and average purchase amount
# simulate how the data looked a year before
customers_2014 <- sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                               MAX(days_since) - 365 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM dt
                        WHERE days_since > 365
                        GROUP BY customer_id")

# Complete segment solution using which, and exploiting previous test as input
customers_2014$segment <- "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] <- "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] <- "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] <- "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] <- "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] <- "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] <- "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] <- "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365 & customers_2014$amount < 100)] <- "new active low"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365 & customers_2014$amount >= 100)] <- "new active high"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] <- "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] <- "active high value"

# Re-order factor in a way that makes sense
customers_2014$segment <- factor(
  x = customers_2014$segment
  , levels = c("inactive"
               , "cold"
               , "warm high value"
               , "warm low value"
               , "new warm"
               , "active high value"
               , "active low value"
               , "new active high"
               , "new active low")
  )

# Show segmentation results
table(customers_2014$segment)
pie(table(customers_2014$segment), col = rainbow(24))
aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), mean)


# --- COMPUTING REVENUE GENERATION PER SEGMENT -------------
dt2 <- dt
dt2$date_of_purchase <- as.Date(dt2$date_of_purchase, "%Y-%m-%d")
dt2$year_of_purchase <- as.numeric(format(dt2$date_of_purchase, "%Y"))

maxDate <- as.character(max(dt2$date_of_purchase) + 1)
dt2$days_since <- as.numeric(difftime(time1 = maxDate,
                                      time2 = dt2$date_of_purchase,
                                      units = "days"))

# Compute how much revenue is generated by segments
# Notice that people with no revenue in 2015 do NOT appear
revenue_2015 <- sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                      FROM dt2
                      WHERE year_of_purchase = 2015
                      GROUP BY customer_id")
summary(revenue_2015)

# Merge 2015 customers and 2015 revenue (correct)
actual <- merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0

# Show average revenue per customer and per segment
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)

# Merge 2014 customers and 2015 revenue (correct)
forward <- merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] = 0

# Show average revenue per customer and per segment
r <- aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), mean)
# Show average purchase per customer and per segment
p <- aggregate(x = forward$amount, by = list(customers_2014$segment), mean)

# Re-order and display results
r <- r[order(r$x, decreasing = TRUE), ]
p <- p[order(p$x, decreasing = TRUE), ]

barplot(r$x, names.arg = r$Group.1)

newActLow <- nrow(customers[customers_2015$segment == "new active" &
                 customers_2015$amount < 100,])

newActHigh <- nrow(customers[customers_2015$segment == "new active" &
                 customers_2015$amount >= 100,])

newActHighLY <- nrow(customers[customers_2014$segment == "new active" &
                 customers_2014$amount >= 100,])

(newActHigh/newActHighLY - 1) * 100

r
p
