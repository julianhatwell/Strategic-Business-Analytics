rm(list = ls())
library(sqldf)
# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------
# Load text file into local variable called 'dt'
dt = read.delim(file = 'module2\\purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(dt) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
dt$date_of_purchase <- as.Date(dt$date_of_purchase, "%Y-%m-%d")
maxDate <- as.character(max(dt$date_of_purchase) + 1)

dt$days_since <- as.numeric(difftime(time1 = maxDate,
                                      time2 = dt$date_of_purchase,
                                      units = "days"))

# Display the dt after transformation
head(dt)
summary(dt)

# Compute key marketing indicators using SQL language
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers <- sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'amount'
                   FROM dt GROUP BY customer_id")

# Explore the dt
head(customers)
summary(customers)
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)

# --- PREPARING AND TRANSFORMING dt ----------------------
# Copy customer dt into new dt frame
new_dt <- customers

# Remove customer id as a variable, store it as row names
head(new_dt)
# a good way to clear out the Ids while preserving them
row.names(new_dt) <- new_dt$customer_id
new_dt$customer_id <- NULL
head(new_dt)

# Take the log-transform of the amount, and plot
new_dt$amount = log(new_dt$amount)
hist(new_dt$amount)

# Standardize variables
new_dt = scale(new_dt)
head(new_dt)
# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------
# Compute distance metrics on standardized dt
# This will likely generate an error on most machines
d <- dist(new_dt)

# Take a 10% sample
# sample = seq(1, 18417, by = 10)
# head(sample)
# customers_sample = customers[sample, ]
# new_dt_sample  = new_dt[sample, ]

# Compute distance metrics on standardized dt
# d = dist(new_dt_sample)

# Perform hierarchical clustering on distance metrics
c <- hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)

# Cut 
members4 <- cutree(c, k = 4)
members9 <- cutree(c, k = 9)
plot(members4)
plot(members9)


# Show 30 first customers, frequency table
members9[1:30]
table(members9)

# Show profile of each segment using the non-scaled data
aggregate(customers[, 2:4], by = list(members9), mean)

