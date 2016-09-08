library(sqldf)
# --- PREPARING AND TRANSFORMING dt ----------------------
# Copy customer dt into new dt frame
new_dt_sub <- customers

# a good way to clear out the Ids while preserving them
row.names(new_dt_sub) <- new_dt_sub$customer_id
new_dt_sub$customer_id <- NULL
head(new_dt_sub)

# Take the log-transform of the amount, and plot
new_dt_sub$amount = log(new_dt_sub$amount)
hist(new_dt_sub$amount)

# Take the log-transform of the amount, and plot
new_dt_sub$frequency = log(new_dt_sub$frequency)
hist(new_dt_sub$frequency)

# Standardize variables
new_dt_sub = scale(new_dt_sub)
head(new_dt_sub)

# full data set
# d <- dist(new_dt_sub)

# Take a 10% sample (not random)
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_dt_sub_sample  = new_dt_sub[sample, ]

# Compute distance metrics on standardized sample
d = dist(new_dt_sub_sample)

# Perform hierarchical clustering on distance metrics
c <- hclust(d, method="ward.D2")

# Cut 
members4_sub <- cutree(c, k = 4)
members5_sub <- cutree(c, k = 5)
members9_sub <- cutree(c, k = 9)

table(members5_sub)

# Show profile of each segment using the non-scaled data
aggregate(customers_sample[, 2:4], by = list(members5_sub), mean)


compare <- c(which(customers_sample$customer_id == 260)
             , which(customers_sample$customer_id == 5920))
members5_sub[compare]
