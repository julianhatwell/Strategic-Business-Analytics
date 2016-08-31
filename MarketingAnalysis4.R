library(gpairs)
library(corrplot)
library(coefplot)
library(MCMCpack)

sat.df <- read.csv("http://goo.gl/HKnl74")
gpairs(sat.df)

# distance has high skew. common to transform with log
sat.df$logdist <- log(sat.df$distance)
hist(sat.df$logdist)
# exclude categorical num.child and raw distance
corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper="ellipse")

plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")

m1 <- lm(overall ~ rides, data=sat.df)
abline(m1, col="blue")

m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(m2)

coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5
         , sort = "magnitude"
         , ylab="Rating of Feature"
         , xlab="Association with Overall Satisfaction")

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared

# Actual vs fitted
plot(sat.df$overall, fitted(m1), col="red"
     , xlim=c(0,100), ylim=c(0,100)
     , xlab="Actual Overall Satisfaction"
     , ylab="Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col="blue")
legend("topleft", legend=c("model 1", "model 2")
       , col=c("red", "blue"), pch=1)

anova(m1, m2)

# scaling
sat.std <- sat.df[ , -3] # sat but remove distance
sat.std[ , 3:8] <- scale(sat.std[ , 3:8])
head(sat.std)

sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean +
          weekend + logdist + num.child.factor, data=sat.std)
summary(m4)

# because coeff is roughly same for any value of num.child other than zero
sat.std$has.child <- factor(sat.std$num.child > 0)
m5 <- lm(overall ~ rides + games + wait + clean + logdist + has.child
         , data=sat.std)
summary(m5)

#Is this still a good model? The change in R-squared between model m4 and
#m5 is negligible, suggesting that our simplification did not deteriorate the
#model fit.

m6 <- lm(overall ~ rides + games + wait + clean +
           weekend + logdist + has.child +
           rides:has.child + games:has.child + wait:has.child +
           clean:has.child + rides:weekend + games:weekend +
           wait:weekend + clean:weekend, data=sat.std)
summary(m6)

# very few of the above interaction terms have added anything
m7 <- lm(overall ~ rides + games + wait + clean + logdist + has.child +
           wait:has.child, data=sat.std)
summary(m7)

# is log dist really necessary
m8 <- lm(overall ~ rides + games + wait + clean + has.child +
           wait:has.child, data=sat.std)
summary(m8)

anova(m8,m7) # probably not

coefplot(m8, intercept=FALSE, outerCI=1.96, lwdOuter=1.5
         , sort = "magnitude"
         , ylab="Rating of Feature"
         , xlab="Association with Overall Satisfaction")

# linear model
m7.bayes <- MCMCregress(overall ~ rides + games + wait + clean + logdist +
                          has.child + wait:has.child, data=sat.std)
summary(m7.bayes)
