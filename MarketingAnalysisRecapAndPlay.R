library(car)
library(psych)
library(gpairs)
library(corrgram)
library(lattice)
library(binom)
library(BayesFactor)
library(multcomp)
library(ggplot2)
library(corrplot)
library(gplots)
library(RColorBrewer)
library(coefplot)
library(MCMCpack)
library(nFactors)
library(GPArotation)
library(semPlot)
library(cluster)
library(forecast)
library(lme4)
library(gam)
source("C:\\Dev\\Study\\R\\Utilities\\Utilities.R")
# data simul
set.seed(2016)
n <- 1000

noise0 <- rnorm(n, mean = 0, sd = 1)
noise1 <- rnorm(n, mean = 1, sd = 0.5)
noise2 <- rnorm(n, mean = 0, sd = 0.75)
noise3 <- rnorm(n, mean = 0.1, sd = 0.1)
noise4 <- rnorm(n, mean = 0.05, sd = 0.05)
noise5 <- rnorm(n, mean = 1, sd = seq(0.05, 0.5, length.out = n))

x0 <- ifelse(rbinom(n, size = 1, prob = 0.2) == 1
             , 2, 0)
binom.test(sum(x0)/2, n, p = 0.5)
binom.confint(sum(x0)/2, n, method="ac")

x1 <- rexp(n, 5)

# getting a box cox parameter
lambda <- coef(powerTransform(x1)) # car package
lambda.alt <- BoxCox.lambda(x1) # forecast package
# why are they different?
# returning a bc transformed variable
hist(bcPower(x1, lambda))
hist(BoxCox(x1, lambda.alt))
# car seems to be better in this case
x2 <- runif(n)

x3 <- factor(sample(c("A", "B", "C", "D")
                    , size = n 
                    , replace = TRUE))

x3 <- ifelse(x2 + noise3 > 0.5 & x3 == "A", "D", as.character(x3))

A <- which(x3 == "A")
B <- which(x3 == "B")
C <- which(x3 == "C")
D <- which(x3 == "D")

bA <- 0.5
bB <- 0.8
bC <- -0.2
bD <- 0.2

x4 <- rep(NA, n)
x4[A] <- bcPower(x1[A]
                 , lambda + mean(noise3[A])) + bA * noise1[A]
x4[B] <- bcPower(x1[B]
                 , lambda + mean(noise3[B])) + bB * noise1[B]
x4[C] <- bcPower(x1[C]
                 , lambda + mean(noise3[C])) + bC * noise1[C]
x4[D] <- bcPower(x1[D]
                 , lambda + mean(noise3[D])) + bD * noise1[D]

x4 <- x4/10

x5 <- rep(NA, n)
x5[A] <- bA + -log(ifelse(x2[A] < 0.5 + noise3[A]
                         , exp(x2[A])
                         , sqrt(x2[A])
                         )
                  ) / (pi + x2[A]^2) + noise2[A]
x5[B] <- bB + -log(ifelse(x2[B] < 0.5 + noise3[B]
                         , exp(x2[B])
                         , sqrt(x2[B])
                         )
                  ) / (pi + x2[B]^2) + noise2[B]
x5[C] <- bC + -log(ifelse(x2[C] < 0.5 + noise3[C]
                         , exp(x2[C])
                         , sqrt(x2[C])
                         )
                  ) / (pi + x2[C]^2) + noise2[C]
x5[D] <- bD + -log(ifelse(x2[D] < 0.5 + noise3[D]
                         , exp(x2[D])
                         , sqrt(x2[D])
                         )
                  ) / (pi + x2[D]^2) + noise2[D]

x6 <- x4
x6 <- ifelse(rbinom(n, 1, 0.33) == 1
            , x4 + noise3
            , runif(n, -2, 0.5))
for(i in 1:n) {
  x6[i] <- mean(c(x6[i], x4[i], x4[i]))
}

x7 <- 1/(1 + x5^2/2) + noise5

y <- x0 + noise0 +
  0.2 * x1 +
  0.1 * x1^2 +
  0.25 * x2 +
  -0.5 * x2^2 +
  -0.25 * (x2-0.1)^3 +
  8 * x4 +
  1.5 * x5 +
  5 * x6 + 
  3 * x7

xs <- data.frame(fx0 = factor(x0)
                 , x0, x1, x2, x3
                 , x4, x5, x6, x7, y)

xyplot(y~x1 | fx0, data = xs, groups = x3)
xyplot(y~x2 | fx0, data = xs, groups = x3)
xyplot(y~x3 | fx0, data = xs, groups = x3)
xyplot(y~x4 | fx0, data = xs, groups = x3)
xyplot(y~x5 | fx0, data = xs, groups = x3)
xyplot(y~x6 | fx0, data = xs, groups = x3)
xyplot(y~x7 | fx0, data = xs, groups = x3)

aggregate(xs[, c("x0", "x1", "x4", "x2"
                 , "x5", "x6", "x7", "y")]
          , list(xs$x3), mean)

corrgram(xs[, c("x1", "x4", "x2"
                , "x5", "x6", "x7", "y")]
         , diag.panel = panel.density
         , lower.panel = panel.ellipse
         , upper.panel = panel.pie
         , order = "HC")

gpairs(xs)

# scatterplotMatrix(xs
#                   , formula = ~x0 + x1 +
#                     x2 + x4 + x5 + x6 + x7 + y
#                   , data = xs
#                   , diagonal = "histogram")

corrplot(cor(xs[,-c(1, 5)]))
corrplot.mixed(cor(xs[,-c(1, 5)])
               , upper = "ellipse"
               , col = colorpanel(50, "red", "transparent", "blue4")
               )

plot(x1, y, col = xs$fx0)
plot(x2, y, col = xs$fx0)
plot(x4, y, col = xs$fx0)
plot(x5, y, col = xs$fx0)
plot(x6, y, col = xs$fx0)
plot(x7, y, col = xs$fx0)

histogram(~x1 | x3 + fx0, data = xs)
histogram(~x2 | x3 + fx0, data = xs)
histogram(~x4 | x3 + fx0, data = xs)
histogram(~x5 | x3 + fx0, data = xs)
histogram(~x6 | x3 + fx0, data = xs)
histogram(~x7 | x3 + fx0, data = xs)
histogram(~y | x3 + fx0, data = xs)

tab.1 <- with(xs, table(fx0, x3))
tab.1
chisq.test(tab.1)
p.tab.1 <- round(prop.table(tab.1, 2), 2)
p.tab.1
dotplot(p.tab.1[2, ], xlab="Proportion by x0:x3"
         , col="darkolivegreen", cex = 2.5)

x1.x3.mean <- aggregate(x1~x3, data=xs, mean)
dotplot(x3~x1, data=x1.x3.mean
        , col="darkolivegreen"
        , cex = 2.5
        , xlim = c(0, 0.4))
x2.x3.mean <- aggregate(x2~x3, data=xs, mean)
dotplot(x3~x2, data=x2.x3.mean
        , col="darkolivegreen"
        , cex = 2.5)
x4.x3.mean <- aggregate(x4~x3, data=xs, mean)
dotplot(x3~x4, data=x4.x3.mean
        , col="darkolivegreen"
        , cex = 2.5)
x5.x3.mean <- aggregate(x5~x3, data=xs, mean)
dotplot(x3~x5, data=x5.x3.mean
        , col="darkolivegreen"
        , cex = 2.5)
x6.x3.mean <- aggregate(x6~x3, data=xs, mean)
dotplot(x3~x6, data=x6.x3.mean
        , col="darkolivegreen"
        , cex = 2.5
        , xlim = c(-0.4, -0.1))
x7.x3.mean <- aggregate(x7~x3, data=xs, mean)
dotplot(x3~x7, data=x7.x3.mean
        , col="darkolivegreen"
        , cex = 2.5
        , xlim = c(2.75, 3.0))
y.x3.mean <- aggregate(y~x3, data=xs, mean)
dotplot(x3~y, data=y.x3.mean
        , col="darkolivegreen"
        , cex = 2.5)

bwplot(x1~x3, data = xs)
bwplot(x2~x3, data = xs)
bwplot(x4~x3, data = xs)
bwplot(x5~x3, data = xs)
bwplot(x6~x3, data = xs)
bwplot(x7~x3, data = xs)
bwplot(y~x3, data = xs)

t.test(x1~fx0, data = xs)
t.test(x2~fx0, data = xs)
t.test(x4~fx0, data = xs)
t.test(x5~fx0, data = xs)
t.test(x6~fx0, data = xs)
t.test(x7~fx0, data = xs)
t.test(y~fx0, data = xs)

x1.aov <- aov(x1~x3, data = xs)
anova(x1.aov)
x2.aov <- aov(x2~x3, data = xs)
anova(x2.aov)
x4.aov <- aov(x4~x3, data = xs)
anova(x4.aov)
x5.aov <- aov(x5~x3, data = xs)
anova(x5.aov)
x6.aov <- aov(x6~x3, data = xs)
anova(x6.aov)
x7.aov <- aov(x7~x3, data = xs)
anova(x7.aov)
y.aov <- aov(y~x3, data = xs)
anova(y.aov)

fit <- step(aov(y~., data = xs))
fit$coefficients # estimates for x4, x6, x5 are out

# remove the intercept term and use glht to plot confint
plot(glht(update(x1.aov, x1~.-1)))
plot(glht(update(x2.aov, x2~.-1)))
plot(glht(update(x4.aov, x4~.-1)))
plot(glht(update(x5.aov, x5~.-1)))
plot(glht(update(x6.aov, x6~.-1)))
plot(glht(update(x7.aov, x7~.-1)))
plot(glht(update(y.aov, y~.-1)))

fit.1 <- lm(y~fx0, data = xs)
summary(fit.1)

fit.1b <- lm(y~x3, data = xs)
summary(fit.1b) # no use

fit.2 <- lm(y~fx0 + x4 + x5, data = xs)
summary(fit.2)

fit.3 <- lm(y~fx0 + x4 + x5 + x3, data = xs)
summary(fit.3)

fit.4 <- lm(y~fx0 + x4 + x5 + x1 + x2, data = xs)
summary(fit.4)

anova(fit.1, fit.2, fit.3)
anova(fit.1, fit.2, fit.4)

fit.5 <- lm(y~fx0 + x4 + x5 + x1 + x2 + x3, data = xs)
summary(fit.5)
anova(fit.1, fit.2, fit.4, fit.5)

fit.6 <- lm(y~fx0 + x4 + x5 + x1 + x2 + x6 + x7, data = xs)
summary(fit.6)
anova(fit.1, fit.2, fit.4, fit.6)

fit.7 <- lm(y~fx0 + x4 + x5 + x6 + x7, data = xs)
summary(fit.7)
anova(fit.7, fit.6)
vif(fit.7) # x5 and x7 are colinear, x4 and x6 moderately so

# We could create a combined variable
# e.g. use PCA just on these 2
# e.g. take the geo.mean
z1 <- prcomp(xs[, c("x4", "x6")])
z2 <- prcomp(xs[, c("x5", "x7")])
z3 <- mapply(function(a, b) {1/mean(1/a + 1/b)}, x5, x7)
xs <- cbind(xs, z1 = z1$x[, 1], z2 = z2$x[, 1], z3)
# the prcomp is very slightly better than the geo mean for z2
fit.z <- lm(y~fx0 + z1 + z2, data = xs)

AIC(fit.7, fit.z, fit.6, fit.4)
# fit 6 with all the vars that made up the model is still the best
# unsurprising, but we rarely have the luxury of knowing for sure

# best fit 6
coefplot(fit.6, outerCI=1.96, lwdOuter=1)
cor(xs$y, fit.6$fitted)
xyplot(fit.6$fitted~xs$y)

# a look at the others
coefplot(fit.4, outerCI=1.96, lwdOuter=1)
cor(xs$y, fit.4$fitted)
xyplot(fit.4$fitted~xs$y)

coefplot(fit.7, outerCI=1.96, lwdOuter=1)
cor(xs$y, fit.7$fitted)
xyplot(fit.7$fitted~xs$y)

coefplot(fit.z, outerCI=1.96, lwdOuter=1) # hard to interpret the coefs
cor(xs$y, fit.z$fitted) # accuracy has taken a drop
xyplot(fit.z$fitted~xs$y) # there's a weird gap in the pattern

# a bit of gam - what it should be:
fit.amaze <- gam(y~fx0 +
                   poly(x1, 3) +
                   poly(x2, 2) +
                   x3 +
                   x4 +
                   x4:x1 +
                   ns(x5, knots = 1) +
                   x5:x2 +
                   x6 +
                   x6:x4 +
                   x7 +
                   x7:x5, data = xs)
summary(fit.amaze)
fit.amaze2 <- gam(y~fx0 +
                    poly(x2, 2) +
                    x4 +
                    ns(x5, df = 6) +
                    x6 +
                    x7, data = xs)
summary(fit.amaze2)
cor(xs$y, fit.amaze2$fitted)
xyplot(fit.amaze2$fitted~xs$y)

# bayesian ANOVA for model selection
set.seed(96761)
bf1 <- lmBF(y~fx0, data = xs)
bf2 <- lmBF(y~fx0 + x4 + x5, data = xs)
bf4 <- lmBF(y~fx0 + x4 + x5 + x1 + x2, data = xs)
bf5 <- lmBF(y~fx0 + x4 + x5 + x1 + x2 + x6 + x7, data = xs)

bf1/bf2
bf2/bf4
bf4/bf5

# model 1 is terriburr
# hasn't found fx0, but not really worse than fit.1
bf1.chain <- posterior(bf1, 1, iterations = 10000)
plot(bf1.chain[, 1:3])
summary(bf1.chain)

# model 2 is not so terriburr
# same results for fx0
# but moderately accurate parameter for x4 and x5
bf2.chain <- posterior(bf2, 1, iterations = 10000)
plot(bf2.chain[,1:3])
plot(bf2.chain[,4:6])
summary(bf2.chain)

# model 4 is improved
# so Bayesian homes in as the model improves
bf4.chain <- posterior(bf4, 1, iterations = 10000)
plot(bf4.chain[,1:3])
plot(bf4.chain[,4:5])
plot(bf4.chain[,6:7])
summary(bf4.chain)

mc1 <- MCMCregress(y~fx0, data = xs)
mc2 <- MCMCregress(y~fx0 + x4 + x5, data = xs)
mc4 <- MCMCregress(y~fx0 + x4 + x5 + x1 + x2, data = xs)
mc5 <- MCMCregress(y~fx0 + x4 + x5 + x1 + x2 + x6 + x7, data = xs)

summary(mc1)
summary(mc2)
summary(mc4)
summary(mc5)
# MCMCregress is quite good at finding the values

# glm logit, probit
xs$cy <- ifelse(scale(y) > 0, TRUE, FALSE)
glm.1 <- glm(cy ~ fx0 + x1 + x2, data = xs
             , family = binomial("logit"))
glm.1.pred <- ifelse(predict(glm.1, type = "response") > 0.5
                     , TRUE, FALSE)
glm.1.pred.tab <- table(prediction = glm.1.pred, actual = xs$cy)
glm.1.metrics <- createMetrics(glm.1.pred.tab)

glm.2 <- glm(cy ~ fx0 + x1 + x2 + x4 + x5, data = xs
             , family = binomial("logit"))
glm.2.pred <- ifelse(predict(glm.2, type = "response") > 0.5
                     , TRUE, FALSE)
glm.2.pred.tab <- table(prediction = glm.2.pred, actual = xs$cy)
glm.2.metrics <- createMetrics(glm.2.pred.tab)

glm.3 <- glm(cy ~ fx0 + x1 + x2 + x4 + x5 + x6 + x7, data = xs
             , family = binomial("logit"))
glm.3.pred <- ifelse(predict(glm.3, type = "response") > 0.5
                     , TRUE, FALSE)
glm.3.pred.tab <- table(prediction = glm.3.pred, actual = xs$cy)
glm.3.metrics <- createMetrics(glm.3.pred.tab)

glm.4 <- glm(cy ~ fx0 + x3, data = xs
             , family = binomial("logit"))
glm.4.pred <- ifelse(predict(glm.4, type = "response") > 0.5
                     , TRUE, FALSE)
glm.4.pred.tab <- table(prediction = glm.4.pred, actual = xs$cy)
glm.4.metrics <- createMetrics(glm.4.pred.tab)

glm.5 <- glm(cy ~ fx0 + z1 + z2, data = xs
             , family = binomial("logit"))
glm.5.pred <- ifelse(predict(glm.5, type = "response") > 0.5
                     , TRUE, FALSE)
glm.5.pred.tab <- table(prediction = glm.5.pred, actual = xs$cy)
glm.5.metrics <- createMetrics(glm.5.pred.tab)

glm.3p <- glm(cy ~ fx0 + x1 + x2 + x4 + x5 + x6, data = xs
             , family = binomial("probit"))
glm.3p.pred <- ifelse(predict(glm.3p, type = "response") > 0.5
                     , TRUE, FALSE)
glm.3p.pred.tab <- table(prediction = glm.3p.pred, actual = xs$cy)
glm.3p.metrics <- createMetrics(glm.3p.pred.tab)

unlist(glm.3p.metrics)-unlist(glm.3.metrics) # probit model better?

# ratings
set.seed(99099)
ratings <- readRDS("brand.ratings.marketing.RData")
ratings$x3 <- rep(NA, n)
ratings$x3 <- ifelse(ratings$brand %in% c("f", "g")
                     , "A", ratings$x3)
ratings$x3 <- ifelse(ratings$brand %in% c("b", "c")
                     , "B", ratings$x3)
ratings$x3 <- ifelse(ratings$brand %in% c("i", "h", "d")
                     , "C", ratings$x3)
ratings$x3 <- ifelse(ratings$brand %in% c("e", "a", "j")
                     , "D", ratings$x3)

ratings$brand <- NULL

ratings.A <- ratings[ratings$x3 == "A", ]
sample.A <- sample(nrow(ratings.A), length(A), replace = FALSE)

ratings.B <- ratings[ratings$x3 == "B", ]
sample.B <- sample(nrow(ratings.B), length(B), replace = TRUE)

ratings.C <- ratings[ratings$x3 == "C", ]
sample.C <- sample(nrow(ratings.C), length(C), replace = FALSE)

ratings.D <- ratings[ratings$x3 == "D", ]
sample.D <- sample(nrow(ratings.D), length(D), replace = TRUE)

ratings.sample <- rbind(ratings.A[sample.A,]
                        , ratings.B[sample.B,]
                        , ratings.C[sample.C,]
                        , ratings.D[sample.D,])

ratings.sample$x3 <- NULL
ratings.scale <- scale(ratings.sample)

xs.ratings <- data.frame(xs[order(x3), ], ratings.scale)

ratings.only <- xs.ratings[, -c(1:4, 6:14)]

ratings.mean <- aggregate(.~x3, data=ratings.only, mean)
row.names(ratings.mean) <- ratings.mean[, 1]
ratings.mean <- ratings.mean[, -1]

heatmap.2(as.matrix(ratings.mean),
          col=brewer.pal(9, "GnBu")
          , trace="none"
          , key=FALSE
          , dend="both"
          , main="Attributes"
)

# this section all makes more sense with the book data
# as there are more brands to compare among one another
# PCA
ratings.pca <- prcomp(ratings.scale)
summary(ratings.pca)
plot(ratings.pca, type = "l")
biplot(ratings.pca, col = c("transparent", "black"))

ratings.mu.pca <- prcomp(ratings.mean)
summary(ratings.mu.pca)
plot(ratings.mu.pca, type = "l")
biplot(ratings.mu.pca)

# EFA
nScree(ratings.only[, -1]) # suggests 3
eigen(cor(ratings.only[, -1]))[[1]] # suggests first 3 are > 1
ratings.fa <- factanal(ratings.only[, -1], factors = 3)
# try with a rotation function
ratings.fa.ob <- factanal(ratings.only[, -1]
                          , factors=3
                          , rotation="oblimin"
                          , scores = "Bartlett"
                          )

heatmap.2(as.matrix(ratings.fa.ob$loadings),
          col=brewer.pal(9, "GnBu")
          , trace="row"
          , tracecol = "red"
          , key=FALSE
          , dend="row"
          , main="Factors"
)
heatmap.2(as.matrix(ratings.fa.ob$loadings),
          col=brewer.pal(9, "GnBu")
          , trace="none"
          , key=FALSE
          , dend="none"
          , main="Factors"
)
semPaths(ratings.fa.ob
         , what="est"
         , residuals=TRUE
         , cut=0.275
         , layout = "tree2"
         , posCol=c("white", "darkgreen")
         , negCol=c("white", "red")
         , edge.label.cex=0.75
         , nCharNodes=7)

ratings.fa.scores <- data.frame(ratings.fa.ob$scores)
ratings.fa.scores$x3 <- ratings.only$x3
ratings.fa.mean <- aggregate(.~x3
                             , data=ratings.fa.scores
                             , mean)
rownames(ratings.fa.mean) <- ratings.fa.mean[, 1] # ratings names on rownames
ratings.fa.mean <- ratings.fa.mean[, -1] # remove ratings names col
names(ratings.fa.mean) <- c("Leader", "Value", "Trendy") # factor names
ratings.fa.mean
heatmap.2(as.matrix(ratings.fa.mean)
          , col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none"
          , cexCol=1.2, main="Mean factor score\nby brand")

# MDS
ratings.dist <- dist(ratings.mean)
ratings.mds <- cmdscale(ratings.dist)
plot(ratings.mds, type = "n")
text(ratings.mds, rownames(ratings.mds))

xs.mean <- aggregate(xs[, c("x0", "x1", "x4", "x2", "x5", "y")], list(xs$x3), mean)
rownames(xs.mean) <- xs.mean[, 1]
xs.mean <- xs.mean[, -1]
xs.dist <- dist(xs.mean)
xs.mds <- cmdscale(xs.dist)
plot(xs.mds, type = "n")
text(xs.mds, rownames(ratings.mds))

# simulate ranked data
ratings.rank <- data.frame(lapply(
  ratings.mean
  , function(x) ordered(rank(x))))
str(ratings.rank)
ratings.rank.dist <- daisy(ratings.rank
                           , metric="gower") # useful as it's an ordered factor
ratings.rank.mds <- isoMDS(ratings.rank.dist)
plot(ratings.rank.mds$points, type = "n")
text(ratings.rank.mds$points, levels(ratings.only$x3))

# hierarchical linear models
# classical
hlm.1 <- lmer(y~perform + leader +
      latest + fun +
      serious + bargain +
      value + trendy +
      rebuy + (1 | x3), data = ratings.only)
fixef(hlm.1)
ranef(hlm.1)

hlm.1a <- lmer(y~fx0 + x1 + x2 +
                 x4 + x5 + 
                 x6 + x7 + 
                 (1 | x3), data = xs)
fixef(hlm.1a)
ranef(hlm.1a)

hlm.2 <- lmer(y~(perform + leader +
                latest + fun +
                serious + bargain +
                value + trendy +
                rebuy | x3), data = ratings.only)
fixef(hlm.2)
ranef(hlm.2)

hlm.2a <- lmer(y~(fx0 + x1 + x2 +
                 x4 + x5 + 
                 x6 + x7 | x3), data = xs
               , control = lmerControl(
                 optCtrl = list(maxfun=150000))
               )
fixef(hlm.2a)
ranef(hlm.2a)

# bayesian
set.seed(97439)
bhlm.1 <-MCMChregress(
  fixed = y ~ perform + leader +
    latest + fun +
    serious + bargain +
    value + trendy + rebuy
  , random = ~ perform + leader +
    latest + fun +
    serious + bargain +
    value + trendy + rebuy
  , group = "x3", data = ratings.only
  , r = 10, R = diag(10))

summary(bhlm.1$mcmc[,1:10])
summary(bhlm.1$mcmc[,grepl(".A", colnames(bhlm.1$mcmc), fixed=TRUE)])
summary(bhlm.1$mcmc[,grepl(".B", colnames(bhlm.1$mcmc), fixed=TRUE)])
summary(bhlm.1$mcmc[,grepl(".C", colnames(bhlm.1$mcmc), fixed=TRUE)])
summary(bhlm.1$mcmc[,grepl(".D", colnames(bhlm.1$mcmc), fixed=TRUE)])

bhlm.1a <-MCMChregress(
  fixed = y ~ fx0 + x1 + x2 + x4 + x5 + x6 + x7
  , random = ~ fx0 + x1 + x2 + x4 + x5 + x6 + x7
  , group = "x3", data = xs
  , r = 8, R = diag(8))

summary(bhlm.1a$mcmc[,1:8])
summary(bhlm.1a$mcmc[,grepl(".A", colnames(bhlm.1a$mcmc), fixed=TRUE)])
summary(bhlm.1a$mcmc[,grepl(".B", colnames(bhlm.1a$mcmc), fixed=TRUE)])
summary(bhlm.1a$mcmc[,grepl(".C", colnames(bhlm.1a$mcmc), fixed=TRUE)])
summary(bhlm.1a$mcmc[,grepl(".D", colnames(bhlm.1a$mcmc), fixed=TRUE)])

# analysing the distribution of each variable in the model
# with more than four, a histogram would be interesting
fx0.summary <- summary(
  bhlm.1a$mcmc[, grepl("b.fx02"
               , colnames(bhlm.1a$mcmc)
               , fixed=TRUE)] +
  bhlm.1a$mcmc[, "beta.fx02"]
  )

# function to get the range and grid for classical
xsequence <- function(mu, st.d) {
  seq(mu - 4 * st.d, mu + 4 * st.d, .01)
}
# function to create a density plot for classical
dens <- function(x, mu, st.d) {
  dnorm(x, mu, st.d)
}
xseq <- xsequence(2, 0.25)
d <- as.data.frame(mapply(
  function(x, y) {
    dens(xseq, x, y) }
  , fx0.summary$statistics[, 1]
  , fx0.summary$statistics[, 2]))
names(d) <- c("A", "B", "C", "D")
xyplot(d$A + d$B + d$C + d$D ~ xseq
       , type = "l"
       , ylab = "Densities of fx0 for each member"
       , key = simpleKey(text = names(d)
                         , points = FALSE
                         , lines = TRUE
                         , columns = 2))

# compare fixed from the two models
# this is the doctored ratings model. Not brilliant
hlm1.fixed.means <- fixef(hlm.1)
bhlm1.fixed.means <- colMeans(bhlm.1$mcmc[, 1:10])
xyplot(bhlm1.fixed.means~hlm1.fixed.means
       , xlim = c(-0.25, 0.2), ylim = c(-0.25, 0.2)
       , panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(a = 0, b = 1)
       }
       )

# this is the sim data. Good matching across both
hlm1a.fixed.means <- fixef(hlm.1a)
bhlm1a.fixed.means <- colMeans(bhlm.1a$mcmc[, 1:8])
xyplot(bhlm1a.fixed.means~hlm1a.fixed.means
       #, xlim = c(-0.25, 0.2), ylim = c(-0.25, 0.2)
       , panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(a = 0, b = 1)
       }
)

# doctored ratings
hlm2.ranA.means <- ranef(hlm.2)$x3["A",]
bhlm1.ranA.means <- colMeans(bhlm.1$mcmc[,grepl(".A", colnames(bhlm.1$mcmc), fixed=TRUE)])
xyplot(hlm2.ranA.means~bhlm1.ranA.means
       , panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(a = 0, b = 1)
       }
)

# sim - all bad, A-D
hlm2a.ranA.means <- ranef(hlm.2a)$x3["B",]
bhlm1a.ranA.means <- colMeans(bhlm.1a$mcmc[,grepl(".B", colnames(bhlm.1a$mcmc), fixed=TRUE)])
xyplot(hlm2a.ranA.means~bhlm1a.ranA.means
       , panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(a = 0, b = 1)
       }
)
# from the MCMC
# get the fix posterior draws
bhlm.cols.fix <- which(grepl("beta.", colnames(bhlm.1a$mcmc), fixed=TRUE))
colnames.fix <- colnames(bhlm.1a$mcmc)[bhlm.cols.fix]
bhlm.post.draw.fix <- as.data.frame(bhlm.1a$mcmc[, bhlm.cols.fix])
names(bhlm.post.draw.fix) <- colnames.fix
# and those for one user
bhlm.cols.A <- which(grepl(".A", colnames(bhlm.1a$mcmc), fixed=TRUE))
colnames.A <- colnames(bhlm.1a$mcmc)[bhlm.cols.A]
bhlm.post.draw.A <- as.data.frame(bhlm.1a$mcmc[, bhlm.cols.A])
names(bhlm.post.draw.A) <- colnames.A

# from the classical - get for one user the mean and post vars
# YUCKY code required
hlm2a.ranA.means <- ranef(hlm.2a)$x3["A",]
hlm2a.ranA.sds <- sqrt(diag(attr(ranef(hlm.2a
                  , condVar = TRUE)$x3
                  , "postVar")[, , 1])) # YUCK!
names(hlm2a.ranA.sds) <- names(hlm2a.ranA.means)

# set the required param
# MCMC
draw <- bhlm.post.draw.A$b.x5.A + 
  bhlm.post.draw.fix$beta.x5
# classical
mn <- hlm2a.ranA.means$x5
std <- hlm2a.ranA.sds["x5"]
xseq <- xsequence(mn, std)

# compare densities
densityplot(~draw, plot.points = FALSE
            , prepanel = function(x, y) {
              list(
              xlim = c(min(c(draw, xseq))
                   , max(c(draw, xseq)))
              , ylim = c(0
                , max(c(dnorm(xseq, mean = mn, sd = std)
            , dens(xseq, mn, std))))
              )}
            , panel = function(x, ...){
              panel.densityplot(x, ...)
              panel.lines(x = xseq
                          , y = dens(xseq, mn, std)
                          , col = "pink")
            })


#CFA - to assess if items believed to be related actually are
# for example, questions in a survey that we hope to validate a latent variable
set.seed(10069)
# create a model
cfaModel <- "murga = ~ x1 + x4 + x6
              troyd = ~ x2 + x5 + x7
              laplace = ~ x0
              PIES = ~ murga + troyd + laplace"
# create a data generation model
cfaDataModel <- "murga = ~ 0.25*x1 + -3*x4 + 3*x6
              troyd = ~ 0.25*x2 + 1*x5 + 0.5*x7
              laplace = ~ 1*x0
              PIES = ~ 1*murga + 1*troyd + 1*laplace"
cfaSimData.norm <- simulateData(cfaDataModel, sample.nobs=n)

createSevenPointScale <- function(x) { cut(x, breaks=7, labels=FALSE) } 

cfaSimData <- data.frame(lapply(
  cfaSimData.norm
  , createSevenPointScale))
xs.cfa <- data.frame(lapply(
  xs[, c(2:4, 6:10)]
  , createSevenPointScale))
scatterplotMatrix(cfaSimData
                  , diag="histogram"
                  , col=brewer.pal(3, "Paired")
                  , ellipse=TRUE)
factanal(cfaSimData, 3) # just a look
# assess the simulated data to the model
sim.fit <- cfa(cfaModel, data = cfaSimData)
summary(sim.fit, fit.measures=TRUE)
semPaths(sim.fit
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75)
# assess our existing data to the model
xs.fit <- cfa(cfaModel, data = xs)
summary(xs.fit, fit.measures=TRUE)
semPaths(xs.fit
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75)
semPaths(xs.fit
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75
         , structural = TRUE)

# create other models for comparison
cfaModel.simple <- "PIES = ~ x1 + x4 + x6 + x2 + x5 + x7 + fx0"
sim.fit.simple <- cfa(cfaModel.simple, data = cfaSimData)
summary(sim.fit.simple, fit.measures=TRUE)
semPaths(sim.fit.simple
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75)
xs.fit.simple <- cfa(cfaModel.simple, data = xs)
summary(xs.fit.simple, fit.measures=TRUE)
semPaths(xs.fit.simple
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75)

cfaModel.inter <- "murga =~ x1 + x4 + x6
              troyd =~ x2 + x5 + x7
              laplace =~ x0
              murga ~~ 0.05*troyd
              murga ~~ 0.05*laplace
              troyd ~~ 0.05*laplace"
sim.fit.inter <- cfa(cfaModel.inter, data = cfaSimData)
summary(sim.fit.inter, fit.measures=TRUE)
semPaths(sim.fit.inter
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75)
xs.fit.inter <- cfa(cfaModel.inter, data = xs)
summary(xs.fit.inter, fit.measures=TRUE)
semPaths(xs.fit.inter
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75)
semPaths(xs.fit.inter
         , what="est"
         , fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75
         , structural = TRUE)

compareFit(sim.fit.simple, sim.fit.inter, sim.fit)
compareFit(xs.fit.simple, xs.fit.inter, xs.fit)

plsMeasureModel <- matrix(c(
  "murga", "x1"
  , "murga", "x4"
  , "murga", "x6"
  , "troyd", "x2"
  , "troyd", "x5"
  , "troyd", "x7"
  , "laplace", "x0"), ncol = 2, byrow = TRUE)

plsStrucModel <- matrix(c(
  "laplace", "murga"
  , "laplace", "troyd"), ncol = 2, byrow = TRUE)

xs.plsm <- plsm(data = xs.cfa
                , strucmod = plsStrucModel
                , measuremod = plsMeasureModel)
xs.plsm.fit <- sempls(data = xs.cfa
                      , model = xs.plsm)

# assessing PLSM 
plsLoadings(xs.plsm.fit)
pathCoeff(xs.plsm.fit)
coef(xs.plsm.fit)
set.seed(4460)
xs.plsm.boot <- bootsempls(xs.plsm.fit
                           , nboot = n
                           , start = "ones")
summary(xs.plsm.boot, type = "bca", level = 0.9)
parallelplot(xs.plsm.boot
             , reflinesAt = 0
             , alpha=c(0.25, 1, 1, 1)
             , varnames=attr(xs.plsm.boot$t
                             , "path")[8:9]
             , main="Path coefficients in 1000 PLS bootstrap iterations (N=1000)")
parallelplot(xs.plsm.boot
             , reflinesAt = 0
             , alpha=c(0.25, 1, 1, 1)
             , pattern = "lam"
             , varnames=attr(xs.plsm.boot$t
                             , "path")[1:7]
             , main="Path coefficients in 1000 PLS bootstrap iterations (N=1000)")