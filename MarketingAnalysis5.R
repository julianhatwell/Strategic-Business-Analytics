library(gplots)
library(RColorBrewer)
library(corrplot)
library(nFactors)
library(GPArotation)
library(semPlot)
library(cluster)

brand.ratings <- read.csv("http://goo.gl/IQl8nc")
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
corrplot(cor(brand.sc[, 1:9]), order="hclust")

brand.mean <- aggregate(.~brand, data=brand.sc, mean) # collapse down to means
rownames(brand.mean) <- brand.mean[, 1] # use brand for the row names
brand.mean <- brand.mean[, -1] # remove brand name column

heatmap.2(as.matrix(brand.mean),
          col=brewer.pal(9, "GnBu")
          , trace="none"
          , key=FALSE
          , dend="none"
          , main="\n\n\n\nBrand attributes"
          )

# to conceptualise PCA
set.seed(98286)
xvar <- sample(1:10, 100, replace=TRUE)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, replace=TRUE)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, replace=TRUE)
my.vars <- cbind(xvar, yvar, zvar)
# take a look see
plot(yvar ~ xvar, data=jitter(my.vars))
plot(zvar ~ xvar, data=jitter(my.vars))
plot(zvar ~ yvar, data=jitter(my.vars))
corrplot(my.vars)

my.pca <- prcomp(my.vars)
summary(my.pca)
my.pca
cor(my.pca$x) # components have zero correlation

biplot(my.pca)
biplot(my.pca, choices = c(2,3)) # to visualise other components if less variation is captured in the first two.

# back to the marketing sim data
brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)
plot(brand.pc, type="l") # scree plot. first 3 components are most useful
biplot(brand.pc) # vectors are useful but points are too dense

brand.mu.pc <- prcomp(brand.mean, scale=TRUE) # using the aggregates
summary(brand.mu.pc)
plot(brand.mu.pc, type="l")
biplot(brand.mu.pc, main="Brand positioning", cex=c(1.5, 1))

# questions such as why is brand e undifferentiated and c is seen as a leader?
# be more like brand c
brand.mean["c", ] - brand.mean["e", ]
# try to find the gap among other brands to go into unclaimed space
colMeans(brand.mean[c("b", "c", "f", "g"), ]) - brand.mean["e", ]

# Exploratory Factor Analysis
nScree(brand.sc[, 1:9])
eigen(cor(brand.sc[, 1:9]))$values # confirming the results

factanal(brand.sc[, 1:9], factors=2)
factanal(brand.sc[, 1:9], factors=3)

# oblique rotation may reveal better aligned results, allows for correlations
(brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin"))
heatmap.2(brand.fa.ob$loadings
          , col=brewer.pal(9, "Greens"), trace="none", key=FALSE, dend="none"
          , Colv=FALSE, cexCol = 1.2,
            main="\n\n\n\nFactor loadings
for brand adjectives")

semPaths(brand.fa.ob, what="est", residuals=TRUE,
        cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
        edge.label.cex=0.75, nCharNodes=7)

# Extracting Scores
brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin"
                        , scores="Bartlett")
brand.scores <- data.frame(brand.fa.ob$scores) # get the factor scores
brand.scores$brand <- brand.sc$brand # get the matching brands
# The result is an estimated score for each respondent on each factor and brand.
brand.fa.mean <- aggregate(. ~ brand, data=brand.scores, mean) #aggregate these results
rownames(brand.fa.mean) <- brand.fa.mean[, 1] # brand names on rownames
brand.fa.mean <- brand.fa.mean[, -1] # remove brand names col
names(brand.fa.mean) <- c("Leader", "Value", "Latest") # factor names
brand.fa.mean

heatmap.2(as.matrix(brand.fa.mean)
          , col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none"
          , cexCol=1.2, main="\n\n\n\n\nMean factor score by brand")

# Multi Dimensional Scaling
brand.dist <- dist(brand.mean) # calculate distance matrix from raw
(brand.mds <- cmdscale(brand.dist))
plot(brand.mds, type="n")
text(brand.mds, rownames(brand.mds), cex=2)

# simulate ranked data
brand.rank <- data.frame(lapply(brand.mean, function(x) ordered(rank(x))))
str(brand.rank)
brand.dist.r <- daisy(brand.rank, metric="gower")
brand.mds.r <- isoMDS(brand.dist.r)

plot(brand.mds.r$points, type="n")
text(brand.mds.r$points, levels(brand.sc$brand), cex=2)