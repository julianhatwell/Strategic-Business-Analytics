seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df <- seg.raw[ , -7] # remove the known segment assignments
summary(seg.df)

# quick sanity check function
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg.summ(seg.df, seg.raw$Segment) # could be used to check results of a clustering

# clustering
# calculating distance
c(1,2,3) - c(2,3,2) # vector of differences
sum((c(1,2,3) - c(2,3,2))^2) # the sum of squared differences
sqrt(sum((c(1,2,3) - c(2,3,2))^2)) # root sum of squares
dist(rbind(c(1,2,3), c(2,3,2)))
d <- dist(seg.df[, c("age", "income", "kids")])
as.matrix(d)[1:5, 1:5]

library(cluster) # daisy works with mixed data types
seg.dist <- daisy(seg.df)
as.matrix(seg.dist)[1:5, 1:5]
seg.hc <- hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])
seg.df[c(101, 107), ] # similar
seg.df[c(278, 294), ] # similar
seg.df[c(173, 141), ] # disimilar
cor(cophenetic(seg.hc), seg.dist) # a goodness of fit test for dendro
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red") # choosing a set of 4 groups
seg.hc.segment <- cutree(seg.hc, k=4) # membership vector for 4 groups
table(seg.hc.segment)
# reusing the sanity check function
seg.summ(seg.df, seg.hc.segment)
# "Our advanced hierarchical analysis in R examined
# consumers who don't yet subscribe and found two segments to target! The segments
# are known as 'Men' and 'Women."'

# visualising the results
plot(jitter(as.numeric(seg.df$gender)) ~
       jitter(as.numeric(seg.df$subscribe)),
     col=seg.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))

# prepare to numeric data for k-means
seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)

# k means
set.seed(96743)
seg.k <- kmeans(seg.df.num, centers=4)
seg.summ(seg.df, seg.k$cluster)
boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")

library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="K-means cluster plot")

# model based methods
library(mclust)
seg.mc <- Mclust(seg.df.num)
summary(seg.mc)

seg.mc3 <- Mclust(seg.df.num, G=3)
summary(seg.mc3)

seg.mc4 <- Mclust(seg.df.num, G=4)
summary(seg.mc4)

BIC(seg.mc, seg.mc3, seg.mc4)
# lower BIC is better and a difference > 10 is very strong

seg.summ(seg.df, seg.mc$class)
seg.summ(seg.df, seg.mc3$class)
seg.summ(seg.df, seg.mc4$class)

clusplot(seg.df, seg.mc$class, color=TRUE, shade=TRUE
         , labels=4, lines=0, main="Model-based cluster plot")

clusplot(seg.df, seg.mc3$class, color=TRUE, shade=TRUE
         , labels=4, lines=0, main="Model-based cluster plot")

clusplot(seg.df, seg.mc4$class, color=TRUE, shade=TRUE
         , labels=4, lines=0, main="Model-based cluster plot")

# polca works on categorical data
library(poLCA)
seg.df.cut <- seg.df
seg.df.cut$age <- factor(ifelse(seg.df$age < median(seg.df$age), 1, 2))
seg.df.cut$income <- factor(ifelse(seg.df$income < median(seg.df$income), 1, 2))
seg.df.cut$kids <- factor(ifelse(seg.df$kids < median(seg.df$kids), 1, 2))
summary(seg.df.cut)
# this is a formla
seg.f <- with(seg.df.cut,
              cbind(age, gender, income, kids, ownHome, subscribe)~1)

set.seed(02807)
seg.LCA3 <- poLCA(seg.f, data=seg.df.cut, nclass=3)
seg.LCA4 <- poLCA(seg.f, data=seg.df.cut, nclass=4)

seg.summ(seg.df, seg.LCA3$predclass)
table(seg.LCA3$predclass)
clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE,
           labels=4, lines=0, main="LCA plot (K=3)")

seg.summ(seg.df, seg.LCA4$predclass)
table(seg.LCA4$predclass)
clusplot(seg.df, seg.LCA4$predclass, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="LCA plot (K=3)")

#comparing models
table(seg.LCA3$predclass, seg.LCA4$predclass)
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)$aTOb
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)

# comparing to our known true model
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)
table(seg.raw$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass)

# classification and prediction
# naive Bayes
library(e1071)
set.seed(04625)
train.prop <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[train.cases, ]
seg.df.test <- seg.raw[-train.cases, ]

(seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train))
(seg.nb.class <- predict(seg.nb, seg.df.test))
prop.table(table(seg.nb.class))
mean(seg.df.test$Segment==seg.nb.class)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment) # substantially better than chance
table(seg.nb.class, seg.df.test$Segment)
clusplot(seg.df.test[, -7], seg.nb.class, color=TRUE, shade=TRUE,
         labels=4, lines=0,
         main="Naive Bayes classification, holdout data")

seg.summ(seg.df.test, seg.nb.class)
seg.summ(seg.df.test, seg.df.test$Segment)
predict(seg.nb, seg.df.test, type="raw")

# random forest
library(randomForest)
set.seed(98040)
(seg.rf <- randomForest(Segment ~ ., data=seg.df.train, ntree=3000, importance=TRUE))
seg.rf.class <- predict(seg.rf, seg.df.test)
clusplot(seg.df.test[, -7], seg.rf.class, color=TRUE, shade=TRUE,
           labels=4, lines=0, main="Random Forest classification, holdout data")

seg.summ(seg.df.test, seg.rf.class)
seg.summ(seg.df.test, seg.df.test$Segment)
mean(seg.df.test$Segment==seg.rf.class)
table(seg.df.test$Segment, seg.rf.class)
adjustedRandIndex(seg.df.test$Segment, seg.rf.class)

# looking at inner results of randfor
seg.rf.class.all <- predict(seg.rf, seg.df.test, predict.all=TRUE) # returns all tree preds
apply(seg.rf.class.all$individual[1:5, ], 1, table) / 3000 # returns proportion of votes of all trees
# var importance
importance(seg.rf)
varImpPlot(seg.rf, main="Variable importance by segment")

library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[ , 1:4]),
            col=brewer.pal(9, "Blues"),
            dend="none", trace="none", key=FALSE,
            margins=c(10, 10),
            main="Variable importance by segment"
            )

set.seed(92118)
train.prop <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train <- seg.df[train.cases, ]
sub.df.test <- seg.df[-train.cases, ]
# the next command uses subscribeYesNo as the cluster identifier in the plot
clusplot(sub.df.train[, -6], sub.df.train$subscribe, color=TRUE, shade=TRUE,
         labels=4, lines=0, main="Subscriber clusters, training data")

library(randomForest)
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000))
# this is a small proportion problem. Not many subscribers in the training data
# setting samp size will force the model to pick more subscribers
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000
                        , sampsize=c(25, 25)) )
# a higher error rate overall, but more subs are predicted
sub.rf.sub <- predict(sub.rf, sub.df.test)
table(sub.rf.sub, sub.df.test$subscribe)
adjustedRandIndex(sub.rf.sub, sub.df.test$subscribe)
library(psych)
cohen.kappa(cbind(sub.rf.sub, sub.df.test$subscribe))