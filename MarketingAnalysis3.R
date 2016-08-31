seg.df <- read.csv("http://goo.gl/qw303p")
mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & seg.df$subscribe=="subNo"])
by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)
aggregate(seg.df$income, list(seg.df$Segment), mean)

seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]
aggregate(income ~ Segment + ownHome, data=seg.df, mean)
aggregate(income ~ Segment + ownHome + subscribe, data=seg.df, mean)
xtabs(kids ~ Segment, data=seg.df)
aggregate(kids ~ Segment, data=seg.df, sum)

seg.tab <- with(seg.df, table(kids, Segment))
apply(seg.tab*0:7, 2, sum)

library(lattice)
histogram(~subscribe | Segment, data=seg.df
          , layout=c(2,2))
histogram(~subscribe | Segment, data=seg.df, type="count",
           layout=c(4,1), col=c("burlywood", "darkolivegreen"))
histogram(~subscribe | Segment + ownHome, data=seg.df)

prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)
p.tab <- prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)
barchart(p.tab[2, ], xlim=c(0, p.tab[2, ]+0.05)
         , xlab="Subscriber proportion by Segment", col="darkolivegreen")

seg.mean <- aggregate(income~Segment, data=seg.df, mean)
barchart(income~Segment, data=seg.mean, col="grey")

seg.income.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
barchart(income ~ Segment, data=seg.income.agg, groups=ownHome
         , auto.key=TRUE, par.settings = simpleTheme(col=terrain.colors(2)))

boxplot(income ~ Segment, data=seg.df, yaxt="n", ylab="Income ($k)")
ax.seq <- seq(from=0, to=120000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)

bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab = "Income")
bwplot(income ~ Segment | ownHome, data=seg.df, horizontal=FALSE, xlab="Income")

# chi sq simple example
tmp.tab <- table(rep(c(1:4), times=c(25,25,25,20)))
tmp.tab
chisq.test(tmp.tab)
# this data shows no evidence that the groups in the population
# are of unequal size, under the assumption of random sampling

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,10)))
tmp.tab
chisq.test(tmp.tab)

binom.test(12, 20, p=0.5)

# sensitivity to sample size - smaller groups, same proportions no longer signif
chisq.test(tmp.tab/5)
binom.test(120, 200, p=0.5)

# on the simulated marketing data
table(seg.df$Segment)
chisq.test(table(seg.df$Segment))

# what's the probability of seeing 8 to 12 successes out of 20?
sum(dbinom(8:12, 20, 0.5))
# a less conservative confint than exact binom test
library(binom)
binom.confint(12, 20, method="ac")

binom.test(0, 20, p=0.5)
binom.confint(0, 20, method="ac")

table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim=TRUE, B=10000)

# tests revisited
hist(seg.df$income)
with(seg.df, hist(income[ownHome=="ownYes"]))
with(seg.df, hist(income[ownHome=="ownNo"]))
t.test(income ~ ownHome, data=seg.df)
# If these are representative data of a larger population, we can have 95 %
# confidence that the group difference is between those values.

t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))

# anova
seg.aov.own <- aov(income ~ ownHome, data=seg.df)
anova(seg.aov.own)
seg.aov.seg <- aov(income ~ Segment, data=seg.df)
anova(seg.aov.seg)

anova(aov(income ~ Segment + ownHome, data=seg.df))
anova(aov(income ~ Segment * ownHome, data=seg.df))

library(multcomp)
seg.aov <- aov(income ~ Segment - 1, data=seg.df)
glht(seg.aov)
oldpar <- par()
par(mar=c(6,10,2,2)) # adjusts margins to preserve axis labels
plot(glht(seg.aov), xlab="Income"
     , main="Average Income by Segment (95% CI)")
par(oldpar)

seg.aov.step <- step(aov(income ~ ., data=seg.df))
anova(seg.aov.step)

# Bayesian Anova
set.seed(96761)
library(BayesFactor)
seg.bf1 <- lmBF(income ~ Segment, data=seg.df)
seg.bf2 <- lmBF(income ~ Segment + ownHome, data=seg.df)
seg.bf1 / seg.bf2

seg.bf.chain <- posterior(seg.bf1, 1, iterations = 10000)
plot(seg.bf.chain[, 1:6])
summary(seg.bf.chain)
# add column 1 (population mu for each draw, to the group deviations)
seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1]
# apply quantile function to each
seg.bf.ci <- t(apply(seg.bf.chain.total, 2
                    , quantile, pr=c(0.025, 0.5, 0.975)))
seg.bf.ci

library(ggplot2)
seg.bf.df <- data.frame(seg.bf.ci)
seg.bf.df$Segment <- rownames(seg.bf.df)
p <- ggplot(seg.bf.df, aes(x=Segment, y=X50., ymax=X97.5., ymin=X2.5.))
p <- p + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Income")
p + ggtitle("95% CI for Mean Income by Segment") + coord_flip()
