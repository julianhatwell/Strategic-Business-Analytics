cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)

spend.m1 <- lm(online.spend ~ .
               , data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m1)

# 98% variance explained? 
# standard error store.trans so high? 
# no pattern with online visits?

library(gpairs)
library(forecast) # Box Cox
gpairs(cust.df)

autoTransform <- function(x) {
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

# clean up complete cases only
cust.df.bc <- cust.df[complete.cases(cust.df), -1]
# online spend values greater than zero only
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
# exclude email (yes/no factor)
numcols <- which(colnames(cust.df.bc) != "email")
# apply function
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform)

summary(cust.df.bc)
gpairs(cust.df.bc)

spend.m2 <- lm(online.spend ~ .
               , data=cust.df.bc)
summary(spend.m2)

# this model still no better than one that predicts spend from transactions alone.
spend.m3 <- lm(online.spend ~ online.trans, data=cust.df.bc)
anova(spend.m3, spend.m2)

# collinear variables inflate one another's variance.
library(car)
vif(spend.m2)

# options
# 1. omit correlated variables
# 2. extract principle components or factors from correlated variables
# 3. use another approach to combine/transform variables (e.g. spend per transaction)
# 4. use a robust method such as RF

# option 1
spend.m4 <- lm(online.spend ~ . -online.trans -store.trans
               , data=cust.df.bc)
vif(spend.m4)
summary(spend.m4)

# option 2
pc.online <- prcomp(cust.df.bc[ , c("online.visits", "online.trans")])
cust.df.bc$online <- pc.online$x[ , 1]
pc.store <- prcomp(cust.df.bc[ , c("store.trans", "store.spend")])
cust.df.bc$store <- pc.store$x[ , 1]

spend.m5 <- lm(online.spend ~ email + age + credit.score +
                 distance.to.store + sat.service +
                 sat.selection + online + store
               , data=cust.df.bc)
vif(spend.m5)
summary(spend.m5)

# exploring logis
exp(0) / (exp(0) + 1) # equal probability

# computing logistic by hand; could use plogis()
plogis(-Inf) # infinitely low = likelihood 0
plogis(2) # moderate probability = 88% chance of outcome
plogis(-0.2) # weak likelihood

0.88 / (1-0.88) # odds
log(0.88 / (1-0.88)) # log odds
qlogis(0.88) # same

pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo
                        , levels=c("NoBundle", "Bundle"))
summary(pass.df)
with(pass.df, table(Pass, Promo, Channel))
mosaic(pass.tab, shade = TRUE)
mosaic(aperm(pass.tab, c(1, 3, 2)), shade = TRUE)
doubledecker(table(pass.df))

# create the data by hand
pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel=c("Mail", "Park", "Email")
                           , Promo=c("Bundle", "NoBundle")
                           , Pass=c("YesPass", "NoPass") )
library(vcdExtra)
pass.df <- expand.dft(pass.tab)
str(pass.df)

pass.m1 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)
plogis(0.3888) / (1-plogis(0.3888))

pass.m2 <- glm(Pass ~ Promo + Channel
               , data=pass.df
               , family=binomial)
summary(pass.m2)
exp(coef(pass.m2))
exp(confint(pass.m2))

pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel
               , data=pass.df, family=binomial)
summary(pass.m3)
exp(coef(pass.m3))
exp(confint(pass.m3))

#hierarchical lm
conjoint.df <- read.csv("http://goo.gl/G8knGV")
conjoint.df$speed <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
summary(conjoint.df)

# manual creation of sim data
set.seed(12814)
resp.id <- 1:200
nques <- 16
speed <- sample(as.factor(c("40", "50", "60", "70"))
                , size=nques, replace=TRUE)
height <- sample(as.factor(c("200", "300", "400"))
                 , size=nques, replace=TRUE)
const <- sample(as.factor(c("Wood", "Steel"))
                , size= nques, replace=TRUE)
theme <- sample(as.factor(c("Dragon", "Eagle"))
                , size=nques, replace=TRUE)

profiles.df <- data.frame(speed, height, const, theme)
profiles.model <- model.matrix(~ speed + height + const + theme
                               , data=profiles.df)
library(MASS)
weights <- mvrnorm(length(resp.id)
                   , mu=c(-3, 0.5, 1, 3, 2, 1, 0, -0.5)
                   , Sigma=diag(c(0.2, 0.1, 0.1, 0.1, 0.2, 0.3, 1, 1)))

conjoint.df <- NULL
for (i in seq_along(resp.id)) {
  # create one respondent’s ratings of the 16 items, plus error
  utility <- profiles.model %*% weights[i, ] + rnorm(16) # preference
  rating <- as.numeric(cut(utility, 10)) # put on a 10-point scale
  conjoint.resp <- cbind(resp.id=rep(i, nques)
                         , rating, profiles.df)
  conjoint.df <- rbind(conjoint.df, conjoint.resp)
}

summary(conjoint.df)

# example group means
by(conjoint.df$rating, conjoint.df$height, mean)

# plain lm
ride.lm <- lm(rating ~ speed + height + const + theme
              , data=conjoint.df)
summary(ride.lm)

# We estimate an overall rating for this most-desired coaster
# it would be the intercept+speed70+height300
#(steel and dragon are included in the intercept)
#, or 3.07+4.49+2.94 = 10.46 points on our 10-point rating scale. 
# But wait! That’s not possible; our scale is capped at 10 points. 
# This shows that simply interpreting 
# the “average” result can be misleading. 
# The coefﬁcients are estimated on the basis of designs 
# that mostly combine both desirable and undesirable attributes,
# and are not as reliable at the extremes of preference.
# Additionally,it could happen that few people prefer that exact
# combination even though the individual features are each
# best on average. 

library(lme4)
ride.hlm1 <- lmer(rating ~ speed + height + const + theme + 
                    (1 | resp.id)
                  , data=conjoint.df)
summary(ride.hlm1)
fixef(ride.hlm1)
head(ranef(ride.hlm1)$resp.id)
head(coef(ride.hlm1)$resp.id)

# this take several minutes
ride.hlm2 <- lmer(rating ~ speed + height + const + theme +
                    (speed + height + const + theme | resp.id)
                  , data=conjoint.df
                  , control=lmerControl(optCtrl=list(maxfun=100000)))

fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196, ]

# Bayesian approach
library(MCMCpack)
set.seed(97439)
ride.mc1 <- MCMCregress(rating ~ speed + height + const + theme
                        , data=conjoint.df)
summary(ride.mc1)

set.seed(97439)
ride.mc2 <- MCMChregress(fixed = rating ~ speed + height + const + theme
                         , random = ~ speed + height + const + theme
                         , group="resp.id"
                         , data=conjoint.df
                         , r=8, R=diag(8))
str(ride.mc2)
summary(ride.mc2$mcmc[ , grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE)])

summary(ride.mc2$mcmc[ ,1:8])

ride.constWood <- summary(ride.mc2$mcmc[ 
  , grepl("b.constWood"
  , colnames(ride.mc2$mcmc))] + 
    ride.mc2$mcmc[ , "beta.constWood"])

hist(ride.constWood$statistics[,1]
     , main="Preference for Wood vs. Steel"
     , xlab="Rating points", ylab="Count of Respondents"
     , xlim=c(-4,4))

ride.speed60 <- summary(ride.mc2$mcmc[,
                                      grepl("b.speed60"
                                            , colnames(ride.mc2$mcmc))] +
                        ride.mc2$mcmc[,"beta.speed60"])
hist(ride.speed60$statistics[,1]
     , main="Preference for 60 vs. 40,mph"
     , xlab="Rating points", ylab="Count of Respondents"
     , xlim=c(-4,4))

summary(ride.mc2$mcmc[,c("beta.constWood", "VCV.constWood.constWood"
                         , "beta.speed60","VCV.speed60.speed60")])

par(mfrow=c(2,2))
plot.xlim <- c(-2, 2) # define limits for the x-axis
# first four parameters only, for convenience
for (i in 2:5) {
  # plot the MCMC density for random effect i
  mcmc.col <- which(grepl(".196", colnames(ride.mc2$mcmc)
                          , fixed=TRUE))[i]
  plot(density(ride.mc2$mcmc[ , mcmc.col])
       , xlab="", ylim=c(0, 1.4), xlim=plot.xlim
       , main=paste("HB & lmer density:"
                    , colnames(ride.mc2$mcmc)[mcmc.col] ))
  # add the HLM density for random effect i
  hlm2.est <- ranef(ride.hlm2)$resp.id[196, i] # mean estimate
  hlm2.sd <- sqrt(attr(ranef(ride.hlm2, condVar=TRUE)$resp.id
                       , "postVar")[ , , 196][i, i])
  seq.pts <- seq(from=plot.xlim[1]
                 , to=plot.xlim[2]
                 , length.out=1000) # range
  # .. find density at x-axis points using dnorm()
  # and add that to the plot
  points(seq.pts, dnorm(seq.pts
                        , mean=hlm2.est
                        , sd=hlm2.sd)
         , col="red", pch=20, cex=0.05)
  legend("topright"
         , legend=c("red = lmer", "black = HB")
         , text.col=c("red", "black"))
}
par(mfrow = c(1,1))
