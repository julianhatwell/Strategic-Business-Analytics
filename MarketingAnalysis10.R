cbc.df <- read.csv("http://goo.gl/5xQObB"
                   , colClasses = c(seat = "factor", price = "factor"))
summary(cbc.df)
# building the sim data by hand
attrib <- list(seat = c("6", "7", "8")
               , cargo = c("2ft", "3ft")
               , eng = c("gas", "hyb", "elec")
               , price = c("30", "35", "40"))
# creating part worths
coef.names <- NULL
for (a in seq_along(attrib)) {
  coef.names <- c(coef.names
                  , paste(names(attrib)[a]
                          , attrib[[a]][-1], sep=""))
}
coef.names
mu <- c(-1, -1, 0.5, -1, -2, -1, -2) 
names(mu) <- coef.names
mu
Sigma <- diag(c(0.3, 1, 0.1, 0.3, 1, 0.2, 0.3))
dimnames(Sigma) <- list(coef.names, coef.names)
Sigma["enghyb", "engelec"] <- Sigma["engelec", "enghyb"] <- 0.3

set.seed(33040)
resp.id <- 1:200 # respondent ids
carpool <- sample(c("yes", "no")
                  , size=length(resp.id)
                  , replace=TRUE
                  , prob=c(0.3, 0.7))
library(MASS)
coefs <- mvrnorm(length(resp.id)
                 , mu=mu, Sigma=Sigma)
colnames(coefs) <- coef.names
coefs[carpool=="yes", "seat8"] <- coefs[carpool=="yes", "seat8"] + 2
coefs[carpool=="yes", "seat7"] <- coefs[carpool=="yes", "seat7"] + 1.5
head(cbind(carpool, coefs))
# the above creates the coefs (model) of the respondents' preferences
# it's not the final model

nques <- 15
nalt <- 3
profiles <- expand.grid(attrib)
nrow(profiles)
head(profiles)
# dummy var scheme for the attribs
profiles.coded <- model.matrix(~seat + cargo + eng + price
                               , data=profiles)[ , -1]
head(profiles.coded)

cbc.df <- data.frame(NULL)
for (i in seq_along(resp.id)) {
  profiles.i <- sample(1:nrow(profiles), size=nques*nalt)
  utility <- profiles.coded[profiles.i, ] %*% coefs[i, ]
  wide.util <- matrix(data=utility, ncol=nalt, byrow=TRUE)
  probs <- exp(wide.util) / rowSums(exp(wide.util))
  choice <- apply(probs, 1, function(x) sample(1:nalt, size=1, prob=x))
  choice <- rep(choice, each=nalt)==rep(1:nalt, nques)
  conjoint.i <- data.frame(resp.id=rep(i, nques)
                           , ques = rep(1:nques, each=nalt)
                           , alt = rep(1:nalt, nques)
                           , carpool = rep(carpool[i], nques)
                           , profiles[profiles.i, ]
                           , choice = as.numeric(choice))
  cbc.df <- rbind(cbc.df, conjoint.i)
}
# Tidy up, keeping only cbc.df and attrib
rm(a, i, resp.id, carpool, mu, Sigma, coefs, coef.names
 , conjoint.i, profiles, profiles.i, profiles.coded, utility
 , wide.util, probs, choice, nalt, nques)

library(mlogit)
cbc.mlogit <- mlogit.data(data=cbc.df, choice="choice", shape="long"
                          , varying=3:6, alt.levels=paste("pos",1:3)
                          , id.var="resp.id")

m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit)
summary(m1) # no intercept
m2 <- mlogit(choice ~ seat + cargo + eng + price, data = cbc.mlogit)
summary(m2) # intercept
# pos2:(intercept) indicates the relative preference of the second
# position in the question (versus the first)
# pos3:(intercept) indicates the
# preference for the third position (versus the first.) 
# These are sometimes called alternative specific constants or ASC's
# to differentiate them from the single intercept in a linear model.

# If we found one of these parameters to be significant, 
# that might indicate that some respondents are simply choosing
# the first or the last option without considering the question.

lrtest(m1, m2) # OK to leave out the intercept

# optionally use price as a numeric quant
m3 <- mlogit(choice ~ 0 + seat + cargo + eng +
               as.numeric(as.character(price))
            , data = cbc.mlogit)
summary(m3)
lrtest(m1, m3) # models perform the same but m3 has fewer parameters

# estimating willingness to pay
coef(m3)["cargo3ft"]/(-coef(m3)["as.numeric(as.character(price))"]/1000)
# the proper interpretation of this number is that, on average,
# customers would be equally divided 
# between a minivan with 2 ft of cargo space and
# a minivan with 3 ft of cargo space that costs $2750.60 more.

# Predicting shares
predict.mnl <- function(model, data) {
  # Function for predicting shares from a multinomial logit model
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to
  # predict shares. Same format as the data used to estimate model.
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  utility <- data.model%*%model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
}

(new.data <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ])
predict.mnl(m3, new.data)
predict.mnl(m1, new.data)


sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame containing design of competitive set
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}

base.data <- expand.grid(attrib)[c(8), ]
competitor.data <- expand.grid(attrib)[c(1, 3, 41, 49, 26), ]
(tradeoff <- sensitivity.mnl(m1, attrib, base.data, competitor.data))
barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level
        , ylab="Change in Share for Baseline Product")

# is it robust with small samples? what's the right size?
small.cbc <- mlogit.data(data=cbc.df[1:(25*15*3),]
                         , choice="choice", shape="long"
                         , varying=3:6, alt.levels=paste("pos", 1:3)
                         , id.var="resp.id")
m4 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = small.cbc)
summary(m4) # larger standard errors

# how do predictions compare?
cbind(predict.mnl(m4, new.data), predict.mnl(m1, new.data))

# creating a hierarchical model
m1.rpar <- rep("n", length=length(m1$coef))
names(m1.rpar) <- names(m1$coef)
m1.rpar

m1.hier <- mlogit(choice ~ 0 + seat + eng + cargo + price
                  , data = cbc.mlogit
                  , panel=TRUE, rpar = m1.rpar, correlation = FALSE)
summary(m1.hier)

m2.hier <- update(m1.hier, correlation = TRUE) # allows correlations to be detected
summary(m2.hier)
cov2cor(cov.mlogit(m2.hier))

predict.hier.mnl <- function(model, data, nresp=1000) {
  # Function for predicting shares of a hierarchical multinomial logit model
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to
  # predict shares. Same format at the data used to estimate model.
  # Note that this code assumes all model parameters are random
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- m2.hier$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
    }
  cbind(colMeans(shares), data)
}

predict.hier.mnl(m2.hier, data=new.data)

# bayesian method - need to reorg the data for use in a different library
library(ChoiceModelR)
choice <- rep(0, nrow(cbc.df))
choice[cbc.df[,"alt"]==1] <- cbc.df[cbc.df[,"choice"]==1,"alt"]
head(choice)
cbc.coded <- model.matrix(~ seat + eng + cargo + price, data = cbc.df)
cbc.coded <- cbc.coded[, -1] # remove the intercept
choicemodelr.data <- cbind(cbc.df[,1:3], cbc.coded, choice)
head(choicemodelr.data)
# feed in some known or assumed demographic info
carpool <- cbc.df$carpool[cbc.df$ques==1 & cbc.df$alt==1]=="yes"
carpool <- as.numeric(carpool)
choicemodelr.demos <- as.matrix(carpool, nrow=length(carpool))
str(choicemodelr.demos)

hb.post <- choicemodelr(data=choicemodelr.data, xcoding=rep(1, 7)
                         , demos=choicemodelr.demos
                         , mcmc=list(R=20000, use=10000)
                         , options=list(save=TRUE))

# examining a random draw
hb.post$compdraw[[567]]$mu
hb.post$deltadraw[567,]
hb.post$compdraw[[567]]$rooti
crossprod(hb.post$compdraw[[567]]$rooti)
str(hb.post$betadraw)
beta.post.mean <- apply(hb.post$betadraw, 1:2, mean)
head(beta.post.mean)
beta.post.q05 <- apply(hb.post$betadraw, 1:2, quantile, probs=c(0.05))
beta.post.q95 <- apply(hb.post$betadraw, 1:2, quantile, probs=c(0.95))
rbind(q05=beta.post.q05[1,], mean=beta.post.mean[1,], q95=beta.post.q95[1,])

predict.hb.mnl <- function(betadraws, data) {
  # Function for predicting shares from a hierarchical multinomial logit model
  # betadraws: matrix of betadraws returned by ChoiceModelIR
  # data: a data frame containing the set of designs for which you want to
  # predict shares. Same format at the data used to estimate model.
  data.model <- model.matrix(~ seat + eng + cargo + price, data = data)
  data.model <- data.model[,-1] # remove the intercept
  nresp <- dim(betadraws)[1]
  ndraws <- dim(hb.post$betadraw)[3]
  shares <- array(dim=c(nresp, nrow(data), ndraws))
   for (d in 1:ndraws) {
     for (i in 1:nresp) {
      utility <- data.model%*%betadraws[i,,d]
      shares[i,,d] = exp(utility)/sum(exp(utility))
    }
  }
  shares.agg <- apply(shares, 2:3, mean)
  cbind(share=apply(shares.agg, 1, mean)
        , pct=t(apply(shares.agg, 1, quantile, probs=c(0.05, 0.95)))
        , data)
}

predict.hb.mnl(hb.post$betadraw, new.data)
