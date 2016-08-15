fileLoc <- "caravanIns\\"
trainFile <- "ticdata2000.txt"
testFile <- "ticeval2000.txt"
testClassesFile <- "tictgts2000.txt"

train <- read.table(paste0(fileLoc, trainFile))
test <- read.table(paste0(fileLoc, testFile))
testClasses <- read.table(paste0(fileLoc, testClassesFile))

summary(train)

train.plotting <- train
train.plotting$purchased <- factor(train.plotting$V86
                                   , labels = c("No", "Yes"))
test.plotting <- test
test.plotting$purchased <- factor(testClasses$V1
                                  , labels = c("No", "Yes"))

#png(filename = "count_sales_fig.png")
g <- ggplot(aes(x = "Purchased\nInsurance"
                , fill = purchased)
            , data = train.plotting)
g <- g + geom_bar(position = "stack")
g <- g + myGgFillScale
g <- g + labs(x = "")
g <- g + coord_flip()
g <- g + theme_bw()
g <- g + theme(aspect.ratio = 0.1)
g
#dev.off()

fit1 <- glm(V86~., data = train, family = binomial)
pred1 <- predict(fit1, type = "response")

pred1.plotting <- data.frame(prob_purchase = sort(pred1)
                             , will_purchase = factor(sort(pred1) > 0.06
                                                      , labels = c("very unlikely"
                                                                 , "more likely"))
                             , count = 1:length(pred1))

#png(filename = "logistic_prediction_fig.png")
h <- ggplot(aes(x = count
                , y = prob_purchase
                , colour = will_purchase)
            , data = pred1.plotting)
h <- h + geom_line(size = 1.5)
h <- h + myGgColourScale
h <- h + labs(y = "Predicted\nProbability\nof Purchase"
              , x = "")
h <- h + theme_bw()
h <- h + theme(aspect.ratio = 0.1)
h
#dev.off()

# predictive value
labelPreds <- function(p) {
  factor(p
         , labels = if (sum(p * 1) == 0) {
           "No" } else {
             if (sum(p * 1) == 1) {
               "Yes" 
             } else {
               c("No", "Yes")
             }
           }
  )
}

costFunction <- function(m, cost_of_fp, cost_of_fn) {
    m$nfn * cost_of_fn + m$nfp * cost_of_fp
}

pred1 <- predict(fit1, type = "response")
pred2 <- predict(fit1, type = "response", newdata = test)

pred.factor <- rev(seq(0.001, 10, 0.001))
tprTrain <- double(length(pred.factor))
fprTrain <- double(length(pred.factor))
tprTest <- double(length(pred.factor))
fprTest <- double(length(pred.factor))
costTrain <- double(length(pred.factor))
costTest <- double(length(pred.factor))

fp_cost <- 1
fn_cost <- 2
refThreshold <- 0.8

for (i in seq_along(pred.factor)) {
  predTrain <- labelPreds(pred1 > pred.factor[i])
  predTest <- labelPreds(pred2 > pred.factor[i])

  trainConfmat <- with(train.plotting, table(predTrain, purchased))
  trainMetrics <- createMetrics(trainConfmat)
  tprTrain[i] <- trainMetrics$sensitivity
  fprTrain[i] <- trainMetrics$fpr
  costTrain[i] <- costFunction(trainMetrics, fp_cost, fn_cost)

  testConfmat <- with(test.plotting, table(predTest, purchased))
  testMetrics <- createMetrics(testConfmat)
  tprTest[i] <- testMetrics$sensitivity
  fprTest[i] <- testMetrics$fpr
  costTest[i] <- costFunction(testMetrics, fp_cost, fn_cost)
}

dataset <- rep(c("train", "test"), each = length(pred.factor))

allMetrics <- data.frame(tpr = c(tprTrain, tprTest)
                        , fpr = c(fprTrain, fprTest)
                        , cost = c(costTrain, costTest)
                        , dataset)

xyplot(tpr~fpr, groups = dataset
       , data = allMetrics
       , type = "l"
       , par.settings = MyLatticeTheme
       , scales = MyLatticeScale
       , panel = function(x, y, ...) {
         panel.abline(h = refThreshold)
         panel.abline(v = fpr[max(which(tpr < refThreshold))])
         panel.text(paste("fpr at Threshold:"
                           , round(fpr[max(which(tpr < refThreshold))], 4))
                     , x = 0.8, y = 0.2)
         panel.abline(a = 0, b = 1)
         panel.xyplot(x, y, ...)
       }
       , main = "ROC curve"
       , auto.key = list(columns = 2
                         , points = FALSE
                         , lines = TRUE)
)

g <- ggplot(data = allMetrics[allMetrics$dataset == "test",]
            , aes(x = fpr, y = tpr, colour = cost)) +
  theme_bw() +
  myGgTheme 
g + geom_line() +
  myGgColourGradient
