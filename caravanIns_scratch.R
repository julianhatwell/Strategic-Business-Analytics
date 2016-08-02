library(ggplot2)
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

png(filename = "count_sales_fig.png")
g <- ggplot(aes(x = "Purchased\nInsurance"
                , fill = purchased)
            , data = train.plotting)
g <- g + geom_bar(position = "stack")
g <- g + scale_fill_manual(values = c("#BBBBBB"
                                        , "#55AA88"))
g <- g + labs(x = "")
g <- g + coord_flip()
g <- g + theme_bw()
g <- g + theme(aspect.ratio = 0.1)
g
dev.off()

fit1 <- glm(V86~., data = train, family = binomial)
pred1 <- predict(fit1, type = "response")

pred1.plotting <- data.frame(prob_purchase = sort(pred1)
                             , will_purchase = factor(sort(pred1) > 0.06
                                                      , labels = c("very unlikely"
                                                                 , "more likely"))
                             , count = 1:length(pred1))

png(filename = "logistic_prediction_fig.png")
h <- ggplot(aes(x = count
                , y = prob_purchase
                , colour = will_purchase)
            , data = pred1.plotting)
h <- h + geom_line(size = 1.5)
h <- h + scale_colour_manual(values = c("#BBBBBB"
                                      , "#55AA88"))

h <- h + labs(y = "Probability\nof Purchase")
h <- h + theme_bw()
h <- h + theme(aspect.ratio = 0.1)
h
dev.off()