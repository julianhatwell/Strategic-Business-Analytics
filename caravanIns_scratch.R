fileLoc <- "caravanIns\\"
trainFile <- "ticdata2000.txt"
testFile <- "ticeval2000.txt"
testClassesFile <- "tictgts2000.txt"

train <- read.table(paste0(fileLoc, trainFile))
test <- read.table(paste0(fileLoc, testFile))
testClasses <- read.table(paste0(fileLoc, testClassesFile))

summary(train)

fit1 <- glm(V86~., data = train, family = binomial)
pred1 <- predict(fit1, type = "response")

table(train$V86, 1 * pred1 > 0.181)

