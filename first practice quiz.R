# first practice quiz
pasta <- read.csv("PASTAPURCHASE.csv")
summary(pasta)
sd(pasta$PASTA)

unique(pasta[pasta$INCOME == min(pasta$INCOME), "AREA"])
unique(pasta[pasta$INCOME == max(pasta$INCOME), "AREA"])

max(tapply(pasta$PASTA, pasta$HHID, sum))

mean(pasta[pasta$AREA == 4, "INCOME"])

pasta2 <- pasta[pasta$AREA == 2 & pasta$INCOME > 20000,]
p2 <- tapply(pasta2$PASTA, pasta2$HHID, sum)
length(p2[p2 > 30])

hist(tapply(pasta$PASTA, pasta$HHID, sum))

pasta.ts <- tapply(pasta$PASTA, pasta$TIME, sum)
plot(pasta.ts)
