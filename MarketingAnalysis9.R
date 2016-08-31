library(car)
library(arules)
library(arulesViz)
data("Groceries")
summary(Groceries)
inspect(head(Groceries, 3))

groc.rules <- apriori(Groceries
                      , parameter=list(supp=0.01
                                       , conf=0.3
                                       , target="rules"))

inspect(subset(groc.rules, lift > 3))

retail.raw <- readLines("http://fimi.ua.ac.be/data/retail.dat")
# retail.raw <- readLines("http://goo.gl/FfjDAO") # alternative link
head(retail.raw)
tail(retail.raw)
summary(retail.raw)
retail.list <- strsplit(retail.raw, " ")
rm(retail.raw)
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")
some(retail.list)
str(retail.list)
retail.trans <- as(retail.list, "transactions")
# takes a few seconds
summary(retail.trans)

retail.rules <- apriori(retail.trans
                        , parameter=list(supp=0.001
                                         , conf=0.4))

plot(retail.rules)
plot(retail.rules, interactive=TRUE) # oh my goodness

retail.hi <- head(sort(retail.rules, by="lift"), 50)
inspect(retail.hi)

plot(retail.hi
     , method="graph"
     , control=list(type="items"))

# simulated margin data
retail.itemnames <- sort(unique(
  unlist(as(retail.trans, "list"))))
head(retail.itemnames); tail(retail.itemnames)
set.seed(03870)
retail.margin <- data.frame(margin=rnorm(
  length(retail.itemnames)
  , mean=0.30, sd=0.30))
quantile(retail.margin$margin)

rownames(retail.margin) <- retail.itemnames
head(retail.margin); tail(retail.margin)
retail.margin[c("39", "48"), ]
sum(retail.margin[c("39", "48"), ])

(basket.items <- as(retail.trans[3], "list")[[1]])
retail.margin[basket.items, ]
sum(retail.margin[basket.items, ])

# a generic function for finding the margins
retail.margsum <- function(items, itemMargins) { 
  # Input: "items" == item names, 
  # rules or transactions in arules format 
  # "itemMargins", a data frame of profit 
  # margin indexed by name 
  # Output: look up the item margins, 
  # and return the sum
  # check the class of "items" and 
  # coerce appropriately to an item list
  if (class(items) == "rules") { 
    tmp.items <- as(items(items), "list") # rules ==> item list 
    } else if (class(items) == "transactions") {
      tmp.items <- as(items, "list") # transactions ==> item list
    } else if (class(items) == "list") { 
      tmp.items <- items # it’s already an item list ! 
    } else if (class(items) == "character") { 
      tmp.items <- list(items) # characters ==> item list 
    } else { 
      stop("Don’t know how to handle margin for class "
           , class(items)) 
    } # make sure the items we found are 
      # all present in itemMargins 
  good.items <- unlist(lapply(tmp.items
                              , function (x) { all(unlist(x) %in% rownames(itemMargins))}
                              )
                       )
  if (!all(good.items)) { 
    warning("Some items not found in rownames of itemMargins. "
            , "Lookup failed for element(s):\n"
            , which(!good.items)
            , "\nReturning only good values.")
    tmp.items <- tmp.items[good.items]
    }
  # and add them up
  return(unlist(lapply(tmp.items, function(x) sum(itemMargins[x, ]))))
}

retail.margsum(c("39", "48"), retail.margin)
retail.margsum(list(t1=c("39", "45"), t2=c("31", "32")), retail.margin)
retail.margsum(retail.trans[101:103], retail.margin)
retail.margsum(retail.hi, retail.margin)

# exploring consumer data - segments
seg.df <- read.csv("http://goo.gl/qw303p") # same as chap 5
seg.fac <- seg.df
seg.fac$age <- cut(seg.fac$age, 
                   breaks=c(0,25,35,55,65,100)
                   , labels=c("19-24", "25-34"
                              , "35-54", "55-64", "65+")
                   , right=FALSE, ordered_result=TRUE)

summary(seg.fac$age)
seg.fac$income <- cut(seg.fac$income
                      , breaks=c(-100000, 40000, 70000, 1000000)
                      , labels=c("Low", "Medium", "High")
                      , right=FALSE, ordered_result=TRUE)
seg.fac$kids <- cut(seg.fac$kids
                    , breaks=c(0, 1, 2, 3, 100)
                    , labels=c("No kids", "1 kid"
                               , "2 kids", "3+ kids")
                    , right=FALSE, ordered_result=TRUE)
summary(seg.fac)
seg.trans <- as(seg.fac, "transactions")
summary(seg.trans)

seg.rules <- apriori(seg.trans
                     , parameter=list(support=0.1
                                      , conf=0.4
                                      , target="rules"))
summary(seg.rules)
plot(seg.rules)
plot(seg.rules, interactive=T)

seg.hi <- head(sort(seg.rules, by="lift"), 35)
inspect(seg.hi)
plot(seg.hi, method="graph", control=list(type="items"))
seg.next <- sort(seg.rules, by="lift")[36:60]
plot(seg.next, method="graph", control=list(type="items"))

