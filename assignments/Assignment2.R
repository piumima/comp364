## Piumi Abeynayaka ID: 260458806


setwd("~/repos/comp364")
source("~/repos/comp364/src/hucMini.R")
huc <- huc.load(dataSets="vanvliet")


## Question 1
## a)

## Obtain the IDs of patient with either good outcome or bad outcomes
good <- subset(huc$vanvliet$clinical,!event.5)[,"id"]
poor <- subset(huc$vanvliet$clinical,event.5)[,"id"]

## Matrices of gene expression for all probes for patient IDs obtained above
good.exprs <- huc$vanvliet$exprs[,good]
poor.exprs <- huc$vanvliet$exprs[,poor]

## Perform t tests on every probe to find differential expression between good and bad outcomes
t.p.values <- c()
t.index <- c()

for (i in 1:nrow(huc$vanvliet$exprs)){
  res <- t.test(good.exprs[i,], poor.exprs[i,])$p.value
  if (res <= 0.001){
    ## obtain indices of every probe with significant differential expression
    t.index <- c(t.index, i)
    ## Obtain p values of all significant results
    t.p.values <- c(t.p.values, res)
  }
}

## Find probe IDs and gene names corresponding to indices found above
t.probes <- huc$vanvliet$probe.info$probe.id[t.index]
t.genes <- huc$vanvliet$probe.info$gene.name[t.index]

## data frame of all significant results
results <- data.frame(t.index, t.probes, t.genes, t.p.values)

## b)

## sort by ascending order of p values
results.ordered <- results[order(results$t.p.values),]
## remove duplicate genes
results2 <- subset(results.ordered,!duplicated(results.ordered[,"t.genes"]))

print(results2$t.genes[1:10])


## c) Same as above but with wilcoxon test
w.p.values <- c()
w.index <- c()


for (i in 1:nrow(huc$vanvliet$exprs)){
  res <- wilcox.test(good.exprs[i,], poor.exprs[i,])$p.value
  if (res <= 0.001){
    w.index <- c(w.index, i)
    w.p.values <- c(w.p.values, res)
  }
}


w.probes <- huc$vanvliet$probe.info$probe.id[w.index]
w.genes <- huc$vanvliet$probe.info$gene.name[w.index]

w.results <- data.frame(w.index, w.probes, w.genes, w.p.values)
w.results.ordered <- w.results[order(w.results$w.p.values),]
w.results2 <- subset(w.results.ordered,!duplicated(w.results.ordered[,"w.genes"]))

print(w.results2$w.genes[1:10])

## d) Top 10 genes as identified by oth t test and wilcoxon
intersect(results2$t.genes, w.results2$w.genes)[1:10]

## e) 

## Box plot for all expression values for top 100 most significant genes for either good/bad outcomes
tmp.good <- c()
tmp.poor <- c()
for (probeID in results.ordered$t.index[1:100]){
  tmp.good <- c(tmp.good, huc$vanvliet$exprs[probeID, good])
  tmp.poor <- c(tmp.poor, huc$vanvliet$exprs[probeID, poor])
}
boxplot( tmp.good, tmp.poor, main="Expression of top 100 genes identified by t-test", names=c("Good Outcome", "Bad Outcome") )

## Same as above but based on significant results obtained with wilcoxon test
w.tmp.good <- c()
w.tmp.poor <- c()
for (probeID in w.results.ordered$w.index[1:100]){
  w.tmp.good <- c(w.tmp.good, huc$vanvliet$exprs[probeID, good])
  w.tmp.poor <- c(w.tmp.poor, huc$vanvliet$exprs[probeID, poor])
}
boxplot( w.tmp.good, w.tmp.poor, main="Expression of top 100 genes identified by Wilcoxon test", names=c("Good Outcome", "Bad Outcome") )


## Question 2

## a)
## The number of people who will probably win a trip is
30000/8192
## and the number of coin flips where there is 50% chance of no one winning is between 15 and 16
(((2^15)-1)/(2^15))^30000 # 40% chance
(((2^16)-1)/(2^16))^30000 # 63% chance

## b)

## Make a random expression df with variates from N(0,3) and plot histogram
huc$vanvliet$rand1.expr <- matrix()
huc$vanvliet$rand1.expr <- huc$vanvliet$exprs
for (i in 1:nrow(huc$vanvliet$rand1.expr)){
  huc$vanvliet$rand1.expr[i,] <- rnorm(n=ncol(huc$vanvliet$exprs), mean=0, sd=3)
}
hist(huc$vanvliet$rand1.expr)

## c)

## Randomly generated expression data corresponding to IDs of patients with good/bad outcomes
n.good.exprs <- huc$vanvliet$rand1.expr[,good]
n.poor.exprs <- huc$vanvliet$rand1.expr[,poor]

## Count how many significant results are obtained with randomized expression data
count <- 0
for (i in 1:nrow(huc$vanvliet$rand1.expr)){
  res <- t.test(n.good.exprs[i,], n.poor.exprs[i,])$p.value
  if (res <= 0.001){
    count <- count+1
  }
}
print(count)

## Number of significant results in question 1 (ie. non-random expression data)
print(length(t.index))

## d) 

## Histogram of non-randomized expression data
hist(huc$vanvliet$exprs) ## SD=0.5 seems more accurate

## Generate new randomized expression data based on SD=0.5 and count signif results as above
huc$vanvliet$rand2.expr <- matrix()
huc$vanvliet$rand2.expr <- huc$vanvliet$exprs
for (i in 1:nrow(huc$vanvliet$rand2.expr)){
  huc$vanvliet$rand2.expr[i,] <- rnorm(n=ncol(huc$vanvliet$exprs), mean=0, sd=0.5)
}
hist(huc$vanvliet$rand2.expr)

n2.good.exprs <- huc$vanvliet$rand2.expr[,good]
n2.poor.exprs <- huc$vanvliet$rand2.expr[,poor]

count <- 0
for (i in 1:nrow(huc$vanvliet$rand2.expr)){
  res <- t.test(n2.good.exprs[i,], n2.poor.exprs[i,])$p.value
  if (res <= 0.001){
    count <- count+1
  }
}
print(count)

## e)

## Count number of patients with good or bad outcomes, excluding NA results
g <- sum(!huc$vanvliet$clinical$event.5, na.rm=TRUE)
b <- sum(huc$vanvliet$clinical$event.5, na.rm=TRUE)
## Randomly select "g" indices from the event.5 to assign good outcomes and store the corresponding patient ID
r.good.index <- sample(x=1:nrow(huc$vanvliet$clinical), size=g, replace=FALSE)
r.good <- huc$vanvliet$clinical$id[r.good.index]
## Obtain remaining indices that weren't assigned to good outcome and sample "b" of them to assign bad outcomes to. Store corresponding patient ID
r.poor <- huc$vanvliet$clinical$id[sample(x=setdiff(1:nrow(huc$vanvliet$clinical),r.good.index),
                                          size=b,
                                          replace=FALSE)]

## Expression data for randomly selected groups of patients
r.good.exprs <- huc$vanvliet$exprs[,r.good]
r.poor.exprs <- huc$vanvliet$exprs[,r.poor]

## Count number of significant results for differential expression between randomized patient groups
count <- 0
for (i in 1:nrow(huc$vanvliet$exprs)){
  res <- t.test(r.good.exprs[i,], r.poor.exprs[i,])$p.value
  if (res <= 0.001){
    count <- count+1
  }
}
print(count)

## g)

## Count number of significant t-test results after applying Bonferroni correction 
count <- 0
for (i in 1:nrow(huc$vanvliet$exprs)){
  res <- t.test(good.exprs[i,], poor.exprs[i,])$p.value
  if (res <= 0.001/nrow(huc$vanvliet$exprs)){
    count <- count+1
  }
}
print(count)
