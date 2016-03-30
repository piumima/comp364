# COMP-364 T4LS Winter 2016
# M Hallett, M Ghadie
# Solutions to Assignment #2.



# Question 1


# Some preliminaries....

source("~/repos/comp364/src/hucMini.R")
setwd('~/repos/comp364')
dataset.collections <- c("vanvliet")
huc <- huc.load(dataSets = dataset.collections, dataDir = "data")
names(huc)

# Q 1a.

good.outcome.indices <- which(huc$vanvliet$clinical$event.5 == FALSE)     # the indices into the clinical matrix of the patients with good outcome; need this later for t-test.
bad.outcome.indices <-  which(huc$vanvliet$clinical$event.5 == TRUE) 

ttest.res <- list()
for (i in 1:length(exprs[,1])) {    # for each row (probe) ...

  good.exprs <- huc$vanvliet$exprs[i, good.outcome.indices]
  bad.exprs <- huc$vanvliet$exprs[i, bad.outcome.indices]
  ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=FALSE )
  
  # for question 2c
  # ttest.res[[i]] <- wilcox.test( good.exprs, bad.exprs )
  }

# So now ttest.res[[i]] holds the results (a data.frame) from the t.test function for probe i.
# Now find those probes with a t.test with p.value < 0.001

sig <- lapply( ttest.res, FUN= function(x) { return(x$p.value < 0.001) })
sig.indices <- which(sig == TRUE)
length(sig.indices)
# You could do this with a for loop too... you would just go through and test each element  i in 1:length(ttest.res)) ttest.res[[i]]$p.value < 0.001


# Q 1b.

# Here's the names for all the probes that were found to be significant
unique( huc$vanvliet$probe.info$gene.name[sig.indices] )

# But we want the "top ten" (lowest 10 pvalues with unique gene names)
# first sort the list of all probes. to do this, let's create vector of pvalues to make it simpler than using lists.

my.pvalues <- unlist(lapply( ttest.res, FUN= function(x) { return(x$p.value) }))
# again you could do this with a simple for loop but I show you the lapply function ...

# Now the ith value of my.pvalues is the p.value returned from the t.test function for probe i.

order.pvalues <- order( my.pvalues, decreasing = FALSE )

# let's do a reality check
order.pvalues[1]
my.pvalues[5425]
min(my.pvalues)
# all ok.

my.gene.names <- c()
i <- 1
while (length(my.gene.names) < 10) {
  my.gene.names <- union(my.gene.names, huc$vanvliet$probe.info$gene.name[order.pvalues[i]])
  i <- i + 1
  }

# the while loop only terminates when 10 distinct gene names have been added to the list
# note that union() removes duplicates e.g. union( c(1, 2), c(2, 3) ) == c(1,2,3)

# [1] "CCNB2"    "PGK1"     "CX3CR1"   "CCT5"     "UBE2S"    "CCNB1"    "C16orf61" 
# "KIF20A"   "CDKN3"    "CBX7"


# Question 1c

# I would simply cut and paste the entire solution to Questions 1 a and b. 
#Now replace the list that is  
#     ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=FALSE )
# with
   ttest.res[[i]] <- wilcox.test( good.exprs, bad.exprs )

#Ok admittedly, it's a bit weird to save the results in ttest.res but 
 for an assignment that's ok... :-)

# so now after running the code for the wilcoxon i can answer Question 2d and 2e easily. 

my.gene.names
# [1] "CCNB2"    "CX3CR1"   "KIF20A"   "C16orf61" "STAT5B"   "CCNB1"    "UBE2S"    
# "KIF4A"    "H2AFZ"   "CCT5"   

wilcox.gene

# Question 1d
   
   intersect(t.test.genes, my.gene.names)

CCNB2, CX3CR1, CCT5, UBE2S, CCNB1

looking in GeneCards to generate some bio-babble:

CCNB1 and CCNB2 - cyclin B1 and B2; cell cycle control. A lost of regulation of cell cycle is a key hallmark
of cancer.
CX3CR1 - chemokine receptor involved in cell adhesion. Loss of cell adhesion is a necessary precursor
  to cell migration, extra-vascularization and eventually metastatic disease.
CCT5 - beats me! No idea except my guess is that this a stress related gene; cancer cells are often 
envionmental constraints (eg hypoxia) and also stress due to the folding process of aberrant 
transcripts. But beats me generally... Any one ele have a good idea?
UBE2S - ubiquitin-conjugating enzyme. It has known links to immune functioning and DNA damage, two 
central hallmarks of cancer. 

# Question 1e


After doing Part a, I would do something like
   t.test.genes <- my.gene.names
   t.test.my.pvalues <- my.pvalues
   t.test.order.pvalues <- order.pvalues


After doing Par b, I would do something like

   wilcox.genes <- my.gene.names
   wilcox.my.pvalues <- my.pvalues
   wilcox.order.pvalues <- order.pvalues

Now I have everything side by side to do this.

def.par <- par( no.readonly = TRUE )
   par( mfrow = c(1,2) )
   par( oma = c( 5, 0, 3, 0 ))
   
boxplot(
    as.vector(huc$vanvliet$exprs[ t.test.order.pvalues[1:100],  good.outcome.indices]), 
    as.vector(huc$vanvliet$exprs[ t.test.order.pvalues[1:100],  bad.outcome.indices]),
    names=c("Good Outcome", "Bad Outcome"),
    main=c("t test") )

boxplot(
    as.vector(huc$vanvliet$exprs[ wilcox.order.pvalues[1:100],  good.outcome.indices]), 
   as.vector(huc$vanvliet$exprs[ wilcox.order.pvalues[1:100],  bad.outcome.indices]),
   names=c("Good Outcome", "Bad Outcome"),
   main=c("wilcoxon") )
   
# Question 1f

I'm suprised at home much agreement there is between the two methods. 
In fact, if you go back to question 2a and choose "variances equal", you'll see more
   differences I think. With this many patients in my opinion and the fact that expression 
   data is pretty normally distributed (the t-test can be used), I think both worked.

   
   
   
   
   

# Question 2

# Q 2a
   
There are 2^13 = 8192 possible sequences of heads and tails: HHHH...H, HHHHH..HT,, 
HHHHH...TH, ..., TTTTT.....TT. 

so if there are 30,000 spectators, then we might expected roughly that 30,0000/8192
 would win. So about 3 or 4.

For the second part of the question....

Ok to get a handle on this, observe that 2^15 = 32768 which is approximately equal
to 30,000 spectators at the game.

Imagine there was only 1 coin. So the hidden outcome would
be a single coin toss (heads or tails). 
That is, there are two possible outcomes:
2^1. What would be the probability that the 
one spectator gets it: 50/50.

Imagine 2 coins. So there are four (2^2) possible outcomes: HH, TT, HT, TH. If there 
are 2 (=2^1) spectators (two attempts (trials) to throw  two coins), someone would be
expected to win 50% of the time.

Imagine 3 coins. HHH, HHT, HTH, ... TTT a total of 2^3 = 8 outcomes. If there
were 4 (2^2), someone would be expected to win 50% of the time.

4 coins, 2^4 = 16 outcomes, 2^3 =8 spectators
5 coins, 2^5 = 32 outcomes, 2^4 =16 spectators
k coins, 2^k outcomes, 2^(k-1) spectators

So if we have 2^15 spectators, then when k is approximately 16, it would be expected that someone
wins 50% of the time.

# Q 2b

detach(huc$vanvliet)

huc$vanvliet$rand1.exprs <- matrix( data = rnorm( n = ncol(huc$vanvliet$exprs) * 
                                            nrow(huc$vanvliet$exprs), 0, 3),
                                   nrow = nrow(huc$vanvliet$exprs),
                                   ncol = ncol(huc$vanvliet$exprs))
hist(huc$vanvliet$rand1.exprs)


# Q 2c

# Copying the code from 1a and change exprs to my.exprs
my.exprs <- huc$vanvliet$rand1.exprs

ttest.res <- list()
for (i in 1:length(my.exprs[,1])) {    # for each row (probe) ...

  good.exprs <- my.exprs[i,][good.outcome.indices]
  bad.exprs <- my.exprs[i,][bad.outcome.indices]
  ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=TRUE )
}

sig <- lapply( ttest.res, FUN= function(x) { return(x$p.value < 0.001) })
sig.indices <- which(sig == TRUE)
length(sig.indices)
# 22 versus 2233 signicant in Question 1a
# The 2 that are identified here are just by random chance. 
# Note that your number might be different each time you run this because
# the data is randomly generated in rand1.exprs: All the expression values whether
# for good or bad outcome patients are just random numbers from N(0,3).
# The fact that there are over 2000 in Question 1a suggestions that the mRNA expression
# data does have genes that are differentially expressed between good and bad outcome patients.



# Q 2d


hist(huc$vanvliet$exprs, col="#FF000066")  # see through reddish (may look gold to some people)

# maybe N(0,1)?

hist(rnorm(n=ncol(huc$vanvliet$exprs) * nrow(huc$vanvliet$exprs), 0, 1), add=TRUE, col="#0000FF66")  # see through bluish (may look white)

# maybe N(0,0.5) ?
# clear your plot window
hist(huc$vanvliet$exprs, col="#FF000066")  # see through reddish (may look gold to some people)
hist(rnorm(n=ncol(huc$vanvliet$exprs) * nrow(huc$vanvliet$exprs), 0, 0.5), add=TRUE, col="#0000FF66")  # see through bluish (may look white)


# let's just copy the previous code and change rand1 to rand2;
# then set my.exprs to point to rand2  instead of rand1 and we're good to go.
huc$vanvliet$rand2.exprs <- matrix( data = rnorm( n = ncol(huc$vanvliet$exprs) * nrow(huc$vanvliet$exprs), 0, 0.5),
                                    nrow = nrow(huc$vanvliet$exprs),
                                    ncol = ncol(huc$vanvliet$exprs))
my.exprs <- huc$vanvliet$rand2.exprs
#################


ttest.res <- list()
for (i in 1:length(my.exprs[,1])) {    # for each row (probe) ...

  good.exprs <- my.exprs[i,][good.outcome.indices]
  bad.exprs <- my.exprs[i,][bad.outcome.indices]
  ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=TRUE )
}

sig <- lapply( ttest.res, FUN= function(x) { return(x$p.value < 0.001) })
sig.indices <- which(sig == TRUE)
length(sig.indices)

# ok so 20 are observed to be significant with N(0, 0.5) as the background.
# but this number might change slighly each time you run teh code.

# It's basically the same as with N(0,3). It's all just random, so 16, 22, ... genes are identified as
# differential between good and bad by chance. Nothin' goin' on except pure randomness in the universe.


# Q 2 e

# Maybe the easiest way to do this is simply to permute the 947 patients.
# Try this.

patient.mixer.upper <- sample( 947, size=947, replace = FALSE )

# see it's just a permutation of the numbers form 1 to 947
(patient.mixer.upper)

# e.g.  [1] 775 319 805 654 820 206 348 802 787 ... (all 947 will appear exactly once)
# Note that each time you run the sample() function above, this permutaion will change.

# The idea is that the i^th patient takes the expression of the value in patient.mixer.upper[i]
# In the exmaple above the expression of patient 1 is replaced by the expression of patient 775.

# Ok, let's make this more concrete. We are going to create huc$vanvliet$rand3.exprs
# Let's copy the original first
huc$vanvliet$rand3.exprs <- huc$vanvliet$exprs

for (i in 1:length(huc$vanvliet$rand3.exprs[1,])) {  # determine the number of columns
  huc$vanvliet$rand3.exprs[,i] <- huc$vanvliet$exprs[,patient.mixer.upper[i]]
  }

head(huc$vanvliet$rand3.exprs[,1])
# [1] -0.12 -0.26 -0.05 -0.25  0.37 -0.29

(patient.mixer.upper[1])
# 775

head(huc$vanvliet$exprs[,775])
#[1] -0.12 -0.26 -0.05 -0.25  0.37 -0.29


# Q 2 f

my.exprs <- huc$vanvliet$rand3.exprs
#################


ttest.res <- list()
for (i in 1:length(my.exprs[,1])) {    # for each row (probe) ...

  good.exprs <- my.exprs[i,][good.outcome.indices]
  bad.exprs <- my.exprs[i,][bad.outcome.indices]
  ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=TRUE )
}

sig <- lapply( ttest.res, FUN= function(x) { return(x$p.value < 0.001) })
sig.indices <- which(sig == TRUE)
length(sig.indices)

# well at least with my patient.mixer.upper, I got 4.
# let me try it again.... my second time gave me 9.
# a third time gave me 7

# I think that all of the background distributions are basically doing about
# the same: any genes that show up as significant are there by chance
# like guessing the sequence of coin tosses if given infinitely long.
# Interestingly, at a p-value of 0.001, you would expect 1 in 1000 to be significant
# by chance.
# There are ~22,000 probes on vanvliet. Therefore we'd expect about
# 22 significant by chance. Which is generally what we see for all but step 3d where
# the permutation test was slightly lower.
# All are very small in comparison to the observed number of > 2000 genes.

# Q 2 g
length(huc$vanvliet$exprs[,1])
# 22268

# So there are 22,268 tests. Using Bonferroni, we would divide our original p-value 0.001
# by this number giving us an adjusted p-value of 4.490749e-08
(adj.pvalue <- 0.001 / length(huc$vanvliet$exprs[,1]))

#### Same as Question 1a
ttest.res <- list()
for (i in 1:length(exprs[,1])) {    # for each row (probe) ...

  good.exprs <- exprs[i,][good.outcome.indices]
  bad.exprs <- exprs[i,][bad.outcome.indices]
  ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=TRUE )
}


# Ok change 0.001 to adj.pvalue
sig <- lapply( ttest.res, FUN= function(x) { return(x$p.value < adj.pvalue) })
sig.indices <- which(sig == TRUE)
length(sig.indices)

# 186

#It turns out there are only 186 probes signficiant after this adjustment (down from > 2200).
# 186 of 22268 means that less than 1% of the probes are siginficant after adjusting for multiple
# testing in this manner.

