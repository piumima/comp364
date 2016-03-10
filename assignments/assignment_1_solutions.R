## COMP 364 - Tools for the Life Sciences
##
## Assignment 1 Solutions, Winter 2016
##
## Copyright (C) 2016 by Michael T. Hallett, Daniel Del Balso
##
## To make it easier for you to run the code here, we've written
## up these solutions as in an R script. Getting used to reading
## code like this won't do you any harm :-).
##
## There are many ways to solve the questions on this assignment. The solutions
## presented here are not necessarily the most efficient or 'best' methods -
## they are meant to be easily understood in the context of this course.
##
## Wherever we use loops, it would be simple to substitue them with higher order
## functions from the 'apply family' which are more efficient for reasons
## beyond the scope of this course.
##
## To practice, try re-doing the assignment using higher order map functions
## instead of loops. (e.g. apply, lapply, Map)

## Q1.

## (i) Swapping variables.
##
## The trivial solution is to use a third temporary variable to
## store one of the values.

x <- "Mike likes Justin Bieber"
y <- "Daniel is so smart"

tmp.x <- x
x <- y
y <- tmp.x

## (ii) Sampling
M <- sample( 1:10000, size = sample(1:10000, size = 1))

## Each time the script is run, M will have a randomly selected size, k,
## between 1 and 10000. I will contain a random collection of k
## numbers between 1 and 10000, selected without replacement.

## (iii) Finding the maximum

## To find the maximum value in M without using R's builtin
## max function, and making use of a loop, we can do this:

## Create a temorary variable to store the current max. We assign it
## the value 1 here since it is the smallest possible value in M.
current.max <- 1

## Iterate over M, update current.max as we go
for (i in 1:length(M)) {
  if (M[i] > current.max) {
    current.max <- M[i]
  }
}
## The value of current.max will now be the maximum value in M.
print(current.max)

## (iv) Functionaly fun

## Here it is. In case you had any doubts, this was a very easy question.
## As stated, we use our solution from (iii).

findMax <- function(myData) {

  for (i in 1:length(myData)) {
    if (myData[i] > current.max) {
      current.max <- myData[i]
    }
  }

  return(current.max)
}

## Considering the R data structures we looked at in class, what could
## myData be? A numeric vector, certainly, but what about a numeric matrix?
## Would findMax work if myData was a numeric matrix? What about if it was a
## data.frame? What if it didn't contain numeric data at all?

## Q2.

## This question should have been very easy to solve, given the examples in
## the slides and the code I (Daniel) showed you on the board in lecture.


## (i) Creating M.
##
## Recall the M already exists in our code at this point, except that
## it is a random vector of random size. The line below will overwrite
## the current value of M, which will be forgotten.

## We'll create M the same way we did in Question 1, except that now we
## want a matrix. Let's practice using sample.

M.nrow <- sample(x=2:1000, size=1)
M.ncol <- sample(x=2:1000, size=1)
M.contents <- sample(x=1:1000, size=(M.nrow * M.ncol), replace=TRUE)

M <- matrix(data=M.contents, nrow=M.nrow, ncol=M.ncol)

## (ii) Diagonal to 0.
##
## Let's use a while loop. For loops have had a lot of attention so far.
## Remember that in R indexing starts at 1, not at 0. Our loop's condition
## is 'are within the bounds of the matrix?'

## Start us off at M[1,1]
i <- 1

while (i <= M.nrow & i <= M.ncol) {

  ## Update the value in M[i,i] to 0
  M[i,i] <- 0
  ## Incremeent i
  i <- i + 1

}

## Alright, here's a for loop that does the same thing.
for (i in 1:min(M.nrow, M.ncol)) {
  M[i,i] <- 0
}


## (iii) & (iv)
##
## You were suppoed to practice with nested for loops here. For fun, let's
## solve both parts at once.

for (i in 1:M.nrow) {
  for (j in 1:M.ncol) {

    ## Here our our conditions for (iii) and (iv)
    if (i < j) {
      M[i,j] <- -1
    } else if (i > j) {
      M[i,j] <- 1
    }

  }
}

## (v) Functional fun
## Recall that from the perspective of this function, we do not
## know what the dimensinos of M are. Above we had M.nrow, and M.ncol. We can't use
## them in our function, since it must work for any matrix of unknown size. Our
## appraoch must be generalized. Fortunately, this gives us (was suppoed to give you)
## the opportunity to practice using R's builtin functions for determining
## dimensions of a matrix-like object.
##
## There are, of course, simpler and more efficient solutions possible here. Can you think of any?
## Faster? Fewer lines of code? Using less memory? More elegant?

initializeM <- function(M) {
  ## Format the input (must be a matrix), so that the main diagonal
  ## is 0s, the entries above the main diagonal are -1, and the entries
  ## below the main diagonal are 1.
  ##
  ## Args:
  ##   M : A numeric matrix.
  ##
  ## Returns:
  ##  A numeric matrix with ncol = ncol(M), nrow = nrow(M), where
  ##  the main diagonal is 0s, the entries above the main diagonal are -1,
  ##  and the entries below the main diagonal are 1.

  ## Let's verify our input, good practive.
  stopifnot(is.numeric(M) & is.matrix(M))


  ## Recall that we do not know the dimensions of M here.
  ## We can take care of (ii) - (iii) in on 'swoop'.

  ## For the sake of good style, let's avoid modifying our input arguments directly
  ret.M <- M

  ## A simple solution: nested for loops once more.
  for (i in 1:nrow(M)) {
    for (j in 1:ncol(M)) {

      ## Our three cases, (ii), (iii), (iv) in order.
      if (i == j) {
        ret.M[i,j] <- 0
      } else if (i < j) {
        ret.M[i,j] <- -1
      } else {
        ret.M[i,j] <- 1
      }

    }
  }

  return(ret.M)
}


## Q3.

## (i)-(iii) Listmania

## Let's define our list in one go. If you read the lecture slides, and remembered what we
## did in class, this was really easy. We'll throw in a few more builting functions we learned
## about. Let's assume y is a vowel. You might have to think a bit about why these solutions work.
## There are other ways, don't forget.

myList <- list(consonants=setdiff(letters, c("a", "e", "i", "o", "u", "y")),
               odd.numbers=(0:100)[which((0:100 %% 2) != 0)],
               birthdays=list(me=c(13, 3, 92)))

## Something to think about:
## We could have gottten the odd numbers with (0:100)[as.logical(0:100 %% 2)].
## Can you see why? How many ways of doing this can you think of?

## (iv) Were you LISTening?
myList.sub <- myList[c(1,3)]

## Q4.

setwd("~/repo/comp364")

source("src/hucMini.R")

## Assign the dataset names to a variable.
dataset.collection <- c("miniTCGA", "vanvliet", "nki")

## Load the datasets.
huc <- huc.load(dataset.collection)

## Print out the names of the huc object.
names(huc)

## Print out the names of all subobjects
for (i in 1:length(huc)) {
  ## For the sake of simplicity, some nice
  ## formatting. Notice the use of [ ] vs [[ ]]. Can you
  ## remember why this works?
  print(paste("Dataset:",
              names(huc[i])))
  print(names(huc[[i]]))
  ## Skip a line for nicer output
  print("")
}

## Q5.

## Initialize some empty vectors to store
## this information.
nsamples <- NULL
erp <- NULL
her2p <- NULL
good.to.bad <- NULL
erp.her2p <- NULL
ern.her2n.lyp.young <- NULL
ern.her2n.lyp.young.good <- NULL
## We can also index by names
## This is the same as the for-loop above.
for (dataset in names(huc)) {

  ## To make things easier to read,
  ## Extract the objects we need from
  ## each dataset. Note that this wastes memory.
  ## This is only here so you an see more easily.
  ## NAs are thrown away here.
  d.er <- huc[[dataset]]$clinical$er[!is.na(huc[[dataset]]$clinical$er)]
  d.her2 <- huc[[dataset]]$clinical$her2[!is.na(huc[[dataset]]$clinical$her2)]
  d.event5 <- huc[[dataset]]$clinical$event.5[!is.na(huc[[dataset]]$clinical$event.5)]

  ## Notice the use of sum(...) on vectors of booleans.
  ## Rememebr what TRUE vs FALSE are if we think of them
  ## as integers?
  ##
  ## Remember & vs && ?

  ## Number of patients (one of may ways...)
  nsamples <- c(nsamples, nrow(huc[[dataset]]$clinical))

  ## ER+
  erp <- c(erp, sum(d.er))

  ## HER2+
  her2p <- c(her2p, sum(d.her2))

  ## Ratio of good to bad outcome
  good.to.bad <- c(good.to.bad,
                   sum(!d.event5)/sum(d.event5))

  ## ER+ /\ HER2+
  erp.her2p <- c(erp.her2p,
                 sum(na.omit(huc[[dataset]]$clinical$er
                             & huc[[dataset]]$clinical$her2)))

  ## (ER- /\ HER2-) /\ (LY+ /\ age < 50)
  ern.her2n.lyp.young <- c(ern.her2n.lyp.young,
                           sum(na.omit((!huc[[dataset]]$clinical$er
                                        & !huc[[dataset]]$clinical$her2)
                                        & (huc[[dataset]]$clinical$lymph
                                           & huc[[dataset]]$clinical$age < 50))))

  ## ((ER- /\ HER2-) /\ (LY+ /\ age < 50)) /\ Good outcome
  ern.her2n.lyp.young.good <- c(ern.her2n.lyp.young.good,
                                sum(na.omit(((!huc[[dataset]]$clinical$er
                                              & !huc[[dataset]]$clinical$her2)
                                             & (huc[[dataset]]$clinical$lymph
                                                & huc[[dataset]]$clinical$age < 50))
                                            & !huc[[dataset]]$clinical$event.5)))
}
## Set names for easier interpretation.
names(nsamples) <- names(huc)
names(erp) <- names(huc)
names(her2p) <- names(huc)
names(good.to.bad) <- names(huc)
names(erp.her2p) <- names(huc)
names(ern.her2n.lyp.young) <- names(huc)
names(ern.her2n.lyp.young.good) <- names(huc)
## Print it all out and do the math...
print("Number of samples")
nsamples
print("ER+")
erp
print("HER2+")
her2p
print("good oucome : bad outcome")
good.to.bad
print("ER+ /\ HER2+")
erp.her2p
print("(ER- /\ HER2-) /\ (LY+ /\ age < 50)")
ern.her2n.lyp.young
print(" ((ER- /\ HER2-) /\ (LY+ /\ age < 50)) /\ Good outcome")
ern.her2n.lyp.young.good
