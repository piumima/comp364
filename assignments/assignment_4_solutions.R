## COMP 364 : Assignment 4 - Solutions.
## Winter 2016
## Michael T. Hallett, Daniel Del Balso

## Question 1

## First, let's clearly label all of the parameters we have to play with.
total.probes <- 20000

student.a.significant <- 100
student.b.significant <- 250
student.c.significant <- 750

intersection.ab <- 10
intersection.ac <- 20
intersection.bc <- 10


## (a)
## Note that using the binomial distribution here implies that we are allowing replacement.
## In other words, after each trial, we can put the ball back in the bag.

## To frame this in terms of the in-class discussion (bag, red balls, white balls)
## and to help you practice for the final, here's a function.
binomial.test <- function(nred, bag.total, trials, successes) {
  ## Is the number of successes more than expected by chance? Return
  ## A p-value using the binomial test.
  ##
  ## Args:
  ##   nred       : Number of red balls
  ##   bag.total  : Number of balls in the bag
  ##   successes  : Number of successes we witness
  ##   trials     : Total number of trials
  ##
  ## Returns:
  ##   The result of a binomial test given the parameters
  ##   described above. Only returns the p-value.

  ## The probability of randomly selecting a red ball, is then
  probability.red <- nred / bag.total

  ## Now, we can perform the binomial test.
  return(binom.test(x=successes, n=trials, p=probability.red,
                    alternative="two.sided")$p.value)
}

## For each of the intersections described in the assignment, one student defines
## 'the bag' while the other defines the number of trials. For example, let's consider
## the intersection for students A, B.
##
## Student A found 100 significant probes out of 20'000. Let this be the number of
## red balls: 100 red balls, 20'000 - 100 = 19900 white balls, 20'000 balls in the bag.
## Now we can reason about that intersection this way: student B pulled 250 balls out
## of the bag, 10 are red. Is this significant? Using our helper function, let's check...
print(paste("A vs B : p =",
            binomial.test(nred = student.a.significant,
                    bag.total = total.probes,
                    trials = student.b.significant,
                    successes = intersection.ab)))

## Now, we can repeat for the other students (who is the weakest link?)
print(paste("A vs C : p =",
            binomial.test(nred = student.a.significant,
                          bag.total = total.probes,
                          trials = student.c.significant,
                          successes = intersection.ac)))

print(paste("B vs C : p =",
            binomial.test(nred = student.c.significant,
                          bag.total = total.probes,
                          trials = student.b.significant,
                          successes = intersection.bc)))

## (b)
## Sacking. Remember, you're a hotshot scientist, who got where he/she is by talking-up/believing
## in the voodoo of actually-rather-meaningless p-values...
## Statisticians? What do they know? Bunch of downers! p < 0.01 is all we care about and (mis-)understand.
## Statisticians, mathematicians, and computer scientists are to us what people who read books/newspapers
## are to Donald Trump. We are the Trump of science. Make p-values great again. n
##
##
## We know that all the students are looking for probes differentially expressed between good/poor outcome.
## We expect that their results should agree for the most part, unless one of them chooses a wonky procedure.
## With that in mind, the evidence we have suggests that we cannot rule out that the number of probes
## observed to be common between B-C may be down to chance alone. The A-C comparison seems to be the
## least likely to have occured by chance, so we trust that these two students may be onto something, and
## seem to have an interesting, common result. B, on the other hand, is in trouble.


## Question 2
## The hypergeometric: This time we don't allow replacement.

## Let's use dhyper
print(paste("A vs B - Hypergeometric : p =",
            dhyper(x=intersection.ab,
                   m=student.b.significant,
                   n=total.probes - student.a.significant,
                   k=student.b.significant)))

print(paste("A vs C - Hypergeometric : p =",
            dhyper(x=intersection.ac,
                   m=student.a.significant,
                   n=total.probes - student.a.significant,
                   k=student.c.significant)))

print(paste("B vs C - Hypergeometric : p =",
            dhyper(x=intersection.bc,
                   m=student.b.significant,
                   n=total.probes - student.b.significant,
                   k=student.c.significant)))

## Once more, B is sacked. Fortunately a bright future awaits him/her.

## Question 3
## See the PDF. I won't make you suffer through plaintext.
