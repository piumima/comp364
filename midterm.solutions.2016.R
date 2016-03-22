(x <- 5)
x <- 6
(x <- x*2)

2*(1:5)

( FALSE && ( (!TRUE | !!!!TRUE) | (!!!!FALSE | !!TRUE) ) )
#  the first part of the AND is false, so everything is false.
#  no baby

for (i in c("hey", "ho", "schmo", "flow") ) {
  if (i == 4) {
    print(i) }
}

# i is never equal to 4. at the fourth iteration it will be
# equal to "flow"

x <- 7
myFunction <- function( x = 3 ) {
  x <- 6
  return(x*3)
}
x <- 8
print(myFunction(4))
print(x)

a <- LETTERS[1:5]
b <- 1:5
c <- TRUE
d <- b
e <- list( a, b, list(c), d )
e[4]
e[[4]]

e[2:4]
e[[2:4]]

myGrades <- factor(  c("A", "F", "K", "B", "A", "K", "C") )
myGrades


randomCoinFlips <- function( 	flips = 10,	prob.heads = 1 ) {
  return( sample( 	x = c("H", "T"), 
                   size = flips, 
                   replace = TRUE,
                   prob=c(prob.heads, 1 - prob.heads) ) ) }

randomCoinFlips()
# all heads because prob of tails is 0
#[1] "H" "H" "H" "H" "H" "H" "H" "H" "H" "H"


aa <- LETTERS[1:5]
bb <- 1:5
cc <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
dd <- data.frame( letterz = aa, numbs = bb, truthity = cc )

which(dd$truthity)
# which gives back the indices of the rows for which the 
# condition truthity is TRUE
#1 2 4

subset(dd, truthity)
# subset gives bach the actual rows for which the condition 
# truthity is TRUE
letterz numbs truthity
#1       A     1     TRUE
#2       B     2     TRUE
#4       D     4     TRUE


# Question 2.

convertToWC <- function( target.dna, five.prime = TRUE ) {
  
  nucleotides <- c("A", "C", "G", "T")
  output.dna <- ""  # this is what we will return at the end.
  
  for (i in 1:nchar(target.dna)) {
        tmp <- substr(toupper(target.dna), start=i, stop = i)
        
        if (tmp=='A') out.char <- 'T'
        else if (tmp=='T') out.char <- 'A'
        else if (tmp=='C') out.char <- 'G'
        else if (tmp=='G') out.char <- 'C'
        else out.char <- 'X'

        if (five.prime) { 
          output.dna <- paste(out.char, output.dna, sep = "") }
        else {
          output.dna <- paste(output.dna, out.char, sep = "") }
      
  } #end of for loop i
  return(output.dna)
}

# Question 3

source("~/repos/comp364/src/hucMini.R")
huc <- huc.load(dataSets = "vanvliet",  dataDir = "~/repos/comp364/data")
attach(huc$vanvliet)

gene.names <- unique(probe.info$gene.name)

keep.count <- vector( mode="integer", length = length(gene.names) )
# initialized to 0s by default 

for (i in 1:length(gene.names)) {
    keep.count[i] <- length( which( 
      probe.info$gene.name == gene.names[i]))
}
print(probe.info$gene.name[which(keep.count == max(keep.count))])

# part (c)

for (i in 1:length(which(keep.count==2))) {
  probes <- which(probe.info$gene.name == 
                    gene.names[restrict.to.two[i]])
  print(  which.max( exprs[probes[1],] - exprs[probes[2],] ) )
}
