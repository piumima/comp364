M <- matrix(ncol=10, nrow=10 )

for (i in 1:(nrow(M)-1)) {
  
  for (j in (i+1):ncol(M)) {
    
      M[i,j] <- TRUE; 
  }
  
}  
for (i in 2:nrow(M)) {
  
  for (j in 1:(i-1)) {
    
    M[i,j] <- FALSE; 
  }
  
}  

# February 9, 2016
# Crazy lists

letterA <- which( LETTERS == "A" )
letterE <- which( LETTERS == "E" )

myLetters <- LETTERS
myLetters <- myLetters[ which( myLetters != "A") ]
myLetters <- myLetters[ which( myLetters != "E") ]

myLetters <- setdiff( LETTERS, c("A", "E", "I") )

q1 <- setdiff( LETTERS, c("A", "E", "I") ) 
  
q2 <- seq(from=0, to=100, by=2)

b1 <- c(11, 11, 11)
b2 <- c(12, 12, 12)
q3 <- list( b1, b2 
           )

myLists <- list( q1, q2, q3 )

myLists[1]
myLists[[1]]







