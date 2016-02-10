#myList <- list(letters,seq(0,100,2),list("09,08,1992","05,01,1986"))

#list of consonants without vowels
myLetters <- setdiff(LETTERS,c("A","E","I","O","U"))
myLetters <- LETTERS
vowels <- c("A","E","I","O","U")
for (a in vowels){
  myLetters <- myLetters [which(myLetters !=a)]
}
q1 <- list(myLetters)

#even numbers from 0-100
q2 <- seq(from=0,to=100,by=2)
q2 <- (0:50)*2
q2 <- c()
for (i in 0:100){
  if (i %% 2 == 0){
    q2<- c(q2,i)
  }
}
q2 <- c()
for (i in 0:100){
  if (ceiling(i/2)==floor(i/2)){
    q2 <- c(q2,i)
  }
}

#list of sibling birthdays
b1 <- c(08,09,92)
b2 <- c(05,01,86)
q3 <- list(b1,b2)


myLists<-list(q1,q2,q3)
