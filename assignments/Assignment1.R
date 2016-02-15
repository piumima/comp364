## Piumi Abeynayaka ID 260458806

## ====================================================================
## QUESTION 1

## i)
(x <- sample(1:100, size=1))
(y <- sample(1:100, size=1))
x1 <- x
x <- y
y <- x1
(x);(y)



## ii)
## M is a vector of randomly sampled numbers from 1 to 10000.
## The size of M (ie. the number of numbers in it) is also 
## randomly chosen from  a number between 1 to 10000.
M <- sample(1:10000, size=sample(1:10000, size=1))



## iii)
maxVal <- M[1]
for (i in 2:length(M)){
  if (M[i]>maxVal){
    maxVal <- M[i]
  }
}
maxVal



## iv)
findMax <- function(myData){
  maxVal <- myData[1]
  for (i in 2:length(myData)){
    if (myData[i]>maxVal){
      maxVal <- myData[i]
    }
  }
  return(maxVal)
}

## ====================================================================
## QUESTION 2

## i)
n <- sample(1:20, size=1)
m <- sample(1:20, size=1)
M <- matrix(nrow=n, ncol=m)



## ii)
smallerDim <- ifelse((ncol(M)<=nrow(M)), yes = ncol(M), no = nrow(M))
for (i in 1:smallerDim){
  M[i,i] <- 0
}



## iii) and iv)
for (i in 1:nrow(M)){
  for (j in 1:ncol(M)){
    if (i<j){
      M[i,j] <- -1
    }
    if (i>j){
      M[i,j] <- 1
    }
  }
}

initializeM <- function (M){
  smallerDim <- ifelse(ncol(M)<=nrow(M), yes = ncol(M), no = nrow(M))
  for (i in 1:smallerDim){
    M[i,i] <- 0
  }
  for (i in 1:nrow(M)){
    for (j in 1:ncol(M)){
      if (i<j){
        M[i,j] <- -1
      }
      if (i>j){
        M[i,j] <- 1
      }
    }
  }
  return (M)
}

## ====================================================================
## QUESTION 3

## i)
q1 <- as.list(setdiff(letters,c("a","e","i","o","u")))



## ii)
q2 <- as.list(seq(from=1, to=100, by=2))



## iii)
q3 <- list(c(09,08,92),c(05,01,86))

myList <- list(q1,q2,q3)



## iv)
myList[c(1,3)]

## ====================================================================
## QUESTION 4

setwd("~/repos/comp364")
source("~/repos/comp364/src/hucMini.R")
dataset.collection <- c("miniTCGA", "nki", "vanvliet")
huc <- huc.load(dataSets=dataset.collection)
names(huc)
for (dataset in dataset.collection){
  print(names(huc[[dataset]]))
}

## ====================================================================
## QUESTION 5

## Data frame for answers to Question 5
Q5 <- data.frame()

## a) CALCULATE NUMBER OF PATIENTS

number.of.patients <- c()
for (dataset in dataset.collection){
  number.of.patients <- c(number.of.patients,
                          length(huc[[dataset]][["clinical"]][["id"]]))
}

number.of.patients[length(dataset.collection)+1] <- sum(number.of.patients)

Q5 <- rbind(number.of.patients)



## b) ER+ FRACTION IN EACH DATASET
## AND
## d) ER+ FRACTION IN ALL DATASETS

er.plus <- c()
for (dataset in dataset.collection){
  er.plus <- c(er.plus, length(subset(huc[[dataset]][["clinical"]], er)[,1]))
}

er.plus[length(dataset.collection)+1] <- sum(er.plus)

Q5 <- rbind(Q5,er.plus/number.of.patients)



## c) GOOD TO POOR OUTCOMES RATIO FOR EACH DATASET

good.poor.string <- c()
good.poor.numeric <- c()
for (dataset in dataset.collection){
  good <- length(subset(huc[[dataset]][["clinical"]],!event.5)[,1])
  poor <- length(subset(huc[[dataset]][["clinical"]],event.5)[,1])
  good.poor.string <- c(good.poor.string, paste(good,":",poor, sep=""))
  good.poor.numeric <- c(good.poor.numeric, good/poor)
}

Q5 <- rbind(Q5,c(good.poor.string,NA))
Q5 <- rbind(Q5,c(good.poor.numeric,NA))



## e) HER2+ FRACTION

her2 <- c()
for (dataset in dataset.collection){
  her2 <- c(her2,length(subset(huc[[dataset]][["clinical"]],her2)[,1]))
}

her2[length(dataset.collection)+1] <- sum(her2)

Q5 <- rbind(Q5,her2/number.of.patients)



## f) ER+ & HER2+

er.and.her2 <- c()
for (dataset in dataset.collection){
  er.and.her2 <- c(er.and.her2,
                   length(subset(huc[[dataset]][["clinical"]],
                                 (her2 & er))[,1])
                   )
}

er.and.her2[length(dataset.collection)+1] <- sum(er.and.her2)

Q5 <- rbind(Q5,er.and.her2/number.of.patients)



## g) ER- & HER2- & LYMPH+ & <50 YEARS OLD

er.her.lymph.age <- c()
for (dataset in dataset.collection){
  er.her.lymph.age <- c(er.her.lymph.age,
                        length(subset(huc[[dataset]][["clinical"]],
                                      (!er & !her2 & lymph & age<50))[,1])
                        )
}

er.her.lymph.age[length(dataset.collection)+1] <- sum(er.her.lymph.age)

Q5 <- rbind(Q5,er.her.lymph.age/number.of.patients)



## h) ER- & HER2- & LYMPH+ & <50 YEARS OLD & NO EVENT AT 5 YEARS

er.her.lymph.age.event <- c()
for (dataset in dataset.collection){
  er.her.lymph.age.event <- c(er.her.lymph.age.event, 
                              length(subset(huc[[dataset]][["clinical"]],
                                            (!er & !her2 & lymph & age<50 & !event.5))[,1])
                              )
}

er.her.lymph.age.event[length(dataset.collection)+1] <- sum(er.her.lymph.age.event)

Q5 <- rbind(Q5,er.her.lymph.age.event/number.of.patients)

colnames(Q5) <- c(dataset.collection,"all")
rownames(Q5) <- c("Number of patients", 
                  "ER+ fraction", 
                  "Ratio of good:poor outcomes",
                  "Numeric ratio of good:poor outcomes",
                  "HER2+ fraction",
                  "ER+ and HER2+ fraction",
                  "ER-,HER2-,lymph+,<50 fraction",
                  "ER-,HER2-,lymph+,<50,no event.5 fraction")
View(Q5)