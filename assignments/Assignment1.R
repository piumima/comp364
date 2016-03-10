## Piumi Abeynayaka ID 260458806

## ====================================================================
## QUESTION 1

## i) Switch values of x and y
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



## iii) Find the maximum value in matrix M
maxVal <- M[1]
for (i in 2:length(M)){
  if (M[i]>maxVal){
    maxVal <- M[i]
  }
}
maxVal



## iv) Function to find max value in a matrix
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

## i) Generate a matrix of random size (between 1-20 rows and columns)
n <- sample(1:20, size=1)
m <- sample(1:20, size=1)
M <- matrix(nrow=n, ncol=m)



## ii) Assign 0 to the diagonal of a square or rectangular matrix
smallerDim <- ifelse((ncol(M)<=nrow(M)), yes = ncol(M), no = nrow(M))
for (i in 1:smallerDim){
  M[i,i] <- 0
}



## iii) and iv) Assign -1 to matrix M above the diagonal and +1 below the diagonal
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


## Function to assign 0 to diagonal, -1 above diagonal, +1 below diagonal of matrix M
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

## i) List of lowercase consonants
q1 <- as.list(setdiff(letters,c("a","e","i","o","u")))



## ii) List of odd numbers
q2 <- as.list(seq(from=1, to=100, by=2))



## iii) List of my and my sibling's birthdays
q3 <- list(c(09,08,92),c(05,01,86))

## List of 3 previous lists
myList <- list(q1,q2,q3)



## iv) List of previous list with only 1st and 3rd elements
myList[c(1,3)]

## ====================================================================
## QUESTION 4: Access hucMini and print out names of datasets in it

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

## Empty data frame for answers to Question 5
Q5 <- data.frame()

## a) CALCULATE NUMBER OF PATIENTS

number.of.patients <- c()
for (dataset in dataset.collection){
  number.of.patients <- c(number.of.patients,
                          length(huc[[dataset]][["clinical"]][["id"]]))
}

## calculate total number of patients in all datasets
number.of.patients[length(dataset.collection)+1] <- sum(number.of.patients)

## add calculated patient numbers to data frame
Q5 <- rbind(number.of.patients)



## b) ER+ FRACTION IN EACH DATASET
## AND
## d) ER+ FRACTION IN ALL DATASETS

er.plus <- c()
for (dataset in dataset.collection){
  er.plus <- c(er.plus, length(subset(huc[[dataset]][["clinical"]], er)[,1]))
}

## calculate ER+ fraction in all datasets
er.plus[length(dataset.collection)+1] <- sum(er.plus)

## add er+ fraction values to data frame
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

## add ratios to data frame as both a string and as a numeric value
Q5 <- rbind(Q5,c(good.poor.string,NA))
Q5 <- rbind(Q5,c(good.poor.numeric,NA))



## e) HER2+ FRACTION

her2 <- c()
for (dataset in dataset.collection){
  her2 <- c(her2,length(subset(huc[[dataset]][["clinical"]],her2)[,1]))
}

## sum of fraction across all datasets
her2[length(dataset.collection)+1] <- sum(her2)

## add her2+ fraction values to data frame
Q5 <- rbind(Q5,her2/number.of.patients)



## f) ER+ & HER2+

er.and.her2 <- c()
for (dataset in dataset.collection){
  er.and.her2 <- c(er.and.her2,
                   length(subset(huc[[dataset]][["clinical"]],
                                 (her2 & er))[,1])
                   )
}

## all datasets
er.and.her2[length(dataset.collection)+1] <- sum(er.and.her2)

## add to dataframe
Q5 <- rbind(Q5,er.and.her2/number.of.patients)



## g) ER- & HER2- & LYMPH+ & <50 YEARS OLD

er.her.lymph.age <- c()
for (dataset in dataset.collection){
  er.her.lymph.age <- c(er.her.lymph.age,
                        length(subset(huc[[dataset]][["clinical"]],
                                      (!er & !her2 & lymph & age<50))[,1])
                        )
}

## all datasets
er.her.lymph.age[length(dataset.collection)+1] <- sum(er.her.lymph.age)

## add to dataframe
Q5 <- rbind(Q5,er.her.lymph.age/number.of.patients)



## h) ER- & HER2- & LYMPH+ & <50 YEARS OLD & NO EVENT AT 5 YEARS

er.her.lymph.age.event <- c()
for (dataset in dataset.collection){
  er.her.lymph.age.event <- c(er.her.lymph.age.event, 
                              length(subset(huc[[dataset]][["clinical"]],
                                            (!er & !her2 & lymph & age<50 & !event.5))[,1])
                              )
}

## all datasets
er.her.lymph.age.event[length(dataset.collection)+1] <- sum(er.her.lymph.age.event)

## add to dataframe
Q5 <- rbind(Q5,er.her.lymph.age.event/number.of.patients)

## label dataframe's columns and rows and view it
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