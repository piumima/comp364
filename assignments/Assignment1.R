## Piumi Abeynayaka ID 260458806

## Question 1

## i)
x <- sample(1:100, size=1)
y <- sample(1:100, size=1)
x1 <- x
x <- y
y <- x1

## ii)
## M is a vector of randomly sampled numbers from 1 to 10000.
## The size of M (ie. the number of numbers in it) is also randomly chosen from 1 to 10000.
M <- sample(1:10000,size=sample(1:10000,size=1))

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


## Question 2 

## i)
n <- sample(1:20, size=1)
m <- sample(1:20, size=1)
P <- matrix(nrow=n, ncol=m)

## ii)
smallerDim <- ifelse(ncol(M)<=nrow(M), yes = ncol(M), no = nrow(M))
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
## Question 3

## i)
q1 <- list(setdiff(letters,c("a","e","i","o","u")))

## ii)
q2 <- list(seq(from=1, to=100, by=2))

## iii)
q3 <- list(c(09,08,92),c(05,01,86))

myList <- list(q1,q2,q3)

## iv)
myList <- list(myList[[1]], myList[[3]])


## Question 4
setwd("~/repos/comp364")
source("~/repos/comp364/src/hucMini.R")
dataset.collection <- c("miniTCGA", "nki", "vanvliet")
huc <- huc.load(dataSets=dataset.collection)
names(huc)
for (dataset in dataset.collection){
  print(names(huc[[dataset]]))
}

## Question 5
Q5 <- data.frame()
colnames(Q5) <- dataset.collection

## a)
num.patients <- function (myData){
  return (length(huc[[myData]][["clinical"]][["id"]]))
}
number.of.patients <- c()
for (dataset in dataset.collection){
  number.of.patients <- c(number.of.patients,(num.patients(dataset)))
}
Q5 <- rbind(number.of.patients)

## b)
er.plus <- function (myData){
  return (sum(huc[[myData]][["clinical"]][["er"]], na.rm=TRUE))
}
fraction.er.plus <- c()
for (dataset in dataset.collection){
  fraction.er.plus <- c(fraction.er.plus,(er.plus(dataset)/num.patients(dataset)))
}
Q5 <- rbind(Q5,fraction.er.plus)

## c)
good.poor.ratio <- function (myData){
  poor <- sum(huc[[myData]][["clinical"]][["event.5"]], na.rm=TRUE)
  na.values <- sum(is.na(huc[[myData]][["clinical"]][["event.5"]]))
  good <- num.patients(myData)-na.values-poor
  paste(good,":",poor, sep="")
}
good.poor.outcomes <- c()
for (dataset in dataset.collection){
  good.poor.outcomes <- c(good.poor.outcomes,good.poor.ratio(dataset))
}
Q5 <- rbind(Q5,good.poor.outcomes)

## d) ER+ fraction
query <- function (myData, var, patient){
  ## Returns the value found in a given sub-object of a given dataset
  ## for a given patient.
  ##
  ## myData: a string matching one of the datasets
  ## var: a string matching the name of a sub-object
  ## patient: an integer, greater than 0,
  ##          equal to or less than the total number of patients in the dataset
  return (huc[[myData]][["clinical"]][[var]][patient])
}

bool <- c()
for (dataset in dataset.collection){
  for (a in 1:num.patients(dataset)){
    bool <- c(bool, (query(dataset,"er",a)))
  }
}
sum(bool, na.rm = TRUE)/length(bool)

## e) HER2+ fraction
bool <- c()
for (dataset in dataset.collection){
  for (a in 1:num.patients(dataset)){
    bool <- c(bool, (query(dataset,"her2",a)))
  }
}
sum(bool, na.rm = TRUE)/length(bool)

## f) ER+ and HER2+
bool <- c()
for (dataset in dataset.collection){
  for (a in 1:num.patients(dataset)){
    bool <- c(bool, (query(dataset,"er",a) 
                     & query(dataset,"her2",a)))
  }
}
sum(bool, na.rm = TRUE)/length(bool)

## g) ER- and HER2- and lymph+ and <50 years old
bool <- 0
for (dataset in dataset.collection){
  for (a in 1:num.patients(dataset)){
      bool <- c(bool, (!query(dataset,"er",a) 
                        & !query(dataset,"her2",a) 
                        & query(dataset,"lymph",a) 
                        & query(dataset,"age",a)<50))
  }
}
sum(bool, na.rm = TRUE)/length(bool)

## h) ER- and HER2- and lymph+ and <50 years old and no event at 5 years
bool <- c()
for (dataset in dataset.collection){
  for (a in 1:num.patients(dataset)){
    bool <- c(bool, (!query(dataset,"er",a) 
                      & !query(dataset,"her2",a) 
                      & query(dataset,"lymph",a) 
                      & query(dataset,"age",a)<50
                      & !query(dataset,"event.5",a)))
  }
}
sum(bool, na.rm = TRUE)/length(bool)