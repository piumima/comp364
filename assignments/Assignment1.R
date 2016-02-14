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
## The size of M (ie. the number of numbers in it) is also 
## randomly chosen from 1 to 10000.
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
q1 <- as.list(setdiff(letters,c("a","e","i","o","u")))

## ii)
q2 <- as.list(seq(from=1, to=100, by=2))

## iii)
q3 <- list(c(09,08,92),c(05,01,86))

myList <- list(q1,q2,q3)

## iv)
sub.myList <- list(myList[[1]], myList[[3]])


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

## Data frame for answers to Question 5
Q5 <- data.frame()

## a) Calculate number of patients
num.patients <- function (myData){
  return (length(huc[[myData]][["clinical"]][["id"]]))
}

number.of.patients <- c()
for (dataset in dataset.collection){
  number.of.patients <- c(number.of.patients,(num.patients(dataset)))
}

Q5 <- rbind(c(number.of.patients,NA))

## b) ER+ fraction in each dataset
er.plus <- function (myData){
  return (sum(huc[[myData]][["clinical"]][["er"]], na.rm=TRUE))
}

fraction.er.plus <- c()
for (dataset in dataset.collection){
  fraction.er.plus <- c(fraction.er.plus,(er.plus(dataset)/num.patients(dataset)))
}

Q5 <- rbind(Q5,c(fraction.er.plus,NA))

## c) good to poor outcomes ratio in each dataset
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

Q5 <- rbind(Q5,c(good.poor.outcomes,NA))

## d) ER+ fraction for all patients
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

ER.plus.fraction <- sum(bool, na.rm = TRUE)/length(bool)

Q5[1,4] <- length(bool)
Q5[2,4] <- ER.plus.fraction

## e) HER2+ fraction
bool <- c()
for (dataset in dataset.collection){
  for (a in 1:num.patients(dataset)){
    bool <- c(bool, (query(dataset,"her2",a)))
  }
}

HER2.plus.fraction <- sum(bool, na.rm = TRUE)/length(bool)

Q5 <- rbind(Q5,c(rep(NA,times = length(dataset.collection)),HER2.plus.fraction))

## f) ER+ and HER2+
bool <- c()
for (dataset in dataset.collection){
  for (a in 1:num.patients(dataset)){
    bool <- c(bool, (query(dataset,"er",a) 
                     & query(dataset,"her2",a)))
  }
}

ER.and.HER2 <- sum(bool, na.rm = TRUE)/length(bool)

Q5 <- rbind(Q5,c(rep(NA,times = length(dataset.collection)),ER.and.HER2))

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

ER.HER2.lymph.50 <- sum(bool, na.rm = TRUE)/length(bool)

Q5 <- rbind(Q5,c(rep(NA,times = length(dataset.collection)),ER.HER2.lymph.50))

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

ER.HER2.lymph.50.event5 <- sum(bool, na.rm = TRUE)/length(bool)

Q5 <- rbind(Q5,c(rep(NA,times = length(dataset.collection)),ER.HER2.lymph.50.event5))

colnames(Q5) <- c(dataset.collection,"all")
rownames(Q5) <- c("Number of patients", 
                  "ER+ fraction", 
                  "Ratio of good to poor outcomes",
                  "HER2+ fraction",
                  "ER+ and HER2+ fraction",
                  "ER-,HER2-,lymph+,>50 fraction",
                  "ER-,HER2-,lymph+,>50,no event.5 fraction")
View(Q5)