# Class Prediction M2.L2
install.packages("e1071")
library("e1071")

# if this library fails, first do the following:
# install.packages("e1071")

source("~/cs364/src/hucMini.R")  # T4LS should be cs364 or cs618 for you guys.


setwd('~/cs364')
dataset.collections <- c("vanvliet")
huc <- huc.load(dataSets = dataset.collections, dataDir = "data")
names(huc)

huc$vanvliet$clinical[1,]

# just er and her2 status


tmp2 <- subset(huc$vanvliet$clinical, !is.na(event.5))
tmp<-tmp2[, colnames(tmp2) %in% c("event.5", "er", "her2")]
tmp$er<-factor(tmp$er)
tmp$her2<-factor(tmp$her2)

tmp$event.5<-factor(tmp$event.5)


classifier<-naiveBayes(  
  event.5 ~ ., 
  data = tmp) 

# The notation "event.5 ~ ." means that event.5 is "modelled as" (~) all the remaining variables (columns in tmp)
# One could instead write event.5 ~ c("er", "her2")

outcome.predict <- predict(classifier, tmp)

table( outcome.predict, 
       tmp$event.5, 
       dnn=list('predicted','actual'))

# add in the lymph and grade

tmp2 <- subset(huc$vanvliet$clinical, !is.na(event.5))
tmp<-tmp2[, colnames(tmp2) %in% c("event.5", "er", "her2", "lymph", "grade")]
tmp$er<-factor(tmp$er)
tmp$her2<-factor(tmp$her2)
tmp$lymph <- factor(tmp$lymph)
tmp$grade <- factor(tmp$grade)
tmp$event.5<-factor(tmp$event.5)


classifier<-naiveBayes(  
  event.5 ~ ., 
  data = tmp) 
# so here event.5 is modelled as c( "er", "her", "lymph" and "grade" )

outcome.predict <- predict(classifier, tmp)

table( outcome.predict, 
       tmp$event.5, 
       dnn=list('predicted','actual'))

# USE EVERYTHING

tmp2 <- subset(huc$vanvliet$clinical, !is.na(event.5))
#tmp<-tmp2[, colnames(tmp2) %in% c("event.5", "er", "her2", "lymph", "grade")]
tmp2 <- tmp2[,c(-1, -2, -3)]

tmp <- tmp2
for (i in 1:length(tmp2[1,])) {
  tmp[[i]] <- factor(tmp2[[i]])
}

# remove the NA columns
tmp <- tmp[,c(-4, -17:-12, -22)]

classifier<-naiveBayes(  
  event.5 ~ ., 
  data = tmp) 

outcome.predict <- predict(classifier, tmp)

table( outcome.predict, 
       tmp$event.5, 
       dnn=list('predicted','actual'))
# ??? huh :-)

#
# explanation of how the Naive Bayes' Classifier works
#

# get rid of NAs in event.5 (the variable we want to predict) for simplicity.
#There are ways to deal with these but...
tmp2 <- subset(huc$vanvliet$clinical, !is.na(event.5))

length(tmp2[,1])

tmp<-tmp2[, colnames(tmp2) %in% c("event.5", "er", "her2", "grade", "lymph")]
tmp$er<-factor(tmp$er)
tmp$her2<-factor(tmp$her2)
tmp$grade <- factor(tmp$grade)
tmp$lymph <- factor(tmp$lymph)
tmp$event.5<-factor(tmp$event.5)

dataset <- tmp[1:20,]
dataset

next.dataset <- tmp[21:length(tmp[,1]), ]
next.dataset[1, c("er", "her2", "grade", "lymph")]

# Prior Probabilities

# Pr[ bad outcome ] == Pr[ event.5 == TRUE ] == (# TRUE / 20)
(p.bad.outcome <- length(which(dataset$event.5==TRUE)) / 20)
(p.good.outcome <- 1 - p.bad.outcome)

# Conditional Probabilities

# Where Bad Outcome is observed (event.5 == TRUE) first

# Pr[ ER+ | bad outcome ]
# (Ok, so just look at bad outcome patients. Count them. 
#  then count how many of these bad outcome patients are ER+.)

p.e_plus.bad <- 4/8

# Pr[ ER- | bad outcome ]
p.e_neg.bad <- 4/8


# Pr[ Her2+ | bad outcome ]
p.h_plus.bad <- 2/8

# Pr[ Her2- | bad outcome ]
p.h_neg.bad <- p.h_plus.bad 
# 6/8



# Pr[ grade | bad outcome ]    (3 levels)
p.gr3.bad <- 6 / 8
p.gr2.bad <- 2 / 8
p.gr1.bad <- 0 / 8
  


# Pr[ lymph | bad outcome ] (2 levels)
p.ln.plus.bad <- 0/8
p.ln.neg.bad <- 1 - p.ln.plus.bad


# Now Good Outcome (event.5 == FALSE) 

# Pr[ ER | good outcome ]
p.e_plus.good <- 9 / 12
p.e_neg.good <- 1 - p.e_plus.good

# Pr[ Her2 | good outcome ]
p.h_plus.good <- 0
p.h_neg.good <- 1- p.h_plus.good


# Pr[ grade | good outcome ]
p.gr3.good <- 5 / 12
p.gr2.good <- 5 / 12
p.gr1.good <- 1 - (p.gr3.good + p.gr2.good)


# Pr[ lymph | good outcome ]
p.ln.plus.good <- 0
p.ln.neg.good <- 1 - p.ln.plus.good

# Let's predict the first patient of next.dataset
next.dataset[1,]
#              er  her2 grade lymph event.5
# vanvliet.21 TRUE FALSE     2 FALSE   FALSE


# Choose the maximum between the following two equations

# Pr[ bad.outcome ] * Pr[ ER+ | bad.outcome ] * Pr[ Her2- | bad.outcome ] 
#      * Pr[Grade 2 | bad.outcome] * Pr[ lymph- | bad.outcome ]

(likelihood.bad <- p.bad.outcome * p.e_plus.bad  * p.h_neg.bad * p.gr2.bad * p.ln.neg.bad)


# versus
# Pr[ good.outcome ] * Pr[ ER+ | good.outcome ] * Pr[ Her2- | good.outcome ] 
#      * Pr[Grade 2 | good.outcome] * Pr[ lymph- | good.outcome ]

(likelihood.good <- p.good.outcome * p.e_plus.good  * p.h_neg.good * p.gr2.bad * p.ln.neg.good)


remaining.cases <- predict( classifier, next.dataset )

table( remaining.cases, 
       next.dataset$event.5, 
       dnn=list('predicted','actual'))

# install.packages("survival")
library("survival")


ind.pred.good <- which( remaining.cases == TRUE )
next.dataset[ ind.pred.good, ]
pred.good <- rownames(next.dataset[ ind.pred.good, ] )
is.good <- (huc$vanvliet$clinical$id %in% pred.good)

surv.tmp <- Surv(huc$vanvliet$clinical$time/12, huc$vanvliet$clinical$event.5)


surv.fit <- survfit( surv.tmp ~ is.good )

plot(surv.fit, col=c("red", "blue")) 

abline(v=c(5, 10), lty=2)

legend("bottomright", col=c("blue", "red"), lty=1, legend=c("Good prognosis", "Poor prognosis"))



