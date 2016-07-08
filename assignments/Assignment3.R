## Piumi Abeynayaka ID: 260458806

library("e1071")
setwd("~/repos/comp364")
source("~/repos/comp364/src/hucMini.R")
huc <- huc.load(dataSets="vanvliet")


## Question 1
tmp2 <- subset(huc$vanvliet$clinical, !is.na(event.5))
tmp<-tmp2[, colnames(tmp2) %in% c("event.5", "er", "her2", "lymph", "grade")]

for (i in 1:length(tmp[1,])){
  tmp[[i]]<-factor(tmp[[i]])
}

train <- tmp[1:450,]
valid <- tmp[451:nrow(tmp),]
classifier<-naiveBayes(  
  event.5 ~ ., 
  data = train)

outcome.predict <- predict(classifier, valid)

results <- table( outcome.predict,
                  valid$event.5,
                  dnn=list('predicted','actual'))
(accuracy <- (results[1,1]+results[2,2])/sum(results))

## Question 2

## Function to calculate 20 most differential probes for a given clinical dataset
## subtype: is a huc clinical dataset that includes at least the event.5 column 
##          and is named by the patient ids
## Returns a dataframe with the probe id, gene name, p values, 
## and the index of the 20 most differential probes
diff.probes <- function(subtype){
  good.exprs <- huc$vanvliet$exprs[,rownames(subset(subtype, !event.5))]
  poor.exprs <- huc$vanvliet$exprs[,rownames(subset(subtype, event.5))]
  
  t.p.values <- c()
  t.index <- c()
  
  for (i in 1:nrow(huc$vanvliet$exprs)){
    res <- t.test(good.exprs[i,], poor.exprs[i,])$p.value
    if (res <= 0.001){
      t.index <- c(t.index, i)
      t.p.values <- c(t.p.values, res)
    }
  }
  t.probes <- huc$vanvliet$probe.info$probe.id[t.index]
  t.genes <- huc$vanvliet$probe.info$gene.name[t.index]
  results <- data.frame(t.index, t.probes, t.genes, t.p.values)
  results.ordered <- results[order(results$t.p.values),]
  results2 <- subset(results.ordered, !duplicated(results.ordered[,"t.genes"]))[1:20,]
  return(results2)
}

## find the 20 most differential probes in the training dataset
unstrat.probes <- diff.probes(tmp2[1:450,])
print(unstrat.probes$t.genes)

## Transpose the exprs data and append the event.5 column from the clinical dataset
## as a factor and store the data frame as 'tmp'.
tmp <- data.frame(t(huc$vanvliet$exprs), factor(huc$vanvliet$clinical$event.5))

## Name the last column as "event.5"
names(tmp)[ncol(tmp)] <- "event.5"

## Take a training subset of tmp: the first 450 patients as columns and
## the 20 most differential probes and event.5 as rows
## Then build an NBC from it
training <- tmp[tmp2$id[1:450],c(unstrat.probes$t.index,ncol(tmp))]
unstrat.classifier<-naiveBayes(  
  event.5 ~ ., 
  data = training)


## Function that returns the accuracy of a classifier at predicting event.5
## and prints the results table
## classif: a Naive Bayes classifier
## ds: a validation dataset with the same predictive variables as the training dataset
nbc.results <- function(classif, ds){
  outcome.predict <- predict(classif, ds)
  results <- table( outcome.predict,
                    ds$event.5,
                    dnn=list('predicted','actual'))
  accuracy <- (results[1,1]+results[2,2])/sum(results)
  print(results)
  return(accuracy)
}

## Return the results and accuracy of the above classifier tested on the patients
## remaining in the vanvliet clinical dataset that weren't used in the training dataset
validation <- tmp[tmp2$id[451:nrow(tmp2)],c(unstrat.probes$t.index,ncol(tmp))]
(nbc.results(unstrat.classifier, validation))

## Question 3
## a)

## take subtypes of clinical dataset
## ER+ = ep; ER- = en; HER2+ = hp; HER2- = hn
ephp <- subset(huc$vanvliet$clinical,!is.na(event.5) & er & her2)
enhp <- subset(huc$vanvliet$clinical,!is.na(event.5) & !er & her2)
ephn <- subset(huc$vanvliet$clinical,!is.na(event.5) & er & !her2)
enhn <- subset(huc$vanvliet$clinical,!is.na(event.5) & !er & !her2)

## Find the 20 most differential probes for each subtype
ephp.probes <- diff.probes(ephp)
enhp.probes <- diff.probes(enhp)
ephn.probes <- diff.probes(ephn)
enhn.probes <- diff.probes(enhn)

(data.frame(ephp.probes$t.genes,
            enhp.probes$t.genes,
            ephn.probes$t.genes,
            enhn.probes$t.genes,
            unstrat.probes$t.genes))

## Find genes shared between subtypes
sort(table(factor(c(as.character(ephp.probes$t.genes),
                    as.character(enhp.probes$t.genes),
                    as.character(ephn.probes$t.genes),
                    as.character(enhn.probes$t.genes),
                    as.character(unstrat.probes$t.genes)))), decreasing=TRUE)

## b)

## Take a subset of the tmp dataset: the columns are patients of a particular subtype
## and rows are the 20 most differential probes for that subtype as well as the
## event.5 column which is the last column in tmp
ephp.df <- tmp[ephp$id, c(ephp.probes$t.index, ncol(tmp))]
enhp.df <- tmp[enhp$id, c(enhp.probes$t.index, ncol(tmp))]
ephn.df <- tmp[ephn$id, c(ephn.probes$t.index, ncol(tmp))]
enhn.df <- tmp[enhn$id, c(enhn.probes$t.index, ncol(tmp))]

## Build NBCs for each subtype
ephp.classifier<-naiveBayes(event.5 ~ ., data = ephp.df)
enhp.classifier<-naiveBayes(event.5 ~ ., data = enhp.df)
ephn.classifier<-naiveBayes(event.5 ~ ., data = ephn.df)
enhn.classifier<-naiveBayes(event.5 ~ ., data = enhn.df)

## Store the accuracies of each classifier against the other subtypes in a vector
accuracies<- c()
classif<- list(unstrat.nbc=list(unstrat.classifier,unstrat.probes),
               ephp.nbc=list(ephp.classifier,ephp.probes),
               enhp.nbc=list(enhp.classifier,enhp.probes),
               ephn.nbc=list(ephn.classifier,ephn.probes),
               enhn.nbc=list(enhn.classifier,enhn.probes))
for (class in classif){
    for (subtype in list(ephp, enhp, ephn, enhn)){
      accuracies <- c(accuracies, 
                      nbc.results(class[[1]], 
                                  tmp[subtype[["id"]],c(class[[2]]$t.index,ncol(tmp))]))
    }
}

## convert the accuracies vector into a matrix and name the dimensions
accuracies.table <- matrix(data=accuracies, nrow=5, ncol=4, byrow=TRUE)
colnames(accuracies.table) <- c("ER+/HER2+","ER-/HER2+","ER+/HER2-","ER-/HER2-")
rownames(accuracies.table) <- c("Unstratifed NBC","ER+/HER2+ NBC","ER-/HER2+ NBC","ER+/HER2- NBC","ER-/HER2- NBC")
print(accuracies.table)
