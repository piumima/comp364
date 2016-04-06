## Assignment ##3 solutions. M. Hallett, M Ghadie COMP-364 T4LS, 2016

## Question 1.

##install.packages("e1071")
library("e1071")
source("~/repos/comp364/src/hucMini.R")  ## T4LS should be cs364 or cs618 for you guys.

setwd('~/repos/comp364')
dataset.collections <- c("vanvliet")
huc <- huc.load(dataSets = dataset.collections, dataDir = "data")
names(huc)



## add in the lymph and grade

tmp2 <- subset(huc$vanvliet$clinical, !is.na(event.5))


tmp<-tmp2[, colnames(tmp2) %in% c("event.5", "er", "her2", "lymph", "grade")]
tmp$er<-factor(tmp$er)
tmp$her2<-factor(tmp$her2)
tmp$lymph <- factor(tmp$lymph)
tmp$grade <- factor(tmp$grade)
tmp$event.5<-factor(tmp$event.5)

half <- floor( nrow(tmp) / 2 )

training <- tmp[1:half,]
validation <- tmp[(half+1):nrow(tmp),]
## careful here: I find it non-intuitive but if you write
## half+1:nrow(tmp) without the parenthesis around (half+1)
## R will do something very different than what you want.
## This is because the order of operations for the : operator are
## stronger than for +.
## When in doubt, use parentheses.
## Except if you are from South Africa: then you should use round brackets.

classifier<-naiveBayes(
  event.5 ~ .,
  data = training)
## so here event.5 is modelled as c( "er", "her", "lymph" and "grade" )

outcome.predict <- predict(classifier, validation)

table( outcome.predict,
       validation$event.5,
       dnn=list('predicted','actual'))

## accuracy <- (## true positives + ## true negatives) / total ##
(accuracy <-  (8 + 331) / nrow(validation) ) * 100



## Question 2 (using most d.e. probes in unstratified (all patients) analysis)

valid.patients <- which(!is.na(huc$vanvliet$clinical$event.5 ))
clin <- huc$vanvliet$clinical[valid.patients,]
exprs <- huc$vanvliet$exprs[,valid.patients]

## the fairest way to do this is to do the t.test only in the training
## dataset.
## so first let's define the training and validation datasets.

half <- floor( nrow(clin) / 2 )
training.clin <- clin[1:half,]
training.exprs <- exprs[,1:half]

validation.clin <- clin[(half+1):nrow(clin),]
validation.exprs <- exprs[,(half+1):ncol(exprs)]

## get ready to do the t-test in the training dataset
good.outcome.ind <- which(training.clin$event.5 == FALSE)
bad.outcome.ind <- which(training.clin$event.5 == TRUE)

## do the t-test
ttest.res <- list()
ttest.pvalues <- vector()
for (i in 1:nrow(training.exprs)) {    ## for each row (probe) ...

  good.exprs <- training.exprs[i,][good.outcome.ind]
  bad.exprs <- training.exprs[i,][bad.outcome.ind]
  ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=TRUE )
  ttest.pvalues[i] <- ttest.res[[i]]$p.value
}
topProbes <- order(ttest.pvalues, decreasing=FALSE)[1:20]

## here are the gene names of the top 20.
(gene.names <- huc$vanvliet$probe.info$gene.name[topProbes])

## now let's massage the data into a format the naiveBayes() expects
training <- as.data.frame( t( training.exprs[topProbes, ] ) )
training$event.5 <- as.factor(training.clin$event.5)

## now the validation
validation <- as.data.frame( t( validation.exprs[topProbes, ]))
validation$event.5 <- as.factor(validation.clin$event.5)



## now as before.
classifier<-naiveBayes(
  event.5 ~ .,
  data = training)

outcome.predict <- predict(classifier, validation)

table( outcome.predict,
       validation$event.5,
       dnn=list('predicted','actual'))

## accuracy <- (## true positives + ## true negatives) / total ##
(accuracy <-  (46 + 276) / nrow(validation) ) * 100







# Question 3. Stratified predictors by subtype



# we need to consider five predictors all, E+/H-, E+/H+, E-/H-, E-/H+ so five cohorts.
# it's not right to do this (except that this is an assignment so it's ok) but we are going to worry
# about cross validation here and overlearning etc.
 
all.indices <- which(!is.na(huc$vanvliet$clinical$event.5 ))
ep.hn.indices <- which(huc$vanvliet$clinical$er & !huc$vanvliet$clinical$her2 & !is.na(huc$vanvliet$clinical$event.5))
ep.hp.indices <- which(huc$vanvliet$clinical$er & huc$vanvliet$clinical$her2 & !is.na(huc$vanvliet$clinical$event.5))
en.hp.indices <- which(!huc$vanvliet$clinical$er & huc$vanvliet$clinical$her2 & !is.na(huc$vanvliet$clinical$event.5))
en.hn.indices <- which(!huc$vanvliet$clinical$er & !huc$vanvliet$clinical$her2 & !is.na(huc$vanvliet$clinical$event.5))

for (c in c("all.indices", "ep.hp.indices", "ep.hn.indices", "en.hp.indices", "en.hn.indices")) {
  
  assign( paste(c, ".clin", sep=""), huc$vanvliet$clinical[get(c),] )
  assign( paste(c, ".exprs", sep=""), huc$vanvliet$exprs[,get(c)] )
  assign( paste(c, ".good", sep=""), which( huc$vanvliet$clinical[ get(c), ]$event.5 == FALSE ) )
  assign( paste(c, ".bad", sep=""), which( huc$vanvliet$clinical[ get(c), ]$event.5 == TRUE ) )
}

### Now here is some nice tricks to avoid copying and pasting!
# three things: 
# 1. note how i use the names of the cohorts in the for loop 
#   I'm not iterating over numbers but over strings (actually variable names!)
#   A little bit like the midterm question.
# 2. I use the assign(x, y) which creates a variable x in your R environment with value y
#   look it up in the help.
# 3. the get(x) funciton takes a string x (in this case the name of a cohort) and evaluates it
#   as a variable. Try it out! Here is a simple example
#   mike <- "King"  # make a string
#   mike    # nothing suprising here
#   x <- "mike" # make a string that matches the varaible name
#   print(x) # print out the value of x
#   print(get(x))
#   Sooo entirely unsurprisingly mike evaluates to king.



for (c in c("all.indices", "ep.hp.indices", "ep.hn.indices", "en.hp.indices", "en.hn.indices")) {
  ## do the t-test for each cohort of interest
  
  training.exprs <- huc$vanvliet$exprs[, get(c)]
  training.clin <- huc$vanvliet$clinical[get(c), ]
  
  ttest.res <- list()
  ttest.pvalues <- vector()
  
  for (i in 1:nrow(training.exprs)) {    ## for each row (probe) ...
    
    good.exprs <- training.exprs[i,][get(paste(c, ".good", sep=""))]
    bad.exprs <- training.exprs[i,][get(paste(c, ".bad", sep=""))]
    ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=TRUE )
    ttest.pvalues[i] <- ttest.res[[i]]$p.value
  }
  assign(paste(c, ".topProbes", sep=""), order(ttest.pvalues, decreasing=FALSE)[1:20])
  
}
# this creates variables of the form all.indices.topProbes


for (c in c("all.indices", "ep.hp.indices", "ep.hn.indices", "en.hp.indices", "en.hn.indices")) {
  ## here are the gene names of the top 20.
  cat("\n\nCohort is\n", c)
  print(huc$vanvliet$probe.info$gene.name[get(paste(c, ".topProbes", sep=""))])

}
  
  

for (c in c("all.indices", "ep.hp.indices", "ep.hn.indices", "en.hp.indices", "en.hn.indices")) {
  cat("\n\nCohort is\n", c, "\n")
  training.exprs <- huc$vanvliet$exprs[  , get(c)]
  training.clin <- huc$vanvliet$clinical[get(c), ]
  cat("\tNumber of samples:", length(get(c)), "\n")
  
  topProbes <- get(paste(c, ".topProbes", sep=""))
  
  ## now let's massage the data into a format the naiveBayes() expects
  training <- as.data.frame(  t( training.exprs[topProbes, ]  ))
  training$event.5 <- as.factor(training.clin$event.5)

  ## now the validation
  validation <- as.data.frame( t( training.exprs[topProbes, ]))
  validation$event.5 <- as.factor(training.clin$event.5)

  # now let's build our classifiers.  I know, we used the same data for training and testing.
  # If you are becoming a bioinformatician, you should vomit a little into your mouth
  # when you see such non-sense. You should see this as a training exercise for some point 
  # in the future when you are in a  meeting at your pharmaceutical company or academic lab.
  # Practice your condescending scoff in front of your colleagues who don't understand these
  # sorts of things.
  

  ## now as before.
  classifier<-naiveBayes(
    event.5 ~ .,
    data = training)

    # let's save the cohort-specific classifier for future reference
  assign( paste(c, ".classifier", sep=""), classifier)
  
  outcome.predict <- predict(classifier, validation)
  # let's save this too.
  assign( paste(c, ".predict", sep=""), classifier)
  

  
  print(table( outcome.predict,
       validation$event.5,
       dnn=list('predicted','actual')))

} # end of for loop

# The output is something like this (i hope):
#   
#   
#   Cohort is
# all.indices 
# Number of samples: 897 
# actual
# predicted FALSE TRUE
# FALSE   481   53
# TRUE    245  118
# 
# 
# Cohort is
# ep.hp.indices 
# Number of samples: 62 
# actual
# predicted FALSE TRUE
# FALSE    43    1
# TRUE      1   17
# 
# 
# Cohort is
# ep.hn.indices 
# Number of samples: 597 
# actual
# predicted FALSE TRUE
# FALSE   387   35
# TRUE    123   52
# 
# 
# Cohort is
# en.hp.indices 
# Number of samples: 61 
# actual
# predicted FALSE TRUE
# FALSE    37    1
# TRUE      6   17
# 
# 
# Cohort is
# en.hn.indices 
# Number of samples: 177 
# actual
# predicted FALSE TRUE
# FALSE   106   14
# TRUE     23   34

# So ER+/HER2+ looks pretty good, as does ER-/HER2+. 
# None look like they totally suck except maybe the all (unstratified)

# Now for the cross-comparison of each classifier across each cohort.

cohorts <- c("all.indices", "ep.hp.indices", "ep.hn.indices", "en.hp.indices", "en.hn.indices")
m <- matrix(nrow=length(cohorts), ncol=length(cohorts), dimnames = list( cohorts, cohorts ))

for (c in 1:(length(cohorts)-1)) {
  for (d in (c+1):length(cohorts)) {
    
    target.classifier <- get(cohorts[c]); target.cohort <- get(cohorts[d])
    topProbes <- get(paste(cohorts[c], ".topProbes", sep=""))
    
    target.cohort.exprs <- huc$vanvliet$exprs[ topProbes , target.cohort]
    target.cohort.clin <- huc$vanvliet$clinical[target.cohort, ]
    
    target.cohort.final <- as.data.frame( t( target.cohort.exprs ))
    target.cohort.final$event.5 <- as.factor(target.cohort.clin$event.5)
    
    tmp <- predict( get(paste(cohorts[c], ".classifier", sep="")),  target.cohort.final  )
    
    cat("\nClassifier is ", cohorts[c], "\n")
    cat("\nTarget Cohort is", cohorts[d], "\n" )
    
    print(table( tmp,
                 target.cohort.final$event.5,
                 dnn=list('predicted','actual')))
    
    
    tp <- fp <- tn <- fn <- 0   # a quick way to make them all 0. tp=true positive
    our.predictions <- as.logical(tmp)
    true.answers <- as.logical(target.cohort.final$event.5)
    
    for (i in 1:length(our.predictions)) {
          if (our.predictions[i] & true.answers[i]) { tp <- tp + 1 }
          if (!our.predictions[i] & !true.answers[i]) { tn <- tn + 1 }
          if (our.predictions[i] & !true.answers[i]) { fp <- fp + 1 }
          if (!our.predictions[i] & true.answers[i]) { fn <- fn + 1 }
      }
    acc <- (tp + tn)/(length(tmp))
    m[c,d] <- acc
  }
}
print(m)
# 
# all.indices ep.hp.indices ep.hn.indices en.hp.indices en.hn.indices
# all.indices            NA     0.5322581     0.7973199     0.4262295     0.3615819
# ep.hp.indices          NA            NA     0.7772194     0.6557377     0.6666667
# ep.hn.indices          NA            NA            NA     0.3770492     0.3389831
# en.hp.indices          NA            NA            NA            NA     0.5932203
# en.hn.indices          NA            NA            NA            NA            NA
    