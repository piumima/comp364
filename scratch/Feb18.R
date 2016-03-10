source("~/repos/comp364/src/hucMini.R")  # students should change this to ~/cs364/src/hucMini.R 
dataset.collections <- c("miniTCGA", "vanvliet", "nki") 
huc <- huc.load(dataset.collections, "~/repos/comp364/data") 
attach(huc$vanvliet)

observed.good <- subset(clinical, !event.5)
observed.bad <- subset(clinical, event.5)
good.exprs <- c()
bad.exprs <- c()
count <- 0

for (i in 1:nrow(exprs)){
  good.exprs <- exprs[i,observed.good$id]
  bad.exprs <- exprs[i,observed.bad$id]
  t.result <- t.test(good.exprs,bad.exprs,var.equal = TRUE)
  p.of.t.result <- t.result$p.value
  if (p.of.t.result < 0.01){
    count <- count+1
    print(probe.info$gene.name[i])
  }
}
print(count)