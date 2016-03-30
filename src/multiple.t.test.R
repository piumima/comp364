
source("~/repos/comp364/src/hucMini.R")  # students should change this to ~/cs364/src/hucMini.R 
dataset.collections <- c("miniTCGA", "vanvliet", "nki") 
huc <- huc.load(dataset.collections, "~/repos/comp364/data")  # students should change this to ~/cs364/data
attach(huc$vanvliet)

c.observed.good <- subset(clinical, !event.5) 
c.observed.bad <- subset(clinical, event.5)

count <- 0
for (i in 1:3) {
  c.good.exprs <- exprs[i, c.observed.good$id]
  c.bad.exprs <- exprs[i, c.observed.bad$id]  
  
  c.t.result <- t.test(c.good.exprs, c.bad.exprs,
                      var.equal=FALSE)
  p.of.t.result <- c.t.result$p.value
  print(p.of.t.result)
  if (p.of.t.result < 0.001) {
    count <- count + 1
    print(probe.info$gene.name[i])
  } # closes if
}

print(count)


