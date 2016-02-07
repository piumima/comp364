coolRandomdata<-matrix(data=runif(n=120,
                                  min=-2,
                                  max=5),
                       nrow=12,
                       ncol=10)
colsWeWant <- sample(1:ncol(coolRandomdata),5)

#lapply - apply function to every element of a list
#apply - apply a function to the selected margin/dim of a eg. matrix, data frame, etc

vars <- c()
for(i in 1:nrow(coolRandomdata)){
  vars <- c(vars,var(coolRandomdata[i,]))
}
rowsWeWant <- order(vars,decreasing=TRUE)[1:5]
coolRandomdata[rowsWeWant,colsWeWant] 
