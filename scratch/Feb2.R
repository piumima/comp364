lanes.highway<-matrix(data=sample(c(0,1),17*200,replace=TRUE),
                      nrow=17,
                      ncol=200)
for (i in 1:ncol(lanes.highway)){
  for (j in 1:nrow(lanes.highway)){
    if (lanes.highway[j,i]==1){
      cat(paste(c("Col:",i,"Row",j,"\n"),collapse=" "))
    }
  }
}