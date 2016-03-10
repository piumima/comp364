## how to make lapply with a for loop

yaxis <- list()

for (i in 1:length(xaxis)){
  yaxis[i] <- dnorm(xaxis[i])
}