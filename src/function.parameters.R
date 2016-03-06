



volume.vers2 <- function(height, 
                         width=1, 
                         length=1) {
 print(width)
  print(length)
  return(height * width *length)
}

volume.vers2(length=3, width=7)



yaxis <- list()
for (i in 1:length(xaxis)) {
  yaxis[i] <- dnorm(xaxis[i])
}

lapply(xaxis[1:50], dnorm)