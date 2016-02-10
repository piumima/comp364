M <- matrix(ncol=10, nrow=10)
for (i in 1:(nrow(M)-1)){
  for (j in (i+1):ncol(M)){
    M[i,j] <- TRUE
  }
}
for (i in 2:nrow(M)){
  for (j in 1:(i-1)){
    M[i,j] <- FALSE
  }
}