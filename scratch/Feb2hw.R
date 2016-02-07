#create a matrix full of random real numbers between -2 and 4 (k rows and z columns greater than 22)
k<-6
z<-24
M<-matrix(data=sample(-2:4,k*z,replace=TRUE),nrow=k,ncol=z)
#should use runif or distribution instead to get real numbers

#compute the mean and variance of each row
means<-numeric(0)
vars<-numeric(0)
for (i in 1:nrow(M)){
  means[i]<-mean(M[i,])
  vars[i]<-var(M[i,])
}
#determine the rows where the mean is greater than the mode
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modes<-numeric(0)
for (i in 1:nrow(M)){
  modes[i]<-getmode(M[i,])
}
row.means.great<-numeric(0)
for (i in 1:nrow(M)){
  if (means[i]>modes[i]){
    row.means.great<-c(row.means.great,i)
  }
}

#extract a submatrix which contains 22 randomly selected columns of M and the k/2 rows with the highest variance
var.sort<-sort(vars,decreasing=TRUE)
sub.rows<-numeric(0)
for (j in 1:k){
  sub.rows<-c(sub.rows,which(unique(var.sort)[j]==vars))
}
sub.col<-sample(1:z,22,replace=FALSE)
sub<-M[sub.rows[1:(k/2)],sub.col]

sub.rows2<-numeric(0)
vars.copy<-vars
for (t in 1:k){
  sub.rows2[t]<-which.max(vars.copy)
  vars.copy[sub.rows2[t]]<-NA
}