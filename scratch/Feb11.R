wcComplement <- function(w, five.direction = TRUE){
  res <- ""
  if (five.direction == FALSE){  
    for (i in 1:nchar(w)){
      current <- substr(w, i, i)
      if (current == "A"){
        newguy <- "T"
      } else if (current == "C"){
        newguy <- "G"
      } else if (current == "T"){
        newguy <- "A"
      } else if (current == "G"){
        newguy <- "C"
      }
      res <- paste(res, newguy, sep="")
    }
  }else{ # five.direction == TRUE
    for (i in nchar(w):1){
      current <- substr(w, i, i)
      if (current == "A"){
        newguy <- "T"
      } else if (current == "C"){
        newguy <- "G"
      } else if (current == "T"){
        newguy <- "A"
      } else if (current == "G"){
        newguy <- "C"
      }
      res <- paste(res, newguy, sep="")
    }
  }
  return(res)
}

myWatson <-"ACGTTACT"

wcComplement(myWatson, TRUE)
