addTwo <- function(x,y){
  x + y
}

addTwoExp <- function(x,y){
  return(x+y)
}

aboveTen <- function(x){
  use <- x > 10
  x[use]
}

above <- function(x,n=10){
  use <- x > n
  x[use]
}

columnMean <- function(y,removeNA = TRUE){
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[,i],na.rm=removeNA)
  }
  means
}