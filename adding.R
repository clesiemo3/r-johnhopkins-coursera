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