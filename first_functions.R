add2 <- function(x, y){
  x + y
}

above10 <- function(x){
  use <- x > 10
  x[use]
}

above <- function(x, n = 10){
  use <- x > n
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
      means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}

x <- 1:10
if(x > 5){
  x <- 0
}


vip_test <- function(x, y){
  find_complement <- function(n){
    10 - n
  }
  
  answer <- 1:length(x)
  for(i in x){
    answer[[i]] <- find_complement(i)
  }
  answer
}