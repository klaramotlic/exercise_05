DDP <- function(xa, xb, xab){
  #mapping
  
  map_xa <- sample(xa)
  map_xb <- sample(xb)
  
  
  map_xa <- c(0, map_xa)
  map_xb <- c(0,map_xb)
  
  map_xa <- c(map_xa, sum(map_xa))
  map_xb <- c(map_xb, sum(map_xb))

  uniques <- unique(map_xa, map_xb)
  
  difference <- abs(diff(uniques))
  
  sorted <- sort(difference)
  
  result <- identical(sorted, xab)
  if(reseni <- TRUE){
    return('correct')
  }
  else{
    
    map_xa <- sample(xa)
    map_xb <- sample(xb)
    
    
    map_xa <- c(0, map_xa)
    map_xb <- c(0,map_xb)
    
    map_xa <- c(map_xa, sum(map_xa))
    map_xb <- c(map_xb, sum(map_xb))
    
    uniques <- unique(map_xa, map_xb)
    
    difference <- abs(diff(uniques))
    
    sorted <- sort(difference)
    return(sorted)
    
  }
  
  
}

xa <- c(2,3,5,10)
xb <- c(3,7,10)
xab <- c(1,2,2,5,5,5)
DDP(xa, xb, xab)

PDP <- function(L){
  width <- max(L)
  x <- x[! x %in% width]
  x1 <- c(0, width)
  place(L, x1)
}

place <- function(L, x1){
  if (length(L) <- 0){
    return(x1)
    y <- max(L)
  }
  delta <- c()
  for (i in range(length(x))){
    delta[i] <- y -x1[i]
  } 
  if (delta %in% L){
    x <- append(x1, y)
    place(L, x1)
    x <- x1[x1! %in% y]
    L <- append(L, length(delta))
  }
  delta2 <- c()
  for (k in range(length(x1))){
    delta2[k] <- (max(L)-y) - x1[k]
  }
  if (delta2 %in% L){
    x1 <- append(x1, (max(L)-y))
    L <- L[L! %in% (max(L)-y)]
    place(L, x1)
    x1 <- x1[x1! %in% (max(L)-y)]
    L <- append(L, delta2)
  }
}





