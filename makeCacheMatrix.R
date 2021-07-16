makeCacheMatrix <- function(x=matrix()){
    x_invrse <- NULL
    set <- function(y){
    x <<- y
    x_invrse <<- NULL
  }
      get <- function() x
      setinverse <- function(inverse) x_invrse <<- inverse
      getinverse <- function() x_invrse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}




cacheSolve <- function(x, ...) {
  
     x_invrse <- x$getinverse()
     if(!is.null(x_invrse)) {
          message("getting cached data")
          return(x_invrse)
  }

    data <- x$get()
    x_invrse <- solve(data, ...)
    x$setinverse(x_invrse)
    x_invrse
}
