makeCacheMatrix <- function(x=matrix()){
    invrse <- NULL
    set <- function(y){
          x <<- y
          invrse <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {invrse <<- inverse}
    getInverse <- function() {invrse}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
    invrse <- x$getInverse()
    if(!is.null(invrse)){
      message("getting cached data")
      return(invrse)
    }
    mat <- x$get()
    invrse <- solve(mat, ...)
    x$setInverse(invrse)
}