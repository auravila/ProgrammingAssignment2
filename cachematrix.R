## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      InverseM <- NULL
      set <- function(y) {
            x <<- y
            InverseM <<- NULL
      }
      get        <- function() x
      setInverse <- function(solve) InverseM <<- solve
      getInverse <- function() InverseM
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting inverse matrix")
            return(m)
      }
      data <- x$get()
      m <- solve(x, ...)
      x$setinverse(m)
      m
}
