## Assingment2
## Matrix inversion by making use of Scoping rules of R

## Cache the inverse of a matrix, assuming the matrix has an inverse
makeCacheMatrix <- function(x = matrix()) {
      InverseM <- NULL
      set <- function(y) {
            x <<- y
            InverseM <<- NULL
      }
      get        <- function() x
      setInverse <- function(Inverse) InverseM <<- Inverse
      getInverse <- function() InverseM
      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Solve a matrix inverse or retrieve one by referring to 
## a matrix cache function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      InverseM <- x$getInverse()
      if(!is.null(InverseM)) {
            message("Getting cached inverse matrix")
            return(InverseM)
      }
      data <- x$get()
      InverseM <- solve(data)
      x$setInverse(InverseM)
      InverseM
}
