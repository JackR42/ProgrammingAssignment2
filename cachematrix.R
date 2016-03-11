# Compute the Inverse of a Matrix and store in cache, if not exists in cache yet.
# If Inverse already available in cache, retrieve and do not compute again.

# makeCacheMatrix: provides functions for cache operations
makeCacheMatrix <- function(cacheMatrix = matrix()) {

  cacheMatrixInverse = NULL
  
  set <- function(inputMatrix) {
    cacheMatrix <<- inputMatrix 
    cacheMatrixInverse <<- NULL
  }

  get <- function() cacheMatrix

  setinverse <- function(inputMatrix) cacheMatrixInverse <<- inputMatrix

  getinverse <- function() cacheMatrixInverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve: Retrieve or calculate Inverse Matrix
cacheSolve <- function(inputMatrix, ...) {

  MatrixInverse <- inputMatrix$getinverse()

  if(!is.null(MatrixInverse)) {
    message("Inverse Matrix already calculated, retrieving from cache.")

    return(MatrixInverse)
  }

  message("Inverse Matrix not available yet, calculating and storing into cache.")
  
  MatrixRegular <- inputMatrix$get()
  
  MatrixInverse <- solve(MatrixRegular)
  
  inputMatrix$setinverse(MatrixInverse)
  
  MatrixInverse
}

## Testing:

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
