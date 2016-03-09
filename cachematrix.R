# Compute the Inverse of a Matrix and store in cache, if not exists in cache yet.
# If Inverse already available in cache, retrieve and do not compute again.

# makeCacheMatrix provides functions for cache operations
makeCacheMatrix <- function(cacheMatrix = matrix()) {

  cacheMatrixInverse = NULL
  
  # store regular input matrix into cache
  set <- function(inputMatrix) {
    cacheMatrix <<- inputMatrix 
    cacheMatrixInverse <<- NULL
  }

  # retrieve regular matrix from cache
  get <- function() cacheMatrix

  # store inverse matrix into cache
  setinverse <- function(inputMatrix) cacheMatrixInverse <<- inputMatrix

  # retrieve inverse  matrix from cache
  getinverse <- function() cacheMatrixInverse
  
  # list of cache operations
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Retrieve or calculate Inverse Matrix
cacheSolve <- function(inputMatrix, ...) {

  # Try to retrieve Inverse Matrix
  MatrixInverse <- inputMatrix$getinverse()

  # If result is available (not null), the Inverse calculation was already done
  if(!is.null(MatrixInverse)) {
    message("Inverse Matrix already calculated, retrieving from cache.")

    # Exit the function
    return(MatrixInverse)
  }

  message("Inverse Matrix not available yet, calculating and storing into cache.")
  
  # Retrieve the regular matrix from the cache
  MatrixRegular <- inputMatrix$get()
  
  # Perform the Solve operation to caluculate the Inverse
  MatrixInverse <- solve(MatrixRegular)
  
  # Store results into Inverse Cache
  inputMatrix$setinverse(MatrixInverse)
  
  # Return value of the function is the Inverse Matrix
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
