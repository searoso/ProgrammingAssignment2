## R Programming Programming Assignment 2


## name: makeCacheMatrix
## purpose: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      matrix <- NULL
      start <- function(t) {
             x <<- t
             matrix <<- NULL
      }
      fin <- function() x
      # sets inverse of matrix
      inverseset <- function(solve) matrix <<- solve
      # inversing of matrix
      inverseget <- function() matrix
      # returns several functions
      list(set = start, get = fin,
           setinverse = inverseset,
           getinverse = inverseget)


}


## name: cacheSolve
## purpose: The function do the inverse of the special "matrix" which is returned by makeCacheMatrix we have above. 
## In case of inverse has already been calculated (and the matrix is the same), then the cachesolve 
## return the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Doing a matrix that is the inverse of x
        matrix <- x$getinverse()
        # Check if value is cached
        if(!is.null(matrix)) {
               message("wait for cached data")
               return(matrix)
        }
        data <- x$get()
        # do the inverse
        matrix <- solve(data, ...)
        # setting cache value
        x$setinverse(matrix)
        matrix
}
