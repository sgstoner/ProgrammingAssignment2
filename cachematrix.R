## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly . The following pair of functions 
## cache the inverse of a matrix by first creating a special "matrix" that can cache it's inverse
## and then computing the inverse of the special "matrix" that was previously created.  If the
## inverse has already been calcuated and the matrix has not changed, then the secon function retrieves
## the inverse from the cache instead of performing the calcuations again.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## returns a list of the functions to:
    ## set the matrix - 1
    ## get the matrix - 2
    ## set the inverse - 3
    ## get the inverse - 4
  
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function uses the output of makeCacheMatrix to compute the
## inverse of the "matrix" created by makeCacheMatrix.  If the inverse
## has already been calculated, it will use the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
