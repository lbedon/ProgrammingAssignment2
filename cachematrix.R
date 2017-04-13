## makeCacheMatrix generates a list to set the matrix an the inverse of it, 
## and let you read trough get and getinverse the cached data.

## makeCacheMatrix creates a list-vector with the functions set, setinverse,
## get, getinverse. 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function, which returns the inverse to 'x'. Before 
## solving the matrix, the function checks if this operation has been
## already done for the given matrix. If this is true, cacheSolve gives the 
## cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
