## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix (x) as input and creates an object with
## four functions to get or set the matrix value, and get or set the inverse 
## matrix. The inverse is initialized as NULL and would be set using cacheSolve
## It is identical to the makeVector() example, but 'm' and 'mean' are renamed 
## 'inv' and 'inverse'.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes a makeCacheMatrix matrix object as input and returns its 
## inverse. It uses the object's getinv function to get the cached inverse 
## matrix and return it if not null. If it is null, it solves for the inverse 
## and uses setinv to cache that inverse before returning it. This is identical 
## to the cachemean example, except that instead of calculating the mean, we 
## use the solve function to get the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
