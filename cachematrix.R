#[ Put comments here that give an overall description of what your
## functions do

## This function adds a cache to a matrix to see if it has been inverted

makeCacheMatrix <- function(x = matrix()) {

   inv <- NULL
   set <- function(y) {
           x <<- y
           inv <<- NULL
   }
   get <- function() x
   setinv <- function(solve) inv <<- solve
   getinv <- function() inv
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Use the solve function on a cached matrix if it has no cached solution

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
   if (!is.null(m)) {
           message("getting cached data")
           return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinv(m)
   m
}
