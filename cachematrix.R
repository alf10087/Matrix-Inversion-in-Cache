# This script uses the lexical scoping rules of R to cache a potentially time-consuming computation: matrix inversion.

rm(list=ls())

## The first function makes the list of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This is the function that checks the cache and finds it if it is stored.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#### Run an example to see if it has worked, I use a 2x2 random normal matrix.

n <- matrix(rnorm(4), 2, 2)
a <- makeCacheMatrix(n)
n_inv <- solve(n)
a$setinverse()
a$getinverse()
cacheSolve(a)

#### Test to see if everything is fine. If this is true, it has worked alright.

n_inv == cacheSolve(a)
