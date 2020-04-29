
## Below are a pair of functions that cache the inverse of a matrix and give its inverse.

makeCacheMatrix <- function(x = matrix()) {

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) {
    x<<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set= set,
       get=get,
       setinverse= setinverse,
       getinverse= getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv2 <- x$getinverse()
  if(!is.null(inv2)) {
    message("getting cached data")
    return(inv2)
  }
  mat<- x$get()
  inv2 <- solve(mat, ...)
  x$setinverse(inv2)
  inv2
}


