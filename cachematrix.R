## Matrix inversion is usually a costly computation 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

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

## The following function returns the inverse of the matrix.
## If the inverse has already been computed, it skips the computation.
## If not, it computes the invers and sets the value in the cache
## using the setinverse function.

cacheSolve <- function(x, ...) {
    
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
