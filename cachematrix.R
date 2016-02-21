## The function creates a special “matrix” object that can cache its inverse.
## makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {## changes the vector stored in the main function
    x <<- y
    m <<- NULL
  }
  get <- function() x## returns the vector x stored in the main function
  setinverse <- function(solve) m <<- solve ## store the value of the input in a variable m 
  getinverse <- function() m                ## into the main function and return it
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function computes the inverse of the special “matrix” returned by privious function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix,
## m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.

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
