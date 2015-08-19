## These functions create a matrix and cache the inverse of matrix. If the inverse of the matrix recalled, it 
## will simply return the cached value that has been calculated.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has
## already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'z'
  m <- z$getinverse()
  if(!is.null(m)) {
    message("getting cashed data")
    return(m)
  }
  data <- z$get()
  m <-solve(data,...)
  z$setinverse(m)
  m
}
