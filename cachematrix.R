## These functions provide a way for caching matrix inverses. This means that
## repeated uses of the inverse of the same matrix can be reused without
## calculating it again. 
##
## Example:
## > cachedMatrix <- makeCacheMatrix(matrix(c(2,0,0,2),nrow=2,ncol=2))
## > inverse <- cacheSolve(cachedMatrix)
##
## Repated calls to cacheSolve(cachedMatrix) will return the same inverse
## without calculating it again

## makeCacheMatrix is a factory function to create an cached matrix object.
## The object has a set/get interface to access the underlying R matrix() 
## value and a getInv/setInv interface to do the same for the inverse matrix() value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve returns the inverse of x.
## If the inverse has already been calculated it returns the previous calculation
## result, otherwise it calculates the inverse and caches it in x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
