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

makeCacheMatrix <- function(mat = matrix()) {
  cachedInverse <- NULL # the get/setInv functions use this variable, its in their parent scope
  
  set <- function(newMat) {
    mat <<- newMat
    # clear the cachedInverse since we have a new matrix and the inverse has not been requested yet
    cachedInverse <<- NULL 
  }
  
  get <- function() mat
  
  setInv <- function(inv) cachedInverse <<- inv
  
  getInv <- function() cachedInverse
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve returns the inverse of x.
## If the inverse has already been calculated it returns the previous calculation
## result, otherwise it calculates the inverse and caches it in x

cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'mat' either by
  ## reusing a cachedInverse or calculating it first (and caching)
  cachedInverse <- mat$getInv()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  data <- mat$get()
  cachedInverse <- solve(data, ...)
  mat$setInv(cachedInverse)
  cachedInverse
}
