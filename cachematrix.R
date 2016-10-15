## This is a set of functions that introduces caching of matrix inverse results.

## This function allows to create instances of cache-enabled matrices 
## from common square invertible matrix.

makeCahceMatrix <- function(original = matrix()) {
  inverce <- NULL
  set <- function(value) {
    original <<- value
    inverce  <<- NULL
  }
  get <- function() {return(original)} 
  
  setInverse <- function(solve) {inverce <<- solve}
  
  getInverse <- function() {return(inverce)}
  
  list(
    get = get,
    set = set,
    getInverse = getInverse,
    setInverse = setInverse
  )
}

## This function can resolve inverse of 'makeCacheMatrix' matrixes utilizing caching functionality of src. object.
## @cacheableMatrix => expects makeCahceMatrix instance.

cacheSolve <- function(cacheableMatrix, ...) {
  ## Return a matrix that is the inverse of 'cacheableMatrix'
  inverce <- cacheableMatrix$getInverse()
  if (is.null(inverce)) {
    message("calculating invecrce")
    original <- cacheableMatrix$get()
    inverce <- solve(original, ...)
    cacheableMatrix$setInverse(inverce)
  }
  else{
    message("getting cached data")
  }
  return(inverce)
}
