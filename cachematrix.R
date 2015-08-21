## First call this function and assign it into a "Value"
## This function sets a null Inverse matrix initially
## thru $strt contained within it.
## the $fetch would fetch the matrix value
## $calcinv will calculate the inverse of matrix
## $oldinv will retrieve the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
  make_inv <- NULL   #intitialize the inverse matrix
  
  strt <- function(y) {
    x <<- y
    make_inv <<- NULL
  }
  # matrix fetched here
  fetch <- function() x
  
  # function to set inverse andstore in make_inv
  calcinv <- function(inverse) make_inv <<- inverse
  # function to check cache for inverse
  oldinv <- function() make_inv
  
  #
  list(strt = strt, fetch = fetch, calcinv = calcinv, oldinv = oldinv)
}

## call function cachesolve with the "Value" made by makeCacheMatrix
## will first populate make_inv by calling $oldinv of makeCacheMatrix
## if make_inv is not empty then display data from cache
## else compute from scratch and cache it.

cacheSolve <- function(x, ...) {
  
  make_inv <- x$oldinv()
  
  
  if (!is.null(make_inv)) {
    message("fetching cached data")
    return(make_inv)
  }
  
  
  data <- x$fetch()
  make_inv <- solve(data, ...)
  
  
  x$calcinv(make_inv)
  
  # Return
  make_inv
}