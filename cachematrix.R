## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix creates a matrix
## and a list of 4 functions (set, get, setinvx, getinvx)

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize matrice invx
  invx = matrix()
  invx <- NULL
  
  ## Function set caches the matrix in the parent environment (<<- operator)
  ## and re-initialize the inverse matrix in the parent environment(<<- operator)
  ## This function is used to inverse a new matrix without running again
  ## the makeCacheMatrix
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  ## Function get retrieves matrix x
  get <- function() x
  
  ## Function setinvx(inverse) caches matrix inverse as invx
  ## in the parent environment
  setinvx <- function(inverse) invx <<- inverse
  
  ##Function getinvx() retrieves inverse matrix invx
  getinvx <- function() invx
  
  ## Generate the list of functions
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)
}


## Function cacheSolve returns a matrix that is the inverse of x.
## If the inverse of x has already been computed the function retrieves
## the inverse from the cache.
## If the inverse of x is not in the cache, the inverse is computed
## and stored in the cache.

cacheSolve <- function(x, ...) {
  
  ## Test if the inverse of x is cached
  invx <- x$getinvx()
  if(!is.null(invx)) {
    message("getting cached data") 
  } else {
    message("computing and caching the inverse matrix")
    ## Retrieve x into data
    data <- x$get()
    ## Compute the inverse matrix into invx
    invx <- solve(data, ...)
    ## Put invx in the caches
    x$setinvx(invx)
  }
  ## Return the cached or computed inverse of x
  return(invx)
}
