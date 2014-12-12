## These two functions together implement a fast matrix inversion method
## Given a matrix it stores its inverse the first time it is computed. Subsequent retrievals of the inverse
## retrieve the inverse matrix directly from the cache.
##
## Use example:
## matrix <- matrix(c(1,2,3,4),nrow=2,ncol=2))
## cache <- makeCacheMatrix(matrix)
## inverse <- cacheSolve(cache) ## first time
## inverse <- cacheSolve(cache) ## second time faster


## The function makeCacheMatrix(x) creates a cache data structure to hold the matrix and its cached inverse 
##
## Arguments:
## x : The matrix
##
## Returns:
## The data structure, implemented as a list with the following elements:
## x               : The matrix data
## set(y)          : a function that sets the matrix data an initializes the cache
## get()           : a function that retrieves the matrix data as a matrix class
## setInv(inverse) : a function that stores the matrix inverse in the cache
## getInv()        : a function thatretrieves the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## sets the matrix data
  set <- function(y= matrix()){
    x <<- y
    inv <<- NULL
  }
  
  ## gets the matrix data
  get <- function() x
  
  ## functions used by cacheSolve 
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  list (set=set, get=get, setInv=setInv, getInv=getInv)

}


## The function cacheSolve(x, ...) implements the cache logic for the inverse matrix:
## If the inverse matrix is already cached it returns the cached inverse matrix,
## otherwise it computes the inverse matrix, stores it in the cache, and returns the computed inverse matrix
##
## Arguments:
## x   : the matrix cache  (use makeCacheMatrix to create a matrix cache)
## ... : optional arguments to be passed to the solve method
##
## Returns:
## The inverse matrix of x

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("Getting cached data ...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
