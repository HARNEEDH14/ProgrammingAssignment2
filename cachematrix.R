## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function is for caching

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
	x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(mean) inv <<- solve(x)
  getInv <- function() inv
  list(set = set, 
	   get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve function is for demonstrating the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv	
}
