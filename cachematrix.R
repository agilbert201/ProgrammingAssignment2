## These functions provide clients the ability to create a special
## matrix that can cache it's inverse. Use 'makeCacheMatrix' to 
## create the cached matrix, and 'cacheSolve' as an interface 
## to get it's inverse.
##
## Example:
##  m <- matrix(c(1,2,3,4),2,2)
##  cm <- makeCacheMatrix(m)
##  for(i in 1:10)
##      cacheSolve(cm)
makeCacheMatrix <- function(x = matrix()) {
    ## 'makeCacheMatrix' creates a special matrix which allows
    ## clients to cache it's inverse
    ##
    ## Args:
    ##  x: optionally specify the matrix, note assumed to be invertible
    ##    
    ## Returns:
    ##  A list with functions to get,set the matrix and get,set it's inverse
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(theInv) inv <<- theInv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## 'cacheSolve' returns the inverse of 'x'
    ##
    ## Args:
    ## x: the special matrix x, note must be created through makeCacheMatrix
    ##
    ## Returns:
    ##  the inverse of x
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    message("solving and caching new inverse")
    data <- x$get()
    theInv <- solve(data)
    x$setinv(theInv)
    theInv
}
