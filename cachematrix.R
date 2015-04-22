## The set of functions implement caching of the matrix inverse
##  1. The user will first use makeCacheMatrix to create a list object
##     that will contain the matrix itself and its inverse if already computed
##  2. Whenever the inverse of the matrix is needed, the user will use
##     cacheSolve function that (computes if necessary and) returns its inverse


## Function 'makeCacheMatrix' generates a list object that can hold
##   (1) the (invertible) matrix
##   (2) the inverse of the matrix if already computed
##   (3) member functions to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
	xinv <<- NULL
    }
    get <- function() x
    setinverse <- function(xinverse) xinv <<- xinverse
    getinverse <- function() xinv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Function 'cacheSolve' (computes if necessary) and returns the matrix inverse
## The function argument should be the object created by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinverse()
    if(!is.null(xinv)) {
        message("getting cached inverse")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinverse(xinv)
    xinv
}
