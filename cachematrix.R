## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix stores a cached variable and defines get/set functions,
#   returning a list of these functions
# cacheSolve get the cached value if possible, othewise it applies the 
#  function solve() to a matrix. It assumes it is given a makeCacheMatrix object

## Write a short comment describing this function
# cache the inverse of a matrix by using get/set functions
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getinverse <- function() inverse
    setinverse <- function(inv) {
        inverse <<- inv
    }
    list(get=get,
         set=set,
         getinverse=getinverse,
         setinverse=setinverse)
}


## Write a short comment describing this function
# apply solve() to a makeCacheMatrix object, cacheing if necessary
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        print("getting cached copy of inverse")
        return(x$getinverse())
    }
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}
