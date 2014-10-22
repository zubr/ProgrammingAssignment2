## The below two functions create a special object that stores a matrix and
## caches its inverse so that when the user needs it again, it can be taken
## from the cache rather than computed again.

## The first function, makeCacheMatrix, creates a special object that stores
## a matrix.

makeCacheMatrix <- function(x = matrix()) {
s <- NULL ## variable s in the global environment is set to NULL (assignment
set <- function(y) { ## operator)
    x <<- y
    s <<-NULL ## local variable s is created (in the containing environment
} ## - super-assignment operator)
get <- function() x
setinverse <- function(solve) s <<- solve
getinverse <- function() s
list(set=set, get=get,
     setinverse=setinverse,
     getinverse=getinverse)
}

## The second function, cacheSolve, calculates the inverse of the special
## "matrix" created by the previous function. First, it checks whether the
## inverse has already been calculated. If it is the case, then it retrieves
## the inverse from the cache. If not, it calculates the inverse and sets
## the resulting inverse matrix in the cache.

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
       message ("getting cached data") 
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
