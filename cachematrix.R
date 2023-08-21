## Allows creation of a matrix which can cache its inverse to avoid the need to
## recompute

## Creates a special 'matrix' which is able to cache its own
## inverse via solve. It defines an variable s which contains the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Returns the cached inverse of a matrix, or calculates it if no cached value
## can be found

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)){
            message("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
}
