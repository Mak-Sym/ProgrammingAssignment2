## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly. 
## Functions below provide data and special wrapper for performing "cached computation" of the inverse of a matrix.


## This function creates a special "matrix" object that can cache some computed values.
## Created object does not put any expectations on what value should be cached, so it's up to caller to decide of what to put in cache
## However this function contains validation of the input parameter, which expected to be a matrix (otherwise an error is thrown)
makeCacheMatrix <- function(x = matrix()) {
    if(!is.matrix(x)) {
        stop("Argument is not a matrix!")
    }
    
    cache <- NULL;
    
    get <- function() x
    
    set <- function(y) {
        if(!is.matrix(y)) {
            stop("Argument is not a matrix!")
        } else {
            x <<- y;
            cache <<- NULL;
            invisible(cache)
        }
    }
    
    getcache <- function() cache
    
    setcache <- function(newcache) cache <<- newcache
    
    list(get = get, set = set, getcache = getcache, setcache = setcache)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invertedmatrix <- x$getcache()
    if(!is.null(invertedmatrix)) {
        message("getting cached data")
        return(invertedmatrix);
    }
    
    data <- solve(x$get(), ...)
    x$setcache(data)
    data
}
