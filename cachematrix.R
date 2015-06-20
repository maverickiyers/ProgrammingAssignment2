## This R code will cache the inverse of a square Mmatrix.

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## stores the cache value & initializes it to NULL
    cache_val <- NULL 
    ## creating the matrix in the working environment
    set <- function(m) {   
        x <<- m
        cache_val <<- NULL
    }
    ## Getting the value of the matrix
    get <- function() x
    ## Inverting the matrix and storing it in cache
    setInverse <- function(inv) cache_val <<- inv     
    ## Getting the inverted matrix from cache
    getInverse <- function() cache_val
    ## Returning the listed fuctions back into working environment
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
        )
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above. 

cacheSolve <- function(x, ...) {
    ## Getting the inverted matrix stored in cache
    cache_val <- x$getInverse()
    ## if inverted matrix is stored in cache, return it. Else, create it.
    if(!is.null(cache_val)) {
        message("getting cached data")
        return(cache_val)
    }
    ## Create matrix if it does not exist.
    matrix <- x$get()
    cache_val <- solve(matrix, ...)
    ## Storing inverted matrix back into cache.
    x$setInverse(cache_val)
## Return a matrix that is the inverse of 'x'
    return(cache_val)
}
