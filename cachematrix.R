## This R code will cache the inverse of a 2x2 Mmatrix.

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cache_value <- NULL
    set_MatrixValue <- function(m) {
        x <<- m
        cache_value <<- NULL
    }
    get_MatrixValue <- function() x
    set_InverseValue <- function(cm) cache_value <<- cm        
    get_InverseValue <- function() cache_value
    list(
        set_MatrixValue = set_MatrixValue,
        get_MatrixValue = get_MatrixValue,
        set_InverseValue = set_InverseValue,
        get_InverseValue = get_InverseValue
        )
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above. 

cacheSolve <- function(x, ...) {
    cache_value <- x$get_MatrixValue()
    if(!is.null(cache_value)) {
        message("getting cached data")
        return(cache_value)
    }
    data <- x$get_MatrixValue()
    cache_value <- solve(data, ...)
    x$set_InverseValue(cache_value)
## Return a matrix that is the inverse of 'x'
    cache_value
}
