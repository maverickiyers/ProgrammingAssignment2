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
        get_MatrixValue = get_MatrixValue
        )
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above. 

cacheSolve <- function(x, ...) {
    Matrix_Inverse <- x$get_MatrixValue()
    if(!is.null(Matrix_Inverse)) {
        message("getting cached data")
        return(Matrix_Inverse)
    }
    data <- x$get_MatrixValue()
    Matrix_Inverse <- solve(data, ...)
    x$set_InverseValue(Matrix_Inverse)
## Return a matrix that is the inverse of 'x'
    Matrix_Inverse
}
