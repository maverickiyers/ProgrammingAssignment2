## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cache_value <- NULL
    set_MatrixValue <- function(m) {
        x <<- m
        cache_value <<- NULL
    }
    get_MatrixValue <- function() x
    cache_InverseValue <- function(cm) {
        cache_value <<- cm        
    }
    get_InverseValue <- function() cache_value
    list(
        set_MatrixValue = set_MatrixValue,
        get_MatrixValue = get_MatrixValue,
        cache_InverseValue = cache_InverseValue,
        get_MatrixValue = get_MatrixValue
        )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    Matrix_Inverse <- x$get_MatrixValue()
    if(!is.null(Matrix_Inverse)) {
        message("getting cached data")
        return(Matrix_Inverse)
    }
    data <- y$get_MatrixValue()
    Matrix_Inverse <- solve(data)
    x$cache_InverseValue(Matrix_Inverse)
    ## Return a matrix that is the inverse of 'x'
    Matrix_Inverse
}
