## The functions below calculate the inverse of an invertible matrix and store the solution for future use.
## The stored solution is retrieved by calling the cacheSolve function.

## makeCacheMatrix creates a list with the input matrix values and stores the calculated inverse matrix values from the cacheSolve 
## function. If the inverse not calculated, the solution is stored as NULL.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function checks whether the inverse of the given matrix exists and retrieves the value from makeCacheMatrix if it does. 
## If the solution does not exist, it calculates the inverse and send the values to makeCacheMatrix to be stored.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                inv

        }
        data <- x$getmat()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
