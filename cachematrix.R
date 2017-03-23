## See assignment description 

## makeCacheMatrix --> store one matrix and its inverted version
##      setmatrix --> input a matrix
##      getmatrix --> read the matrix
##      setinverse --> input the inverted matrix, computed outside the function
##      getinverse --> read the inverted matrix


## cacheSolve reads the stored matrix or, if the stored inverted matrix is not
## avalaible, it computes it.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$getmatrix()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
