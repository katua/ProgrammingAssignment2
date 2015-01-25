## These two functions were created mostly based on the example given by Roger 
## Pang (Caching the Mean of a Vector). They can be used to create a special 
## object that stores a matrix and its inverse. 
 

## The function below creates a list containing a function to set and get the value 
## of the matrix, to set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The following function calculates the inverse of the matrix created by the 
## function above. First, it checks whether the inverse has already been 
## calculated and, if so, it gets inverse's value and returns it without computation.
## Otherwise, it computes the inverse, saves its value in cache and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
