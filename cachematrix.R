## Compute a Matrix Inverse with caching. 

## makeCacheMatrix - creates a special "matrix" object that can cache its inverse.
## Returns a list containing functions to: 
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse 
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

## cacheSolve -  computes the inverse of the special "matrix" returned by m
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
