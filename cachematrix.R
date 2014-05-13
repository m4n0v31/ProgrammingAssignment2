## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    matInverse <- NULL ## placeholder for the inverse
    
    set <- function(y) {
        x <<- y
        matInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matInverse <<- inverse
    getInverse <- function() matInverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    
}


## cacheSolve calculates the inverse of the special "matrix" created with 
## makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, it 
## gets the inverse from the cache and skips the computation.
cacheSolve <- function(x, ...) {
    matInverse <- x$getInverse() 
    if(!is.null(matInverse)) {   
        message("getting cached data")
        return(matInverse) 
    }
    data <- x$get() 
    matInverse <- solve(data, ...) 
    x$setInverse(matInverse) 
    matInverse 
}
