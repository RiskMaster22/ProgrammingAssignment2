## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# Below are two functions that are used to create a special object 
# that stores a numeric matrix and caches its inverse.
 
 
 
## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse. This special "matrix" object is really a list 
# containing a function to 
#     1. set the matrix
#     2. get the the matrix
#     3. set the inverse of the matrix
#     4. get the inverse of the matrix
 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() # cached inverse
    if(!is.null(m)){
        message("getting cashed inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
