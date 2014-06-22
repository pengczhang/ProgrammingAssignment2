## This program cache the inverse of a matrix, which is composed
## two parts

## Write a short comment describing this function
## first part of this function creates a matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverseSol) m <<- inverseSol
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse
         getInverse = getInverse
        )
}


## Write a short comment describing this function
## The second part retrives the inverse if it has already been calculated
## If it has not been calculated, it will compute the inverse of the 
## special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        message("Getting cached inverse data")
        return(m)    
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
