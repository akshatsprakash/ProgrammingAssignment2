## Author: ASP                     Date: 23-Jan-2016

## File created as part of Coursera R-Programming, Week 3
## Assignment to be assessed by course peers
## Creation and submission via GitHub


## function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # nothing in beginning
        set <- function(y) {
                x <<- y
                inv <<- NULL # reset 
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() # does it exist in cache? 
        if(!is.null(inv)) {
                message("getting cached inverse data") 
                return(inv) # it does exist, look no further. Hurrey !
        }

        data <- x$get() # is not cached...
        inv <- solve(data, ...) # work to do here and ...
        x$setInverse(inv) # save the result in x's cache
        message("caching inverse data") 
        inv # return
}