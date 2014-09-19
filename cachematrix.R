## The following two functions create a special matrix and compute its
## inverse. If the inverse has already been calculated(and matrix has not
## changed), then 'cachesolve' retrieve the inverse from the cache.

## 'makeCacheMatrix' creates a special 'matrix' which is really
## a list containing functions to set and get the value and inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Take an empty matrix 'x' as an input and
        ## Return a special 'matrix' as a list
        
        inverse <- NULL
        
        setValue <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        getValue <- function() x
        
        setInverse <- function(inv) inverse <<- inv
        
        getInverse <- function() inverse
        
        list(setValue = setValue, getValue = getValue,
             setInverse = setInverse, getInverse = getInverse)
}

## 'cacheSolve' returns the inverse of a special matrix by using
## 'cached' inverse if it exists. Otherwise it would calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Take a special matrix 'x' as an input and
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        
        if (!is.null(inv)) {
                message("Getting cached inverse!")
                return(inv)
        }
        ## else we need to compute inverse of 'x' by using 'solve' function
        
        matrix_data <- x$getValue()
        inv <- solve(matrix_data)
        x$setInverse(inv)
        inv
}