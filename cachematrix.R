## Put comments here that give an overall description of what your
## functions do

## cacheSolve takes a special "matrix" object created by makeCacheMatrix and 
## returns the inverse of the "matrix". If the inverse is cached, it is return
## from cache. If the inverse is not already cached, it is computed using 
## the "solve" R function, saved to the cache (to be used for the next time) 
## and returned.

## Some test data
## test_mat <- matrix(rnorm(36,0,1),6,6)
## test_mat2 <- matrix(rnorm(49,0,2),7,7)
## input_mat <- makeCacheMatrix(test_mat)
## input_mat2 <- makeCacheMatrix(test_mat2)

## Write a short comment describing this function
## makeCacheMatrix: makes a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## variable to hold cached value, default to NULL
        inv <- NULL
        ## calling function with matrix y will set x <<- y, inv <<- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get will return the matrix x
        get <- function() x
        ## setsolve will save the inverse matrix into inv
        setsolve <- function(solve) inv <<- solve
        ## getsolve will return the inverse matrix stored in inv
        getsolve <- function() inv
        ## returns a list of functions that can be called by another function
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## cacheSolve: computes the inverse of the input "matrix" object 
## and save it for reuse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get inverse from x (if available)
        inv <- x$getsolve()
        ## if inverse is available (in cache), return message and inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if inverse is not available, get the original matrix
        data <- x$get()
        ## run solve() on the matrix to get the inverse, save to inv
        inv <- solve(data, ...)
        ## store inverse (inv) to cache
        x$setsolve(inv)
        ## return the computed inverse
        inv
}
