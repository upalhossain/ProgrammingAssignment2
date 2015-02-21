## This file contains 2 functions: makeCacheMatrix() and cacheSolve
## These functions create matrix objects and calculates the inverse 
## of a given matrix


## This function creates a special "matrix" object that can cache its inverse. 
## The function returns the cached inverse of the matrix when called, if the 
## result already exists
makeCacheMatrix <- function(x = matrix()) {

	## initialize the inverse matrix
	m <- NULL
	
		## setter function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
    	## getter function
        get <- function() x
        
        ## set and get the inverse
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        ## store the results inside a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix function. The function retrieves the inverse from 
## the cache if it is available, otherwise it computes the inverse. 
cacheSolve <- function(x, ...) {

        ## get the invesrse of matrix 
        m <- x$getinverse()
        
        ## check to see if the matrix returned is not empty
        ## if it has data, then return the matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## otherwise, call the solve function to get the inverse matrix and set it
        data <- x$get()
        m <- solve(data) %*% data
        
        ## set the inverse result after solving it and return the result
        x$setinverse(m)
        m
}
