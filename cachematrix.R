## By means of these two functions we enable to get a caching matrix inverse.
## The first function creates a special "matrix" object that can cache its 
## inverse.
## The second function computes the inverse of the special "matrix" returned by 
## the first function.


## This function returns a list through which we can set/get the matrix data and
## its inverse (if this one has been previously made). The returned list 
## contains four functions:
##      set: it sets the matrix values
##      get: it gets the matrix values
##      setInverse: it sets the inverse of the matrix that is taken into account
##      getInverse: it gets the inverse of the matrix that is taken into account

makeCacheMatrix <- function(x = matrix()) {
        ## return a list composed of four functions for managing a cacheMatrix
        
        # We initialize the inverse value to NULL
        inverse <- NULL
        
        # set() function
        set <- function(y) {
                x <<- y
                # Always we set the matrix values, the inverse is set to NULL
                inverse <<- NULL
        }
        # get() function
        get <- function() x
        # setInverse() function
        setInverse <- function(inverseArg) inverse <<- inverseArg
        # getInverse() function
        getInverse <- function() inverse
        
        # Finally, we make a list with the four functions that is the result
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve gets the inverse from the cache
## that had previously stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Firstly, we get the inverse of the special matrix
        inverse <- x$getInverse()
        # If such inverse already exists
        if(!is.null(inverse)) {
                # we return such cache inverse version
                message("getting cached Inverse...")
                return(inverse)
        }
        # Otherwise, we get the current matrix,
        data <- x$get()
        # we compute its inverse
        inverse <- solve(data, ...)
        # and we set such inverse for future computations under the same matrix
        x$setInverse(inverse)
        # Finally, we return the inverse matrix
        inverse
}
