## Below are a set of functions that find the inverse of a matrix
## To improve speed, one function caches the inverse of the matrix
##      as the other generates it, enabling it to be called later from
##      the cache rather than re-calculating the inverse
##
## For optimal results, set a matrix with your desired values
## Then, set another variable as the result of the makeCacheMatrix
##      function on the initial matrix
## Then, run the cacheSolve() function on that last variable
##
## Example:
##      samplematrix <- matrix(1:4, 2,2)
##      samplematrixObject <- makeCacheMatrix(samplematrix)
##      cacheSolve(samplematrixObject)

## Provided with an invertible matrix, determines if its inverse
## has already been determined

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        ## Allows us to set a new value for the object, if desired
        set <- function(newValue) {
                x <<- newValue
                inverse <<- NULL
        }
        get <- function() x
        ## Accessed by cacheSolve during first run-through and stores value
        ##      in cache for later use
        setInverse <- function(writeInverse) inverse <<- writeInverse
        ## Returns cached value during subsequent runs of cacheSolve
        getInverse <- function() inverse
        ## Allows cacheSolve to find and identify functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)        
}


## Finds the inverse of an invertible matrix and returns the value
## Unless the inverse has already been calculated and stored in the cache
## In which case it returns the stored value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        ## Checks to see if we already have the inverse in our cache
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## If the inverse is not known, find the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        ## Set the inverse in our cache for the future
        x$setInverse(inverse)
        ## Return results to the user
        inverse
}