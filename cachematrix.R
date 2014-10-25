## Programming Assignment 2 for R Programming
## There are two matched functions here:
##   makeCacheMatrix: a "constructor" function that sets up
#                     a value that can cache the results
#                     of an operation (in this case the matrix
#                     inverse)
#
##   cacheSolve: returns the inverse of the given matrix.
#                Returns the cached value if available,
#                otherwise calculating and storing the value
#                for later retrieval.

## makeCacheMatrix: Returns a list with methods that can be used to store,
## and retrieve matrix data and cache the results of the inverse operation.

makeCacheMatrix <- function(x = matrix()) {
        # This value is our cache - it stores the cached value,
        # is set to null if no value has been calculated yet.
        inv <- NULL

        # Reset the value of the stored matrix. Also
        # invalidates the cache (since the matrix has changed)
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }

        ## return the current matrix
        get <- function () x

        ## Store the calculated inverse in the cache
        setInverse <- function (inverse) inv <<- inverse

        ## and retrive the cached inverse
        getInverse <- function () inv

        ## Construct the returned list with the four operations above.

        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: Calculate the inverse of the given matrix x,
## using the cached value if it's available

cacheSolve <- function(x, ...) {
        # Retrieve cached data
        inv <- x$getInverse()

        # is there anything in the cache?
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # no cached data, calculate it
        data <- x$get()
        inv <- solve(data)

        # store it back in the cache for later
        x$setInverse(inv)

        # and return calculated value
        inv
}
