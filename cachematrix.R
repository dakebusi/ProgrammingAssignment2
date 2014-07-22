## Implementation of a matrix object that supports
## caching its inverse

## This function returns a special matrix with the
## following internal functions:
## - set(y): set the matrix value to be y
## - get(): get the matrix
## - setinv(inv): set the inverse of the matrix to be inv
## - getinv(): get the invverse matrix
 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of matrix x.
## If the inverse has already been computed before
## and it is cached (x has not changed since), then
## the cached value is returned.
## Otherwise the inverse is computed by calling
## solve(x) and the results is cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinv()
        if (!is.null(i)){
		## Return cached value
                message("getting cached data")
                return (i)
        }

	## Compute inverse and cache it
        i <- solve(x$get(),...)
        x$setinv(i)
        i			   
}
