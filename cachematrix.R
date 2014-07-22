## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix", which is really a list 
## containing a function to get/set value of the matrix,
## and get/set value of its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        ## if the matrix changes, it resets the inverse value.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}


## Calculates the inverse of the special "matrix" 
## created with the above function.
## First checks to see if the mean has already been calculated
## If so, its gets the inverse from the cache. 
## Otherwise, its calculates the inverse and sets the value for
## another time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
