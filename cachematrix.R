## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# makeCacheMatrix creates a list containing a function to
# a) set the value of the matrix
# b) get the value of the matrix
# c) set the value of inverse of the matrix
# d) get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_matrix <<- inverse
        getinverse <- function() inv_matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

# The following function returns the inverse of the matrix. It 1st checks if
# the inverse has already been computed. If yes, then it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix)) {
                message("getting cached data.")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data)
        x$setinverse(inv_matrix)
        inv_matrix
}
