## This R file contains two functions for computing and caching an inverse of a given matrix.

## The first function - makeCacheMatrix() - returns a list containing functions for setting
## and getting the matrix together with functions for setting and getting the inverse
## of stored matrix.
## Second function - cacheSolve() - returns the inverse of the matrix (stored as a first
## element of the list created by makeCacheMatrix()); it either computes it and store to
## the list, or gets the stored inverse. 


## The function makeCacheMatrix() takes a matrix as an argument (default value is an empty
## matrix) and returns a list containing functions get(), set(), getinv(), setinv().
## The set() functions takes a matrix as an argument and stores it into the variable x in
## the environment of the list; the get() function returns the value of this x.
## Similarly the setinv() and getinv() functions store and load an inverse of the matrix
## stored in x (however they DOES NOT calculate it explicitly); the inverse is stored
## in the variable inverse in the environment of the list. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
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


## Function cacheSolve() takes as an argument the list produced by function makeCacheMatrix()
## and returns the inverse of the respective matrix. The resulting inverse is either
## explicitly computed and stored by the setinv() function, or loaded by the getinv()
## function (in this case a corresponding message is displayed to the user).

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
