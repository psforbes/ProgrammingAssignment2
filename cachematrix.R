## These functions fulfill Programming Assignment 2. The purpose of the functions
## is to save time by caching the values of time intensive calculations. In
## this case, the inverse of a matrix is calculated and stored. 

## This function creates a matrix, then defines a series of functions
## These functions set the value of the matrix, get the value of the matrix, 
## set the value of the inverse of the matrix and get the value of the inverse 
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(y) {
      x<<- y
      inv<<- NULL
    }
    get<- function() x
    setinv<- function(inverse) inv<<- inverse
    getinv<- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function first checks to see if the inverse of the matrix
## has been calculated. If is has, it skips the computation and returns the 
## inverse of the matrix. If not, it proceeds with the calculation and sets
## the value of the inverse matrix. 

cacheSolve <- function(x, ...) {
        inv<- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data<- x$get()
        inv<- solve(data, ...)
        x$setinv(inv)
        inv
}
