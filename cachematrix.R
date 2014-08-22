## These functions fulfill Programming Assignment 2. The functions
## save time by caching the values of time intensive calculations. In
## this case, the inverse of a matrix is calculated and cached. 

## The first function creates a matrix, then creates a series of functions.

makeCacheMatrix <- function(x = matrix()) { #create special matrix
    inv<- NULL # set variable to NULL
    set<- function(y) { # create set function
      x<<- y # superassign x to y
      inv<<- NULL # superassign inv to NULL
    }
    get<- function() x # create get function, return x
    setinv<- function(inverse) inv<<- inverse # create setinv function
    getinv<- function() inv # create getinv function
    list(set = set, get = get, setinv = setinv, getinv = getinv) 
    # create and return list
}

## The following function calculates the inverse of the matrix
## First the function checks to see if the inverse has been
## calculated and stored previously. If it has, it skips the computation,
## displays a message and returns the inverse. If if has not, it calculates
## the inverse caches the value and returns the inverse. 

cacheSolve <- function(x, ...) { 
        inv<- x$getinv() # get inverse value
        if(!is.null(inv)) { # test if inverse has been calculated 
            message("getting cached data") # if defined, display message
            return(inv) # display inverse and end function
        }
        data<- x$get() # if not defined, get matrix 
        inv<- solve(data, ...) # calc inverse
        x$setinv(inv) # cache inverse 
        inv # display inverse 
}
