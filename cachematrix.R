## R Programming Assignment #2

## makeCacheMatrix is called with an invertible matrix and returns a list of functions that can
## be used with the cacheSolve function to return the inverse of the matrix if it already exists,
## otherwise it calculates it, sets it (in the parent environnent), and then returns it.

## The list contains four functions to manipulate two variables in the parent environment:
## 1. set, which sets the value of the matrix (x) in the parent environment.
## 2. get, which gets the value of the matrix (x).
## 3. setInverse, which calculates the inverse of the matrix (i) in the parent environment.
## 4. getInverse, which returns the inverse of the matrix (i).

## Prepare and return a list of functions for managing the inverse of the provided matrix.
makeCacheMatrix <- function(x = matrix()) {
    ## First we set the initial state of the inverse, i.e., non-existent.
    i <- NULL
    ## Next we define the four management functions:
    # This resets the matrix (x) and inverse (i) in the parent environment
    set <- function(y) {
        x <<- y ## Update the matrix
        i <<- NULL ## Make sure the inverse is cleared-out
    }
    ## This retrieves the matrix
    get <- function() x
    ## This sets the inverse (i) in the parent environment to the argument
    setInverse <- function(inverse) i <<- inverse
    ## This retrieves the inverse
    getInverse <- function() i
    ## Build a list of all four functions for return to our invoker
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve uses the list of functions defined in makeCacheMatrix to return
## the the inverse if it already exists, otherwise it calculates it,
## sets it (in the parent environment), and returns it.

cacheSolve <- function(x, ...) {
    ## Return the matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
            message("Using 'cached' inverse")
            return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix,...)
    x$setInverse(inverse)
    inverse
}
