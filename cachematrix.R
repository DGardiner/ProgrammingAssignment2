## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 
## makeCacheMatrix takes a matrix as a parameter and 
## returns a list that contains four functions:
##    set    : sets the matrix to the provided parameter
##    get    : returns the matrix
##    setinv : sets the inverse of the matrix
##    getinv : returns the inverse of the matrix
## 
## Assumptions: the provided parameter is a square,
##              invertible matrix
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## 
## cacheSolve takes a list created by makeCacheMatrix
## as a parameter, determines the inverse of the matrix,
## calls setinv from the list and returns the inverse
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
