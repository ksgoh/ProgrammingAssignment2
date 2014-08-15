## The following 2 functions are used to cache potentially time-consuming computations
## to inverse matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to
## 1.  set the matrix 
## 2.  get the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(matrix = matrix()) {
        inverse <- NULL
        set <- function(m) {
                matrix  <<- m
                inverse <<- NULL
        }
        get <- function() matrix
        setinverse <- function(i) inverse <<- i
        getinverse <- function()  inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special matrix - 'x', returned by makeCacheMatrix(). 
## If the inverse has already been calculated (and the matrix has not changed), 
##		then the cacheSolve retrieves and returns the inverse from the cache.
## else it computes the inverse, stores it in the cache and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix 'i' that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)) {
              message("using cached inverse matrix")
        } else {
              m <- x$get()
              i <- solve(m, ...)
              x$setinverse(i)
        }
        return (i)
}