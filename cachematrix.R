## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix saves a matrix in memory
## cacheSolve calculates the inverse of a matrix or gets it from cache, if available

## Write a short comment describing this function
## Makes and saves a matrix in memory

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
            set <- function(y)  {
              x <<- y
              inverse <<- NULL
            }
            get <- function () x
            setinverse <- function(Inverse) inverse <<- Inverse
            getinverse <- function() inverse
            list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## Calcualtes the inverse of a matrix if not available in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached matrix")
        return(inverse)
    }
    message("inverse not in memory and will be calculated") 
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
