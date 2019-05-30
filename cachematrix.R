## Programming Assignment 2
## Caching the Inverse of a Matrix
# This program consists of two function.
# The first function creates a special "matrix" object that can cache it's inverse
# The second function computes the inverse of the matrix or retrieves an already calculated inverse

## The first function is called "makeCacheMatrix". Input of matrix x.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second functon is called "cacheSolve". Input of matrix x.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        #If it retrieves already calculated data, it's prints this message.
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    message("no cached data")
    m
}



#Test below, commented for submission
#y <- cbind(c(1,5,3,2),c(4,5,7,20),c(7,8,8,8),c(1,2,3,4))
#y2 <- makeCacheMatrix(y)
#y3 <- cacheSolve(y2)
#y3