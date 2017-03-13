# Programming Assignment 2
# makeCacheMatrix: Function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x = matrix(), ...) {

    m <- x$getinverse()
    if(!is.null(m)){
        message("Retrieving Cached Data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinverse(m)
    m
}
