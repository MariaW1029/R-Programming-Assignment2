##This project is to cache the inverse of a matrix
##makeCacheMatrix consists of set, get, setinverse, and getinverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##This part is to get the cache data
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    matinv <- x$get()
    inv <- solve(matinv)
    x$setinverse(inv)
    inv
}
