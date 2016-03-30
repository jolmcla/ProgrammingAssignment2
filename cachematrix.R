# makeCacheMatrix creates a list with a function to
# set the value of the matrix and get the value of the matrix
#and get the value of inverse of the matrix
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

}

#The following function returns the inverse of the matrix, first of all verifies if the inverse was 
#calculated previously an in case of that gets the result and does not compute it.
#If not, it calculates the inverse and cache it. It also suppose the matriz has some inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("accessing cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
