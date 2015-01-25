## The following functions can cache the inverse of a matrix so that it does not have to be recalculated everytime.

#The first function sets and gets the value of a matrix which is then used in setting and getting the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The second function checks to see if the inverse of the matrix has already been calculated and gets it and if not, then it uses the setinv function to compute it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    dat <- x$get()
    inv <- solve(dat)
    x$setinv(inv)
    inv
}