
## Put comments here that give an overall description of what your
## functions do

## This function produces the cacheSolve-ready object and inits the NULL
makeCacheMatrix <- function(x = matrix()) {
        xp <- NULL
        set <- function(y) {
                x <<- y
                xp <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xp <<- inv
        getinv <- function() xp
        list(set = set, get = get,
             setinv= setinv,
             getinv = getinv)
}


## whereas this bad boy runs that method on the object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xp <- x$getinv()
        if(!is.null(xp)) {
                message("getting cached data")
                return(xp)
        }
        data <- x$get()
        xp <- solve(data, ...)
        x$setinv(xp)
        xp
}
