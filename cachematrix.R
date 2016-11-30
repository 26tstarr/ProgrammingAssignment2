## Put comments here that give an overall description of what your
## functions do

##Make a special 'matrix' which can be inversed

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## check if the inversed version is already available in cache, if so, use it, otherwise calculate and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}
