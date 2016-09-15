## This function calculates, caches and returns the inverse of a mtrix.

## The first function makes a list containing 4 functions:set,get,setsolve,getsolve. 
## "set" and "get" will set and get the value of the matrix respectively
##  "setsolve" and "getsolve" are for preserving the computed value

makeCacheMatrix<-function(x=matrix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function would return the inverse of indicated matrix. 
## If the inverse matrix is checked to be computed, it gets the inverse from cache.
## Otherwise it would compute the inverse by calling get function and then solve the matrix.

cacheSolve<-function(x,...){
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
