# makeCacheMatrix takes a matrix, saved in the private variable x
# return value of the makeCacheMatrix function is a list
# of functions that we want to expose as public.  
# these are accessed with the $ operator.  Any variables
# declared inside makeVector but not exported as part of this list
# are private...they are inaccessible to any caller of this function
makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set=set, get=get, setInv = setInv, getInv = getInv)
}


# cacheSolve takes a caching matrix created with makeCacheMatrix and
# return the caching vector

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
## Abhi was here