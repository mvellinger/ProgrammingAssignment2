## makeCacheMatrix stores a list of 4 functions,
## these functions can be called by cacheSolve depending on
## the scenario.
## -get retrieves the matrix set by function x
##
## -set updates/replaces the matrix set by function x, and then NULLs the
##  previously stored inverted matrix stored in m
##
## -setInv stores the inverse of the matrix input of x
##  (via the solve function) in m
##
## -getInv retrieves the inverse matrix from m


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## cacheSolve takes as input the object where we stored makeCacheMatrix
## for example:
##
## data <- makeCacheMatrix(matrix(c(-2,4,6,8), nrow=2))
## cacheSolve(data)
##
## cacheSolve first verifies if m exists and is not NULL
##
## if m exists and is not NULL, a message is displayed and m is returned.
##
## if m IS NULL, cacheSolve calls the x$get() function to retrieve the input
## matrix from makeCacheMatrix, applies the solve() function to it and stores
## the result in m. It then calls x$setInv to cache the result before
## returning m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
