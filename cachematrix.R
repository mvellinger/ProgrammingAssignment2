## makeCacheMatrix stores a list of 4 functions,
## these functions can be called by cacheSolve depending on
## the scenario.
## -get retrieves the matrix set by function x
##
## -set can be used to replace our input matrix x with it's own input y
## and then NULL any previously stored inverted matrix cached in m
##
## -setInv stores the inverse of the matrix input of x
##  (via the solve function) in m
##
## -getInv retrieves the inverse matrix from m


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL  #ensure m is clear in our workspace

    set <- function(y) {
            #replace input from x with input y
            x <<- y
            #NULL any cached inverse matrix
            m <<- NULL
    }
    #retrieves the original matrix input x
    get <- function() x

    #caches the inverse matrix of x in m
    setInv <- function(solve) m <<- solve

    #retrieves the cached matrix from m
    getInv <- function() m

    #create a list of our functions so cacheSolve can call them with $ notation
    #e.g. listname$function()
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
