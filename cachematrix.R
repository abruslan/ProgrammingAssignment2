## Put comments here that give an overall description of what your
## functions do

## Functions are designed for educational pattern
##
## Test:
##   mx <- matrix(c(1,-0.25,-0.25,1), nrow = 2, ncol = 2)
##   mx1 <- makeCacheMatrix(mx)
##   cacheSolve(mx1)
##   cacheSolve(mx1)
##   mx %*% cacheSolve(mx1)


## Function "Caching the Inverse of a Matrix"
##    Return list with setters and getters
##    $set()      - set the value of the matrix
##    $get()      - get the value of the matrix
##    $setsolve() - set the value of the inverse matrix
##    $getsolve() - get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        ## cached value
        m <- NULL
        set <- function(y) {
                x <<- y
                ## clear cached value
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Function "Get the Inverse of a Matrix"
cacheSolve <- function(x, ...) {
        ## Get cached value
        m <- x$getsolve()

        ## Check of null
        if(!is.null(m)) {
                message("getting cached data")
                ## Return Cached Value
                return(m)
        }

        ## Compute the Inverse of Matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)

        ## Return computed matrix
        m
}


