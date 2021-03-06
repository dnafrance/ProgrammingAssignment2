# Matrix inversion is usually a costly computation 
#       and there may be some benefit to caching the inverse
#       of a matrix rather than computing it repeatedly
#       The following pair of functions cache the inverse of a matrix.


# This function `makeCacheMatrix` creates a special "matrix" object
#       that can cache its inverse.
#       1. sets the value of the matrix
#       2. gets the value of the matrix
#       3. sets the value of the inverse of the matrix
#       4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        setmatrix <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        getmatrix <- function() x
        setinvmat <- function(inverse) invmat <<- inverse
        getinvmat <- function() invmat
        
        # Create list to allow selecting elements using $ operator 
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinvmat = setinvmat, getinvmat = getinvmat)
}


# This function computes the inverse of the special
#       "matrix" returned by `makeCacheMatrix` above. If the inverse has
#       already been calculated (and the matrix has not changed), then
#       `cacheSolve` retrieves the inverse from the cache.
# The assumption for this function is that the matrix 
#       supplied is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinvmat()
        
        # Check if inverse is already calculated and available
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        
        # Else calculate inverse using solve() function
        data <- x$getmatrix()
        invmat <- solve(data, ...)
        x$setinv(invmat)
        invmat
}

# Sample use of the functions
#       x <- matrix(1:4, nrow=2, ncol=2)
#       c <- makeCacheMatrix(x)
#       cacheSolve(c) # calculates and returns inverse matrix of x
#       cacheSolve(c) # prints "getting cached data" and 
#               returns cached inverse matrix