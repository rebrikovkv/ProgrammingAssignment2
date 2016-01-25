## Put comments here that give an overall description of what your
## functions do

## Usage sample:
#pseudoMatrix <- makeCacheMatrix(matrix(rnorm(9),3,3))
#inverse1 <- cacheSolve(pseudoMatrix)
#inverse2 <- cacheSolve(pseudoMatrix)
#pseudoMatrix$get() %*% inverse2

## Write a short comment describing this function
# This function creates an emulation of matrix that can keep its appropriate reverse matrix in its cache 
# This cache is stored in parent (global in this case) environment
# Access to cached contents is made via "super assignment" operator "<<-"


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) inv <<- inverseMatrix
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# This function either calculates inverse matrix to pseudo-matrix x, or retreives cached contents of inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached value of inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
